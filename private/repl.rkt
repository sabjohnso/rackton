#lang racket/base

;; Rackton — interactive REPL kernel.
;;
;; Exposes a small state-machine interface so the loop can be driven
;; by tests as easily as by a live stdin.  Persistent state lives in
;; a `rackton-repl-state` value; each input form is processed by
;; `rackton-repl-step` which returns the next state plus a formatted
;; output string.
;;
;; The kernel reuses the same inference + codegen pipeline that the
;; `(rackton ...)` macro uses, but slices it one form at a time and
;; carries the full set of inference parameters between calls so a
;; later definition can see what an earlier one declared.

(provide rackton-repl-init
         rackton-repl-step
         rackton-repl-state?
         rackton-repl-run
         (rename-out [rackton-repl-state-quit? rackton-repl-quit?]))

(require racket/match
         racket/format
         (only-in racket/port with-output-to-string)
         "surface.rkt"
         "infer.rkt"
         "codegen.rkt"
         "prelude.rkt"
         "env.rkt"
         "types.rkt")

;; ----- session state ----------------------------------------------

;; Wraps every piece of mutable infrastructure that the inference
;; pipeline expects to find through parameters.  Boxes hold the
;; per-program fresh-counter and the pending-pred bag; hashes hold
;; the resolution tables that codegen consumes.  `nsp` is the live
;; Racket namespace that executes compiled code.
(struct rackton-repl-state
  (env declared
       fresh-box preds-box
       method-uses method-resolutions method-dict-resolutions
       needs-dict-defs
       nsp
       expr-counter
       quit?)
  #:transparent)

(define (rackton-repl-init)
  (define ns (make-base-namespace))
  (parameterize ([current-namespace ns])
    (namespace-require 'racket/base)
    (namespace-require 'rackton))
  (rackton-repl-state prelude-env
                      (hasheq)
                      (box 0)
                      (box '())
                      (make-hasheq)
                      (make-hasheq)
                      (make-hasheq)
                      (make-hash)
                      ns
                      0
                      #f))

;; ----- dispatch ----------------------------------------------------

(define (rackton-repl-step state input)
  (cond
    [(repl-command? input) (handle-command state input)]
    [(top-form? input)     (handle-top-input state input)]
    [else                  (handle-expr-input state input)]))

(define (repl-command? form)
  (and (pair? form)
       (memq (car form) '(:type :t :info :i :quit :q :help :h))))

(define (top-form? form)
  (and (pair? form)
       (memq (car form)
             '(define define-data define-newtype define-struct
                define-class define-instance define-alias
                : require))))

;; ----- command handling -------------------------------------------

(define (handle-command state input)
  (match input
    [`(:quit)  (values (struct-copy rackton-repl-state state [quit? #t]) "")]
    [`(:q)     (values (struct-copy rackton-repl-state state [quit? #t]) "")]
    [`(:help)  (values state (help-text))]
    [`(:h)     (values state (help-text))]
    [`(:type ,expr) (values state (show-type state expr))]
    [`(:t    ,expr) (values state (show-type state expr))]
    [`(:info ,name) (values state (show-info state name))]
    [`(:i    ,name) (values state (show-info state name))]
    [_ (values state
               (format "unknown command: ~s\n" input))]))

(define (help-text)
  (string-append
   ":type EXPR   show inferred type of EXPR\n"
   ":info NAME   show what's bound to NAME\n"
   ":quit        exit the REPL\n"
   ":help        this message\n"))

(define (show-type state expr-datum)
  (with-handlers
   ([exn:fail?
     (lambda (e) (format "error: ~a\n" (exn-message e)))])
   (define name (gensym '$repl-type-))
   (define synthetic (datum->syntax #f `(define ,name ,expr-datum)))
   (define-values (env* _compiled) (elaborate-form state synthetic))
   (define sch (env-ref-var env* name))
   (format "~s :: ~a\n" expr-datum (scheme->datum sch))))

(define (show-info state name)
  (define env (rackton-repl-state-env state))
  (cond
    [(env-ref-var env name)
     => (lambda (sch) (format "~s :: ~a\n" name (scheme->datum sch)))]
    [(env-ref-data env name)
     => (lambda (di) (format "~s :: ~a (data ctor)\n"
                             name (scheme->datum (data-info-scheme di))))]
    [(env-ref-tcon env name)
     => (lambda (_) (format "~s (type ctor)\n" name))]
    [(env-ref-class env name)
     => (lambda (_) (format "~s (class)\n" name))]
    [else (format "~s is unbound\n" name)]))

;; ----- top-form input ---------------------------------------------

(define (handle-top-input state input)
  (with-handlers
   ([exn:fail?
     (lambda (e) (values state (format "error: ~a\n" (exn-message e))))])
   (define stx (datum->syntax #f input))
   (define-values (env* compiled) (elaborate-form state stx))
   (define state*
     (struct-copy rackton-repl-state state [env env*]))
   (for ([c (in-list compiled)])
     (eval-in state* c))
   (values state*
           (format-top-result input
                              (rackton-repl-state-env state)
                              env*))))

(define (format-top-result input pre-env post-env)
  (match input
    [`(define ,name . ,_)
     (define sch (env-ref-var post-env name))
     (cond
       [sch (format "~s :: ~a\n" name (scheme->datum sch))]
       [else ""])]
    [_ ""]))

;; ----- expression input -------------------------------------------

(define (handle-expr-input state input)
  (with-handlers
   ([exn:fail?
     (lambda (e) (values state (format "error: ~a\n" (exn-message e))))])
   (define n (rackton-repl-state-expr-counter state))
   (define name (string->symbol (format "$repl-~a" n)))
   (define synthetic (datum->syntax #f `(define ,name ,input)))
   (define-values (env* compiled) (elaborate-form state synthetic))
   (define state*
     (struct-copy rackton-repl-state state
                  [env env*]
                  [expr-counter (add1 n)]))
   (for ([c (in-list compiled)])
     (eval-in state* c))
   (define value (eval-in state* (datum->syntax #f name)))
   (define sch (env-ref-var env* name))
   (values state*
           (format "~a :: ~a\n"
                   (format-value value)
                   (scheme->datum sch)))))

;; ----- elaboration + eval -----------------------------------------

;; Parse + type-check + compile one top-form syntax under the
;; persisted parameters.  Returns updated env and the list of
;; compiled Racket-syntax forms (a single parsed entry may expand
;; to multiple, e.g. #:deriving).
(define (elaborate-form state stx)
  (parameterize ([current-fresh-state
                   (rackton-repl-state-fresh-box state)]
                 [current-pending-preds
                   (rackton-repl-state-preds-box state)]
                 [current-method-uses
                   (rackton-repl-state-method-uses state)]
                 [current-method-resolutions
                   (rackton-repl-state-method-resolutions state)]
                 [current-method-dict-resolutions
                   (rackton-repl-state-method-dict-resolutions state)]
                 [current-needs-dict-defs
                   (rackton-repl-state-needs-dict-defs state)])
    (define parsed (parse-toplevel-list (list stx)))
    (define-values (env* _declared*)
      (for/fold ([env (rackton-repl-state-env state)]
                 [declared (rackton-repl-state-declared state)])
                ([p (in-list parsed)])
        (handle-top-form-step p env declared)))
    (define compiled
      (filter values
              (for/list ([p (in-list parsed)])
                (compile-top p env*))))
    (values env* compiled)))

;; Mirror of infer.rkt's `handle-top-form` invocation; the helper
;; isn't exported so we re-derive it by reusing infer-program over
;; a single form's parsed list.  Returns (values env* declared*).
(define (handle-top-form-step parsed env declared)
  ;; infer-program's loop is private; re-implement the single-step
  ;; variant by running infer-program over a list with one parsed
  ;; entry and the current env as starting point.  But infer-program
  ;; wraps in `with-fresh` which would reset our boxes — so instead
  ;; call its private worker.  We expose one helper from infer.rkt
  ;; (`infer-program-step`) for exactly this.
  (infer-program-step parsed env declared))

(define (eval-in state stx)
  (parameterize ([current-namespace (rackton-repl-state-nsp state)])
    (eval stx)))

;; ----- interactive loop -------------------------------------------

;; Drive the kernel from `current-input-port` / `current-output-port`.
;; EOF or `:quit` ends the loop.  Exposed as a single entry that
;; user-facing shims can call (e.g. via `racket -l rackton/repl`).
(define (rackton-repl-run)
  (display "rackton REPL — :help for commands, :quit to exit\n")
  (let loop ([state (rackton-repl-init)])
    (display "λ> ")
    (flush-output)
    (define form (read))
    (cond
      [(eof-object? form)
       (newline)
       (void)]
      [else
       (define-values (state* output) (rackton-repl-step state form))
       (display output)
       (cond
         [(rackton-repl-state-quit? state*) (void)]
         [else (loop state*)])])))

;; ----- value rendering --------------------------------------------

;; Top-level catch-all: print everything via ~v for now.  Rackton
;; data ctors are Racket structs whose printers already render them
;; readably.
(define (format-value v) (~v v))

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
         rackton-read-form
         rackton-parse-command-line
         rackton-repl-completions
         (rename-out [rackton-repl-state-quit? rackton-repl-quit?]))

(require racket/match
         racket/format
         racket/list
         racket/string
         (only-in racket/port with-output-to-string)
         "surface.rkt"
         "infer.rkt"
         "codegen.rkt"
         "codegen-plan.rkt"
         "prelude.rkt"
         "env.rkt"
         "types.rkt"
         "term.rkt")

;; ----- session state ----------------------------------------------

;; Wraps every piece of mutable infrastructure that the inference
;; pipeline expects to find through parameters.  The fresh counter and
;; pending-pred bag now live in the immutable `infer-state` that
;; infer-program/phases threads internally; hashes hold the resolution
;; tables that codegen consumes.  `nsp` is the live Racket namespace
;; that executes compiled code.
;; `macros` is the list of symbols this session has bound as user macros
;; (via `define-syntax` / `define-syntax-rule` / `define-syntaxes`).  The
;; transformer bindings themselves live in `nsp`; this list records which
;; heads the expansion walk should expand and turns the parser's hygiene
;; on once the session defines any macro.
(struct rackton-repl-state
  (env declared
       infer-st
       nsp
       expr-counter
       macros
       quit?)
  #:transparent)

(define (rackton-repl-init)
  (define ns (make-base-namespace))
  (parameterize ([current-namespace ns])
    (namespace-require 'racket/base)
    (namespace-require 'rackton))
  (rackton-repl-state prelude-env
                      (hasheq)
                      (make-infer-state)
                      ns
                      0
                      '()
                      #f))

;; ----- dispatch ----------------------------------------------------

(define (rackton-repl-step state input)
  (cond
    [(repl-command? input)   (handle-command state input)]
    [(macro-def-form? input) (handle-macro-def state input)]
    [else
     ;; Expand any session-macro uses first, then dispatch on the result:
     ;; a macro may expand to a top form (e.g. a `define`) or to an
     ;; expression, so the top-form/expression split must see the
     ;; expansion, not the raw input.  The expanded form is carried as
     ;; syntax so macro hygiene (scope-tagged binders) survives into the
     ;; parser; only its head datum is consulted to pick the handler.
     (define exp-stx (expand-session-macros state input))
     (cond
       [(top-form? (syntax->datum exp-stx)) (handle-top-input state exp-stx)]
       [else                                (handle-expr-input state exp-stx)])]))

;; A command is a leading-comma line, which reads as `(unquote word arg ...)`.
;; An `unquote` outside a quasiquote is never a valid Rackton form, so a
;; comma-prefixed line can never be mistaken for ordinary input.
(define (repl-command? form)
  (and (pair? form) (eq? (car form) 'unquote)))

(define (top-form? form)
  (and (pair? form)
       (memq (car form)
             '(define data newtype struct
                protocol instance define-alias
                : require))))

;; A macro-definition form binds a Racket transformer rather than a
;; Rackton value, so it is handled outside the parse/infer/codegen
;; pipeline: evaluated straight into the session namespace.
(define (macro-def-form? form)
  (and (pair? form)
       (memq (car form)
             '(define-syntax define-syntax-rule define-syntaxes))))

;; The macro name(s) a macro-definition form introduces.
(define (macro-def-names form)
  (match form
    [(list 'define-syntax (cons name _) _ ...)      (list name)] ; (define-syntax (m . args) body)
    [(list 'define-syntax (? symbol? name) _ ...)   (list name)] ; (define-syntax m expr)
    [(list 'define-syntax-rule (cons name _) _ ...) (list name)] ; (define-syntax-rule (m . pat) tmpl)
    [(list 'define-syntaxes (list names ...) _ ...) names]       ; (define-syntaxes (m ...) expr)
    [_ '()]))

;; ----- command handling -------------------------------------------

(define (handle-command state input)
  ;; `unquote` is special inside `match`'s quasiquote, so spell the command
  ;; shapes out with explicit `(list 'unquote …)` patterns.
  (match input
    [(list 'unquote)              (values state "")]         ; bare `,` — no-op
    [(list 'unquote 'geiser-no-values) (values state "")]    ; Geiser probe — no-op
    [(list 'unquote 'quit)        (quit state)]
    [(list 'unquote 'q)           (quit state)]
    [(list 'unquote 'clear)       (values (rackton-repl-init) "session cleared\n")]
    [(list 'unquote 'c)           (values (rackton-repl-init) "session cleared\n")]
    [(list 'unquote 'help)        (values state (help-text))]
    [(list 'unquote 'h)           (values state (help-text))]
    [(list 'unquote 'type expr)   (values state (show-type state expr))]
    [(list 'unquote 't    expr)   (values state (show-type state expr))]
    [(list 'unquote 'info name)   (values state (show-info state name))]
    [(list 'unquote 'i    name)   (values state (show-info state name))]
    [_ (values state
               (format "unknown command: ~a\n" (command->string input)))]))

(define (quit state)
  (values (struct-copy rackton-repl-state state [quit? #t]) ""))

;; Render a command datum back to the comma syntax the user typed, for
;; the "unknown command" message: `(unquote foo bar)` -> ",foo bar".
(define (command->string cmd)
  (string-append "," (string-join (map ~a (cdr cmd)) " ")))

(define (help-text)
  (string-append
   ",type EXPR   show inferred type of EXPR\n"
   ",info NAME   show what's bound to NAME\n"
   ",clear       reset the session to a fresh prelude env\n"
   ",quit        exit the REPL\n"
   ",help        this message\n"))

(define (show-type state expr-datum)
  (with-handlers
   ([exn:fail?
     (lambda (e) (format "error: ~a\n" (exn-message e)))])
   (define name (gensym '$repl-type-))
   ;; Expand session macros in the expression so `,type (a-macro …)`
   ;; types the expansion; splice the result as syntax to keep its scopes.
   (define exp-stx (expand-session-macros state expr-datum))
   (define synthetic (datum->syntax #f (list 'define name exp-stx)))
   (define-values (env* _compiled _final-st) (elaborate-form state synthetic))
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

;; ----- macro-definition input -------------------------------------

;; Evaluate a `define-syntax` / `define-syntax-rule` / `define-syntaxes`
;; straight into the session namespace, binding a real hygienic Racket
;; transformer there.  Record its name(s) so later inputs expand uses of
;; it (see `expand-session-macros`) and so the parser turns hygiene on.
;; The namespace retains the binding across inputs, so a macro defined on
;; one line is usable on every later line with no re-feeding.
(define (handle-macro-def state input)
  (with-handlers
   ([exn:fail?
     (lambda (e) (values state (format "error: ~a\n" (exn-message e))))])
   (eval-in state (datum->syntax #f input))
   (define state*
     (struct-copy rackton-repl-state state
                  [macros (append (macro-def-names input)
                                  (rackton-repl-state-macros state))]))
   (values state* "")))

;; ----- session-macro expansion ------------------------------------

;; Expand every session-macro use in `input`, returning the result as a
;; syntax object so macro-introduced binders keep the scopes the parser's
;; hygiene relies on.  When the session has defined no macros this just
;; returns `(datum->repl-syntax input)`, so non-macro sessions are
;; unaffected.
(define (expand-session-macros state input)
  (define names (rackton-repl-state-macros state))
  (define stx (datum->repl-syntax input))
  (cond
    [(null? names) stx]
    [else
     (parameterize ([current-namespace (rackton-repl-state-nsp state)])
       (expand-macro-walk stx names))]))

;; Convert REPL input to syntax.  A `require` form is given a notional
;; source in the working directory so a relative library path resolves
;; against the cwd — its spec syntax otherwise carries no source, and
;; `require-spec->submod-spec` then can't locate the library's sidecar
;; (so neither its types nor its macros would import).  Every other form
;; keeps a #f source, so type errors are never prefixed with a fake file.
(define (datum->repl-syntax input)
  (cond
    [(and (pair? input) (eq? (car input) 'require))
     (datum->syntax #f input
                    (list (build-path (current-directory) "repl-input")
                          #f #f #f #f))]
    [else (datum->syntax #f input)]))

;; One structural pass over `stx`: while the head names a session macro,
;; take a single expansion step with `expand-once` (which fires the
;; transformer without lowering the result into Racket core syntax the
;; way a full `expand` would), then recurse into every sub-form.  The
;; `names` guard is essential — `expand-once` on a plain application like
;; `(+ 1 2)` would lower it to `(#%app + 1 2)`, which `parse-top` cannot
;; read; guarding on session-macro heads keeps ordinary forms untouched.
(define (expand-macro-walk stx names)
  (define l (syntax->list stx))
  (cond
    [(or (not l) (null? l)) stx]
    [(head-macro? (car l) names)
     (expand-macro-walk (expand-once stx) names)]
    [else
     (datum->syntax stx
                    (map (lambda (s) (expand-macro-walk s names)) l)
                    stx stx)]))

(define (head-macro? head-stx names)
  (and (identifier? head-stx)
       (and (memq (syntax-e head-stx) names) #t)))

;; ----- top-form input ---------------------------------------------

;; `stx` is the (already macro-expanded) input as a syntax object.
(define (handle-top-input state stx)
  (with-handlers
   ([exn:fail?
     (lambda (e) (values state (format "error: ~a\n" (exn-message e))))])
   (define-values (env* compiled final-st) (elaborate-form state stx))
   (define base
     (struct-copy rackton-repl-state state [env env*] [infer-st final-st]))
   ;; Run the compiled forms (including a `require`'s runtime import)
   ;; before pulling in any exported macros, so a macro that expands into
   ;; the library's runtime bindings finds them already loaded.
   (for ([c (in-list compiled)])
     (eval-in base c))
   (define datum (syntax->datum stx))
   (define state*
     (if (and (pair? datum) (eq? (car datum) 'require))
         (struct-copy rackton-repl-state base
                      [macros (import-session-macros base stx)])
         base))
   (values state*
           (format-top-result datum
                              (rackton-repl-state-env state)
                              env*))))

;; Bind any macros a required Rackton library exports into the session.
;; Each library's `rackton-schemes` sidecar carries `rackton-macros` — a
;; list of (name . definition-datum), one entry per provided macro name.
;; Evaluate each definition into the session namespace (so later uses
;; expand) and record the name.  A
;; spec with no sidecar, or a library that exports no macros, contributes
;; nothing.  References inside a macro's template resolve in the session
;; namespace, so a macro built only from prelude/exported names works; one
;; that reaches a library-private binding will not (the same boundary the
;; module pipeline has for non-exported references).
(define (import-session-macros state require-stx)
  (define specs (cdr (syntax->list require-stx)))   ; (require spec ...)
  (for/fold ([macros (rackton-repl-state-macros state)])
            ([spec-stx (in-list specs)])
    (define submod (require-spec->submod-spec spec-stx))
    (cond
      [(not submod) macros]
      [else
       (define entries
         (with-handlers ([exn:fail? (lambda (_) '())])
           (dynamic-require submod 'rackton-macros)))
       (for/fold ([macros macros]) ([entry (in-list entries)])
         (eval-in state (datum->syntax #f (cdr entry)))
         (cons (car entry) macros))])))

(define (format-top-result input pre-env post-env)
  (match input
    [`(define ,name . ,_)
     (define sch (env-ref-var post-env name))
     (cond
       [sch (format "~s :: ~a\n" name (scheme->datum sch))]
       [else ""])]
    [_ ""]))

;; ----- expression input -------------------------------------------

;; `stx` is the (already macro-expanded) input expression as a syntax
;; object.  It is spliced into the synthetic `define` as syntax so any
;; macro-introduced scopes survive — `datum->syntax` leaves an embedded
;; syntax object intact rather than re-wrapping it.
(define (handle-expr-input state stx)
  (with-handlers
   ([exn:fail?
     (lambda (e) (values state (format "error: ~a\n" (exn-message e))))])
   (define n (rackton-repl-state-expr-counter state))
   (define name (string->symbol (format "$repl-~a" n)))
   (define synthetic (datum->syntax #f (list 'define name stx)))
   (define-values (env* compiled final-st) (elaborate-form state synthetic))
   (define state*
     (struct-copy rackton-repl-state state
                  [env env*]
                  [infer-st final-st]
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
;; Returns (values env* compiled final-st).  The inference state — fresh
;; counter, pending preds, and the resolution tables — is threaded as an
;; immutable infer-state the caller persists into the next repl-state, so the
;; tables accumulate across inputs the way the old mutable hashes did.
(define (elaborate-form state stx)
  ;; A REPL session iterates by re-evaluating forms, so a re-declared instance
  ;; replaces the prior one instead of raising the module coherence error.
  (parameterize ([current-allow-instance-redefinition? #t])
    ;; Turn hygiene on once the session has bound any macro, so a
    ;; macro-introduced local binder is α-renamed apart from a user binder
    ;; of the same symbol (and a macro's reference falls through to the
    ;; prelude).  Off otherwise: non-macro sessions parse exactly as before.
    (define parsed
      (parameterize ([current-hygiene?
                      (not (null? (rackton-repl-state-macros state)))])
        (parse-toplevel-list (list stx))))
    ;; Run the full 4-phase pipeline over the parsed list so that multi-form
    ;; REPL input is order-invariant just like a module body.  Pass the
    ;; persisted st so resolution tables accumulate; get the final st back.
    ;; infer-program/phases also returns the post-expansion form list
    ;; (`#:derive-superclasses` instances replaced by the plain instances they
    ;; synthesize); compile THAT so derived instances are lowered.
    (define-values (env* _declared* parsed* final-st)
      (infer-program/phases parsed
                            (rackton-repl-state-env state)
                            (rackton-repl-state-declared state)
                            (rackton-repl-state-infer-st state)))
    ;; Hand codegen the resolution tables inference just wrote into final-st.
    ;; return-typed-methods is left #f: a REPL session is incremental, so
    ;; return-typed calls dispatch through the runtime table rather than a
    ;; monomorphized per-input impl name (which a later input might not bind).
    (define plan
      (codegen-plan (st-table final-st 'method-resolutions)
                    (st-table final-st 'method-dict-resolutions)
                    (st-table final-st 'needs-dict-defs)
                    (st-table final-st 'instance-default-bodies)
                    #f))
    ;; Thread a fresh codegen state across this input's forms; the REPL evals
    ;; the compiled forms directly, so its inline/export logs aren't needed.
    (define-values (compiled _cgst)
      (for/fold ([acc '()] [cgst (make-cg-st)] #:result (values (reverse acc) cgst))
                ([p (in-list parsed*)])
        (let-values ([(s cgst*) (compile-top p env* plan cgst)])
          (values (if s (cons s acc) acc) cgst*))))
    (values env* compiled final-st)))

(define (eval-in state stx)
  (parameterize ([current-namespace (rackton-repl-state-nsp state)])
    (eval stx)))

;; ----- multi-line input ------------------------------------------

;; Read one rackton form from `port`, accumulating lines
;; until parens balance.  `prompt-cont` is called with the current
;; depth count (always positive on continuation prompts) and
;; should return a continuation-prompt string to display; in tests
;; pass `(lambda (_) "")` to silence.  Returns the parsed form or
;; eof when the port is exhausted with nothing read.

;; Parse one line of input into a command datum, or #f when it is not a
;; command.  A leading `,` (which never begins a valid Rackton form) marks
;; a command: the rest of the line is read as the command word and its
;; arguments, producing `(unquote word arg ...)`.  A bare `,` yields
;; `(unquote)` — the accepted no-op.  Each `read` is guarded so a
;; malformed tail simply stops accumulation instead of raising.
(define (rackton-parse-command-line str)
  (define s (string-trim str))
  (and (positive? (string-length s))
       (char=? (string-ref s 0) #\,)
       (let ([ip (open-input-string (substring s 1))])
         (let loop ([acc '()])
           (define d
             (with-handlers ([exn:fail:read? (lambda (_) eof)])
               (read ip)))
           (if (eof-object? d)
               (cons 'unquote (reverse acc))
               (loop (cons d acc)))))))

(define (rackton-read-form port [prompt-cont (lambda (_) "..> ")])
  (let loop ([buf ""] [depth 0])
    (define line (read-line port))
    (cond
      [(eof-object? line)
       (cond
         [(zero? (string-length buf)) eof]
         [(rackton-parse-command-line buf) => values]
         [else
          ;; Buffer non-empty but parens never closed — let read
          ;; raise the natural "expected )" error so the caller
          ;; sees a useful message.
          (read (open-input-string buf))])]
      [else
       (define buf* (string-append buf line " "))
       (define new-depth (+ depth (line-paren-delta line)))
       (cond
         [(<= new-depth 0)
          ;; Parens balanced (or never opened).  A leading comma marks a
          ;; REPL command (handled first); otherwise try to read, and if
          ;; the buffer is purely whitespace, keep looping.
          (cond
            [(rackton-parse-command-line buf*) => values]
            [else
             (define trimmed-port (open-input-string buf*))
             (define form
               (with-handlers ([exn:fail:read? (lambda (_) #f)])
                 (read trimmed-port)))
             (cond
               [(eof-object? form) (loop "" 0)]    ;; blank-ish line
               [form form]
               [else (loop buf* new-depth)])])]
         [else
          (display (prompt-cont new-depth))
          (flush-output)
          (loop buf* new-depth)])])))

;; Net `(` - `)` for one line, ignoring `;` comments and
;; string contents.  Brackets `[]` and braces `{}` count too —
;; Racket treats them as parens.
(define (line-paren-delta line)
  (define n (string-length line))
  (let loop ([i 0] [delta 0] [in-string? #f] [in-comment? #f])
    (cond
      [(= i n) delta]
      [in-comment? delta]
      [in-string?
       (define c (string-ref line i))
       (cond
         [(char=? c #\") (loop (add1 i) delta #f #f)]
         [(char=? c #\\) (loop (+ i 2) delta #t #f)]
         [else (loop (add1 i) delta #t #f)])]
      [else
       (define c (string-ref line i))
       (cond
         [(char=? c #\;) delta]
         [(char=? c #\") (loop (add1 i) delta #t #f)]
         [(or (char=? c #\() (char=? c #\[) (char=? c #\{))
          (loop (add1 i) (add1 delta) #f #f)]
         [(or (char=? c #\)) (char=? c #\]) (char=? c #\}))
          (loop (add1 i) (sub1 delta) #f #f)]
         [else (loop (add1 i) delta #f #f)])])))

;; Completion candidates from the session env.  Returns
;; a list of strings whose names start with `prefix`.  Consults
;; the four user-extensible namespaces — vars, data ctors,
;; classes, tcons — so a partial type or class name also
;; completes.
(define (rackton-repl-completions state prefix)
  (define env (rackton-repl-state-env state))
  (define all-names
    (append (hash-keys (env-vars env))
            (hash-keys (env-data-ctors env))
            (hash-keys (env-classes env))
            (hash-keys (env-tcons env))))
  (define candidates
    (for/list ([n (in-list all-names)]
               #:when (let ([s (symbol->string n)])
                        (and (>= (string-length s) (string-length prefix))
                             (string=? prefix
                                       (substring s 0 (string-length prefix))))))
      (symbol->string n)))
  (sort (remove-duplicates candidates) string<?))

;; ----- interactive loop -------------------------------------------

;; Drive the kernel from `current-input-port` / `current-output-port`.
;; EOF or `,quit` ends the loop.  Exposed as a single entry that
;; user-facing shims can call (e.g. via `racket -l rackton/repl`).
;; Uses readline for history + line editing when stdin
;; is interactive, plus multi-line input accumulation via
;; `rackton-read-form`.  Tab completion consults the live session
;; env for variable / type / class names.
(define (rackton-repl-run)
  (display "rackton REPL — ,help for commands, ,quit to exit\n")
  (define current-state (box (rackton-repl-init)))
  ;; Set up readline tab completion: callbacks consult the
  ;; current-state box's snapshot of the env.
  (with-handlers ([exn:fail? (lambda (_) (void))])
    (dynamic-require 'readline/readline 'set-completion-function!)
    (define f (dynamic-require 'readline/readline 'set-completion-function!))
    (f (lambda (text)
         (rackton-repl-completions (unbox current-state) text))))
  (let loop ()
    (define state (unbox current-state))
    (define port (current-input-port))
    ;; Track the terminal width so wrapped type errors fit this session's
    ;; window (re-checked each prompt, so a mid-session resize is honored).
    (refresh-type-columns!)
    (display "λ> ") (flush-output)
    (define form
      (rackton-read-form port
                         (lambda (_depth)
                           (display "..> ")
                           (flush-output)
                           "")))
    (cond
      [(eof-object? form) (newline)]
      [else
       (define-values (state* output) (rackton-repl-step state form))
       (display output)
       (set-box! current-state state*)
       (cond
         [(rackton-repl-state-quit? state*) (void)]
         [else (loop)])])))

;; ----- value rendering --------------------------------------------

;; Top-level catch-all: print everything via ~v for now.  Rackton
;; data ctors are Racket structs whose printers already render them
;; readably.
(define (format-value v) (~v v))

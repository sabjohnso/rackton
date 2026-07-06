#lang racket/base

;; scribblings/rackton-eval.rkt — executable Scribble examples for Rackton.
;;
;; Tenet: the documentation is the single source of truth.  An example
;; shown in the Guide or Reference is *run* while the docs are built, so a
;; snippet that no longer compiles (a missing `require`, a renamed binding,
;; a stale type) fails the build instead of silently lying to readers.
;;
;; Public API (programming to an interface — callers never touch the
;; sandbox or the accumulator directly):
;;
;;   (make-rackton-eval [#:requires extra]) -> eval-ctx
;;       One per documentation page.  Holds the page's accumulated
;;       top-level forms for examples that opt into #:context? (separate
;;       `(rackton …)` blocks do NOT share an inference env, so cumulative
;;       reuse demands a single growing program).
;;
;;   @rackton-example[#:eval ev #:mode m #:run r #:context? c]{ <code> }
;;       Renders <code> as a linked code block (identifiers hyperlink to
;;       the Reference via `for-label`) and evaluates it.  Examples are
;;       isolated by default; pass #:context? #t on a page whose prose
;;       develops one program across blocks so an example sees the earlier
;;       definitions and contributes its own.  Modes:
;;         'defs    — type-check; add the forms to the page context.   [default]
;;         'value   — the last form is an expression; show its `show` value.
;;         'io      — run an IO action (the last form, or #:run name) and
;;                    show its captured output.
;;         'module  — auto-selected when <code> begins with `#lang rackton`;
;;                    evaluate the whole module and show its output.
;;         'error   — the code is expected to fail; run it, catch the
;;                    exception, and show its real message.  If it
;;                    compiles/runs without error, the build fails —
;;                    a stale error claim must not sit undetected.
;;         'display — render only, never evaluate.  Reserved for genuine
;;                    syntax fragments (grammar templates using `...` as
;;                    a metavariable) that are not runnable code at all;
;;                    an example that is real code, even one demonstrating
;;                    an error, belongs in 'defs/'value/'io/'error instead.
;;
;; Isolation/side effects: each example evaluates the page program in a
;; fresh sandbox with CPU/memory limits, so a runaway snippet cannot hang
;; the build.  Re-evaluating the accumulated program per example is O(n²)
;; over a page; pages hold a handful of examples, so this stays in the
;; millisecond range.  Should profiling ever show otherwise, the obvious
;; optimization is to drive `private/repl.rkt`'s incremental kernel — but
;; that trades the sandbox's time limits for in-process evaluation, so it
;; needs evidence first.

(require (for-syntax racket/base
                     syntax/parse)
         racket/sandbox
         racket/port
         racket/string
         scribble/manual
         scribble/core
         scribble/base)

(provide make-rackton-eval
         current-rackton-eval
         rackton-example)

;; ---------------------------------------------------------------------------
;; Per-page evaluation context
;; ---------------------------------------------------------------------------

;; forms : (listof datum)         — accumulated top-level forms for the page
;; extra : (listof module-path)   — racket-level requires for the sandbox
(struct eval-ctx (forms extra) #:mutable)

(define current-rackton-eval (make-parameter #f))

(define (make-rackton-eval #:requires [extra '()])
  (eval-ctx '() extra))

(define (page-context ev) (eval-ctx-forms ev))
(define (extend-context! ev new-forms)
  (set-eval-ctx-forms! ev (append (eval-ctx-forms ev) new-forms)))

;; A fresh, limited sandbox that has the Rackton embedding macro available.
;; The example's own surface `(require …)` lines pull in any non-prelude
;; modules, so the sandbox only needs `rackton` itself here.
(define (fresh-sandbox ev)
  (parameterize ([sandbox-output 'string]
                 [sandbox-error-output 'string]
                 [sandbox-eval-limits '(30 512)]
                 [sandbox-memory-limit 512])
    (make-evaluator 'racket/base #:requires (cons 'rackton (eval-ctx-extra ev)))))

;; ---------------------------------------------------------------------------
;; Source string -> datums
;; ---------------------------------------------------------------------------

(define (read-datums src)
  (with-input-from-string src
    (lambda ()
      (let loop ([acc '()])
        (define d (read))
        (if (eof-object? d) (reverse acc) (loop (cons d acc)))))))

(define (module-source? src)
  (regexp-match? #px"^\\s*#lang\\s+rackton\\b" src))

;; Drop a leading `#lang rackton` line so the remainder reads as datums.
(define (strip-lang-line src)
  (regexp-replace #px"^\\s*#lang\\s+rackton[^\n]*\n?" src ""))

(define (split-last xs)
  (when (null? xs)
    (error 'rackton-example "expected at least one form to evaluate"))
  (let loop ([xs xs] [acc '()])
    (if (null? (cdr xs))
        (values (reverse acc) (car xs))
        (loop (cdr xs) (cons (car xs) acc)))))

;; ---------------------------------------------------------------------------
;; Evaluation per mode
;; ---------------------------------------------------------------------------

;; Returns one of: (void) | (list 'value string) | (list 'output string)
;;
;; Examples are isolated by default: each evaluates on its own.  With
;; #:context? #t the example is prepended with the page's accumulated forms
;; and contributes its own definitions back, so a later example may build on
;; an earlier one (use this only on pages whose prose develops one program).
(define (evaluate ev src mode run context?)
  ;; The forms to compile, in order: prior page context (when requested)
  ;; followed by this example's own forms.
  (define (program own) (if context? (append (page-context ev) own) own))
  (define (remember! own) (when context? (extend-context! ev own)))
  (cond
    [(eq? mode 'display) (void)]
    [(module-source? src)
     (define body (read-datums (strip-lang-line src)))
     (define s (fresh-sandbox ev))
     (s `(module rktex rackton ,@body))
     (s '(require 'rktex))
     (list 'output (get-output s))]
    [else
     (define datums (read-datums src))
     (case mode
       [(defs)
        (define s (fresh-sandbox ev))
        (s `(rackton ,@(program datums)))
        (remember! datums)
        (void)]
       [(value)
        (define-values (defs expr) (split-last datums))
        (define s (fresh-sandbox ev))
        (s `(rackton ,@(program defs)
                     (define _rackton-result (show ,expr))))
        (define v (s '_rackton-result))
        (remember! defs)
        (list 'value v)]
       [(io)
        (define run-form (and run (read (open-input-string run))))
        (define-values (defs expr)
          (if run-form (values datums run-form) (split-last datums)))
        (define s (fresh-sandbox ev))
        (s `(rackton ,@(program defs)
                     (define _rackton-result (run-io ,expr))))
        (remember! defs)
        (list 'output (get-output s))]
       [(error)
        (define s (fresh-sandbox ev))
        (define failure
          (with-handlers ([exn:fail? exn-message])
            (s `(rackton ,@(program datums)))
            #f))
        (unless failure
          (error 'rackton-example
                 "example tagged #:mode 'error compiled/ran without error:\n~a"
                 src))
        (list 'error failure)]
       [else (error 'rackton-example "unknown mode: ~s" mode)])]))

;; ---------------------------------------------------------------------------
;; Rendering
;; ---------------------------------------------------------------------------

(define (result->blocks outcome)
  (cond
    [(void? outcome) '()]
    [(eq? (car outcome) 'value)
     (list (nested #:style 'inset
                   (racketresultfont (literal (string-append "⇒ " (cadr outcome))))))]
    [(eq? (car outcome) 'output)
     (define out (string-trim (cadr outcome) "\n" #:left? #f))
     (if (string=? out "")
         '()
         (list (nested #:style 'inset (verbatim out))))]
    [(eq? (car outcome) 'error)
     (list (nested #:style 'inset
                   (verbatim (string-append "✗ " (cadr outcome)))))]
    [else '()]))

;; rendered : the codeblock element; src : its source text.
(define (rackton-example* rendered src #:eval ev #:mode mode #:run run #:context? context?)
  (define use (or ev (current-rackton-eval)
                  (error 'rackton-example "no #:eval given and current-rackton-eval is #f")))
  (define outcome (evaluate use src mode run context?))
  (nested-flow (make-style #f '()) (cons rendered (result->blocks outcome))))

;; ---------------------------------------------------------------------------
;; Surface form
;; ---------------------------------------------------------------------------

(define-syntax (rackton-example stx)
  (syntax-parse stx
    [(_ (~alt (~optional (~seq #:eval ev:expr))
              (~optional (~seq #:mode mode:expr))
              (~optional (~seq #:run run:expr))
              (~optional (~seq #:context? context?:expr))) ...
        body:str ...)
     #'(rackton-example* (codeblock body ...)
                         (string-append body ...)
                         #:eval (~? ev #f)
                         #:mode (~? mode 'defs)
                         #:run  (~? run  #f)
                         #:context? (~? context? #f))]))

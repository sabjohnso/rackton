#lang racket/base

;; REPL for #lang rackton.
;;
;; The REPL kernel exposes a step function `rackton-repl-step` that
;; takes a session state and one input s-expr, and returns a new
;; state plus an output string.  These tests drive the kernel
;; directly so we don't need to wrangle real stdin/stdout.

(require rackunit
         "../private/repl.rkt")

(define (drive-session inputs)
  ;; Returns (values final-state outputs), where outputs is the list
  ;; of per-step output strings.
  (for/fold ([state (rackton-repl-init)] [out '()] #:result (values state (reverse out)))
            ([form (in-list inputs)])
    (define-values (state* o) (rackton-repl-step state form))
    (values state* (cons o out))))

(test-case "REPL: evaluate a literal expression"
  (define-values (_ outs) (drive-session '(42)))
  (check-regexp-match #rx"42" (car outs))
  (check-regexp-match #rx"Integer" (car outs)))

(test-case "REPL: define and refer to a binding"
  (define-values (_ outs)
    (drive-session
     '((define x 7)
       x)))
  (check-regexp-match #rx"7" (cadr outs))
  (check-regexp-match #rx"Integer" (cadr outs)))

(test-case "REPL: ,type prints the inferred type without evaluating"
  (define-values (_ outs) (drive-session '((unquote type (lambda (x) x)))))
  (check-regexp-match #rx"->" (car outs)))

(test-case "REPL: bare , is an accepted no-op"
  ;; A lone comma reads as `(unquote)`; it leaves the session untouched,
  ;; emits no output, and does not signal exit.
  (define state (rackton-repl-init))
  (define-values (state* out) (rackton-repl-step state '(unquote)))
  (check-equal? out "")
  (check-false (rackton-repl-quit? state*)))

(test-case "REPL: ,geiser-no-values is a no-op"
  ;; Geiser/racket-mode probes the REPL with `,geiser-no-values`; swallow
  ;; it silently rather than reporting an unknown command.
  (define state (rackton-repl-init))
  (define-values (state* out)
    (rackton-repl-step state '(unquote geiser-no-values)))
  (check-equal? out "")
  (check-false (rackton-repl-quit? state*)))

(test-case "REPL: ,clear wipes prior definitions"
  ;; `,clear` resets the session to a fresh prelude env, so a name bound
  ;; before the clear is unbound afterward.
  (define-values (_ outs)
    (drive-session
     '((define keep 99)
       (unquote clear)
       keep)))
  (check-regexp-match #rx"clear" (list-ref outs 1))
  (check-regexp-match #rx"(?i:unbound|error)" (list-ref outs 2)))

(test-case "REPL: env persists a data and uses ctor later"
  (define-values (_ outs)
    (drive-session
     '((data (Box a) (MkBox a))
       (MkBox 5))))
  (check-regexp-match #rx"MkBox" (cadr outs))
  (check-regexp-match #rx"Box" (cadr outs)))

(test-case "REPL: ill-typed form is reported and session continues"
  (define-values (_ outs)
    (drive-session
     '((define y (+ 1 "no"))
       (define z 1)
       z)))
  ;; First entry errors; later entries still work.
  (check-regexp-match #rx"(?i:type|error|mismatch)" (list-ref outs 0))
  (check-regexp-match #rx"1" (list-ref outs 2)))

(test-case "REPL: re-declaring an instance replaces it instead of erroring"
  ;; At a REPL you iterate by re-evaluating forms; a second instance with
  ;; the same head must replace the first (not raise the module-level
  ;; coherence error), and the new method must win on the next call.
  (define-values (_ outs)
    (drive-session
     '((protocol (Greet a) (: greet (-> a String)))
       (instance (Greet Integer) (define (greet _) "hello"))
       (instance (Greet Integer) (define (greet _) "hi"))
       (greet 5))))
  (check-false (regexp-match? #rx"(?i:duplicate|error)" (list-ref outs 2))
               (list-ref outs 2))
  (check-regexp-match #rx"hi" (list-ref outs 3)))

(test-case "REPL: ,quit signals exit"
  (define-values (st _outs) (drive-session '((unquote quit))))
  (check-true (rackton-repl-quit? st)))

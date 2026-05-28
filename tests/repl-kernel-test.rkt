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

(test-case "REPL: :type prints the inferred type without evaluating"
  (define-values (_ outs) (drive-session '((:type (lambda (x) x)))))
  (check-regexp-match #rx"->" (car outs)))

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

(test-case "REPL: :quit signals exit"
  (define-values (st _outs) (drive-session '((:quit))))
  (check-true (rackton-repl-quit? st)))

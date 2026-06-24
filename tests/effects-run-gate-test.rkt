#lang racket/base

;; The effect-system guarantee: `run-eff` is gated on the EMPTY row, so a
;; computation with an UNHANDLED effect cannot be run — it is a TYPE ERROR.
;; (Compile-error-at-expansion checks need #lang racket/base.)

(require rackunit
         (for-syntax racket/base)
         "../main.rkt")

(define-syntax-rule (check-rackton-compile-error form ...)
  (check-exn
   exn:fail?
   (lambda ()
     (eval #'(rackton form ...)
           (variable-reference->namespace (#%variable-reference))))))

(test-case "running an unhandled Except effect is a type error"
  (check-rackton-compile-error
   (require "../effects.rkt")
   (: oops Integer)
   (define oops (run-eff (throw "boom")))))

(test-case "running an unhandled Writer effect is a type error"
  (check-rackton-compile-error
   (require "../effects.rkt")
   (: oops Unit)
   (define oops (run-eff (tell "x")))))

;; positive control: a pure (empty-row) computation runs.
(rackton
  (require "../effects.rkt")
  (: ok Integer)
  (define ok (run-eff (epure 42))))

(test-case "a pure computation runs (empty row)"
  (check-true #t))

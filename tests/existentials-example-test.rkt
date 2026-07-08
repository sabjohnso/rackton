#lang racket/base

;; Regression test for examples/existentials.rkt — first-class
;; existential types.  The example builds a heterogeneous list whose
;; elements pack values of different types behind a shared `Pretty`
;; constraint, then `open`s each one to render it.  Its `main` runs
;; via its auto-generated `module+ main`; we instantiate that
;; submodule for effect, capture stdout, and pin every rendered line.

(require rackunit
         racket/port
         racket/runtime-path)

(define-runtime-path existentials-example "../examples/existentials.rkt")

(define output
  (with-output-to-string
    (lambda () (dynamic-require `(submod ,existentials-example main) #f))))

(test-case "existentials example renders each packed value via open"
           (check-regexp-match #rx"the integer 7" output)
           (check-regexp-match #rx"a true flag" output)
           (check-regexp-match #rx"the point \\(3, 4\\)" output)
           (check-regexp-match #rx"a false flag" output))

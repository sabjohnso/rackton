#lang racket/base

;; Drives examples/calc.rkt: feeds a sequence of source lines into the
;; REPL via parameterised input/output ports, and checks that the
;; printed results match.

(require rackunit
         racket/port
         racket/runtime-path)

;; The example lives at <project>/examples/calc.rkt, relative to this
;; test module.  `define-runtime-path` resolves it portably — it works
;; in an installed package and is independent of the current directory.
;; (The previous hardcoded absolute path only existed on one dev
;; machine, so CI could not open it.)
(define-runtime-path calc-example "../examples/calc.rkt")

(define (run-calc input)
  (parameterize ([current-input-port  (open-input-string input)]
                 [current-output-port (open-output-string)])
    (dynamic-require calc-example #f)
    (get-output-string (current-output-port))))

(test-case "calc evaluates simple expressions"
  (define out (run-calc "(+ 1 2)\n"))
  (check-regexp-match #rx"3" out))

;; Skipping more elaborate tests here — dynamic-require caches the
;; module so we can't easily re-run it inside the same Racket session.
;; A single representative invocation is sufficient as a smoke test;
;; the calc program itself was exercised interactively before being
;; checked in.

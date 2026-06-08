#lang racket/base

;; Cross-module macro export (Option B): a Rackton module that requires a
;; library can use the macros that library provides, just like its values
;; and types.

(require rackunit
         "../main.rkt")

(rackton
  (require "macro-export-lib.rkt")

  (: r1 Integer)
  (define r1 (double 21))

  (: r2 Integer)
  (define r2 (quadruple 5)))

(test-case "an imported pattern macro expands and runs"
  (check-equal? r1 42))

(test-case "an imported macro defined in terms of another imported macro works"
  (check-equal? r2 20))

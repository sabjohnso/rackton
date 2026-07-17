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

(rackton
  (require "macro-export-procedural-lib.rkt")

  (: r3 Integer)
  (define r3 (splice-sum 2 10)))

(rackton
  ;; This block's own for-syntax require is the SAME spec the library's
  ;; sidecar records — the lift must dedup it, not stamp two scopes
  ;; carrying the same phase-1 bindings.
  (require (for-syntax racket/base syntax/parse))
  (require "macro-export-procedural-lib.rkt")

  (define-syntax local-twice
    (syntax-parser
     [(_ e:expr) #'(+ e e)]))

  (: r4 Integer)
  (define r4 (+ (local-twice 3) (splice-sum 2 10))))

(test-case "an imported pattern macro expands and runs"
  (check-equal? r1 42))

(test-case "an imported macro defined in terms of another imported macro works"
  (check-equal? r2 20))

(test-case "an imported procedural (syntax-parser) macro expands and runs"
  ;; The library's own (for-syntax …) requires must be lifted from its
  ;; sidecar — this module deliberately requires no phase-1 toolbox.
  (check-equal? r3 21))

(test-case "a local procedural macro composes with an imported one (spec dedup)"
  (check-equal? r4 27))

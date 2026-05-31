#lang rackton

;; rackton/unit — the public entry point.
;;
;; `(require rackton/unit)` brings the whole framework into scope:
;; generators (with integrated shrinking), properties, rackunit-style
;; checks, the describe/it test tree and its IO runner, and the
;; algebraic-law bundles.  Everything is funnelled through a single
;; import path (this module → laws → tree → property/check → gen) so
;; module-level instance coherence sees each instance exactly once.

(require "unit/laws.rkt")

(provide eq-laws
         ord-laws
         semigroup-laws
         monoid-laws
         functor-laws
         applicative-laws
         monad-laws
         traversable-laws
         (data-out Test)
         (data-out Outcome)
         (data-out Summary)
         it
         it-prop
         group-of
         run-tests
         summary-passed
         summary-failed
         (data-out Property)
         (data-out PropOutcome)
         for-all-gen
         for-all
         run-property
         (data-out Gen)
         (data-out Tree)
         gen-tree
         tree-value
         constant
         int-range
         bool
         gen-integer
         gen-boolean
         gen-pair
         replicate-gen
         gen-list
         element-of
         gen-string
         (data-out CheckResult)
         (data-out Assertion)
         assertion-result
         check-equal?
         check-not-equal?
         check-true
         check-false
         fail
         pass
         all-checks)

#lang rackton

;; rackton/unit — algebraic-law bundles.
;;
;; Each bundle turns a generator into a `Test` group of named
;; properties capturing the laws of a structure (Eq, Ord, Semigroup,
;; Monoid).  Laws are expressed with the positional methods `==`, `<=`,
;; `<>` (which dispatch on their runtime arguments), so the bundles work
;; for any type with the relevant instances.  `monoid-laws` takes the
;; identity element explicitly rather than via the return-typed `mempty`
;; (which does not resolve across module boundaries).
;;
;; Functor/Monad laws are intentionally NOT bundled in v1: comparing
;; mapped containers needs an `Eq` instance for the container (the
;; prelude provides none for `Maybe`/`List`) and law-checking functions
;; needs function generation (cogen) we do not have.  Users can still
;; express those laws directly with `for-all-gen` and their own `Eq`.
;;
;; Re-exports the full tree/runner/generator/check surface so a consumer
;; requires only this module (or rackton/unit).

(require "tree.rkt")

(provide eq-laws
         ord-laws
         semigroup-laws
         monoid-laws
         ;; Re-exports.
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

;; The prelude has no `Show` instance for `Pair`, so properties that
;; quantify over pairs/triples render them by showing each component.
(: show-pair2 ((Show a) => (-> (Pair a a) String)))
(define (show-pair2 p)
  (match p
    [(MkPair x y)
     (string-append "(" (string-append (show x)
                          (string-append ", " (string-append (show y) ")"))))]))

(: show-pair3 ((Show a) => (-> (Pair a (Pair a a)) String)))
(define (show-pair3 t)
  (match t
    [(MkPair x rest)
     (string-append "(" (string-append (show x)
                          (string-append ", " (string-append (show-pair2 rest) ")"))))]))

;; Eq: reflexivity and symmetry of `==`.
(: eq-laws ((Eq a) (Show a) => (-> (Gen a) Test)))
(define (eq-laws gen)
  (describe "Eq laws"
    (it-prop "reflexivity"
             (for-all gen (lambda (x) (== x x))))
    (it-prop "symmetry"
             (for-all-gen show-pair2 (gen-pair gen gen)
                          (lambda (p)
                            (match p
                              [(MkPair x y) (== (== x y) (== y x))]))))))

;; Ord: reflexivity and totality of `<=`.
(: ord-laws ((Ord a) (Show a) => (-> (Gen a) Test)))
(define (ord-laws gen)
  (describe "Ord laws"
    (it-prop "reflexivity of <="
             (for-all gen (lambda (x) (<= x x))))
    (it-prop "totality"
             (for-all-gen show-pair2 (gen-pair gen gen)
                          (lambda (p)
                            (match p
                              [(MkPair x y)
                               (if (<= x y) #t (<= y x))]))))))

;; Semigroup: associativity of `<>`.
(: semigroup-laws ((Eq a) (Show a) (Semigroup a) => (-> (Gen a) Test)))
(define (semigroup-laws gen)
  (describe "Semigroup laws"
    (it-prop "associativity"
             (for-all-gen show-pair3 (gen-pair gen (gen-pair gen gen))
                          (lambda (t)
                            (match t
                              [(MkPair x (MkPair y z))
                               (== (<> (<> x y) z) (<> x (<> y z)))]))))))

;; Monoid: `identity` is a left and right unit for `<>`.  The identity
;; element is supplied explicitly.
(: monoid-laws ((Eq a) (Show a) (Semigroup a) => (-> (Gen a) (-> a Test))))
(define (monoid-laws gen identity)
  (describe "Monoid laws"
    (it-prop "left identity"
             (for-all gen (lambda (x) (== (<> identity x) x))))
    (it-prop "right identity"
             (for-all gen (lambda (x) (== (<> x identity) x))))))

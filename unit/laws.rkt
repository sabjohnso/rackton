#lang rackton

;; rackton/unit — algebraic-law bundles.
;;
;; Each bundle turns a generator into a `Test` group of named
;; properties capturing the laws of a structure (Eq, Ord, Semigroup,
;; Monoid; Functor, Applicative, Monad, Traversable).  First-order laws
;; are expressed with the positional methods `==`, `<=`, `<>` (which
;; dispatch on their runtime arguments), so the bundles work for any
;; type with the relevant instances.  `monoid-laws` takes the identity
;; element explicitly rather than via the return-typed `mempty` (which
;; does not resolve across module boundaries).
;;
;; The higher-kinded bundles (functor/applicative/monad/traversable)
;; face two obstacles the first-order ones don't, and address each by
;; taking an explicit argument:
;;
;;   - Comparing two mapped containers needs equality on `(f Integer)`.
;;     Rather than require an `Eq` instance for the container (which the
;;     bundle, being generic over `f`, cannot name), each bundle takes
;;     an explicit `eq` predicate and an explicit `render` for
;;     counterexamples — the same move `for-all-gen` makes for `Show`.
;;   - `pure`/`return` are return-typed, so like `mempty` they do not
;;     resolve across module boundaries.  The applicative/monad bundles
;;     take the point operation explicitly and monomorphically:
;;     `monad-laws` needs it only at the element type
;;     (`-> Integer (m Integer)`); `applicative-laws` additionally needs
;;     it at the function type (`point-fn`), since the identity and
;;     homomorphism laws build an `f (Integer -> Integer)`.  The
;;     value-dispatched methods (`fmap`, `fapply`, `flatmap`,
;;     `traverse`) resolve from the class constraint as usual.
;;
;; The genuinely higher-order laws (applicative interchange/composition;
;; traversable naturality/composition) need function generation we do
;; not have, so the bundles cover the laws checkable with fixed
;; representative functions — identity and homomorphism for applicative,
;; all three monad laws, the identity law for traversable.
;;
;; Re-exports the full tree/runner/generator/check surface so a consumer
;; requires only this module (or rackton/unit).

(require "tree.rkt")

(provide eq-laws
         ord-laws
         semigroup-laws
         monoid-laws
         functor-laws
         applicative-laws
         monad-laws
         traversable-laws
         ;; Re-exports.
         (data-out Test)
         (data-out Outcome)
         (data-out Summary)
         it
         it-prop
         group-of
         run-tests
         run-tests-quiet
         run-suite
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

;; ----- higher-kinded laws -------------------------------------------
;;
;; The two representative endofunctions used by the composition laws:
;; succ = (+ 1), dbl = (* 2).  Their composite is (dbl . succ).

;; Functor: `fmap id == id` and `fmap (g . f) == fmap g . fmap f`.
;; `eqf` compares two `(f Integer)`; `render` shows one for the report.
(: functor-laws
   ((Functor f) =>
    (-> (-> (f Integer) (-> (f Integer) Boolean))
        (-> (-> (f Integer) String)
            (-> (Gen (f Integer)) Test)))))
(define (functor-laws eqf render gen)
  (describe "Functor laws"
    (it-prop "identity: fmap id == id"
             (for-all-gen render gen
                          (lambda (xs)
                            (eqf (fmap (lambda (x) x) xs) xs))))
    (it-prop "composition: fmap (g . f) == fmap g . fmap f"
             (for-all-gen render gen
                          (lambda (xs)
                            (eqf (fmap (lambda (n) (* 2 (+ n 1))) xs)
                                 (fmap (lambda (n) (* 2 n))
                                       (fmap (lambda (n) (+ n 1)) xs))))))))

;; Applicative: identity (`pure id <*> v == v`) and homomorphism
;; (`pure f <*> pure x == pure (f x)`).  `point` is `pure` at the value
;; type; `point-fn` is `pure` at the function type — both monomorphic,
;; since `pure` is return-typed and can't be passed polymorphically.
(: applicative-laws
   ((Applicative f) =>
    (-> (-> (f Integer) (-> (f Integer) Boolean))
        (-> (-> (f Integer) String)
            (-> (-> Integer (f Integer))
                (-> (-> (-> Integer Integer) (f (-> Integer Integer)))
                    (-> (Gen (f Integer)) Test)))))))
(define (applicative-laws eqf render point point-fn gen)
  (describe "Applicative laws"
    (it-prop "identity: pure id <*> v == v"
             (for-all-gen render gen
                          (lambda (v)
                            (eqf (fapply (point-fn (lambda (x) x)) v) v))))
    (it-prop "homomorphism: pure f <*> pure x == pure (f x)"
             (for-all-gen integer->string gen-integer
                          (lambda (n)
                            (eqf (fapply (point-fn (lambda (m) (+ m 1))) (point n))
                                 (point (+ n 1))))))))

;; Monad: left identity, right identity, associativity.  `point` is
;; `return` at the element type (monomorphic, for the reason above);
;; the fixed Kleisli arrows `\x -> point (x+1)` and `\y -> point (y*2)`
;; are built from it inside each law.
(: monad-laws
   ((Monad m) =>
    (-> (-> (m Integer) (-> (m Integer) Boolean))
        (-> (-> (m Integer) String)
            (-> (-> Integer (m Integer))
                (-> (Gen (m Integer)) Test))))))
(define (monad-laws eqf render point gen)
  (describe "Monad laws"
    (it-prop "left identity: return a >>= k == k a"
             (for-all-gen integer->string gen-integer
                          (lambda (n)
                            (eqf (flatmap (lambda (x) (point (+ x 1))) (point n))
                                 (point (+ n 1))))))
    (it-prop "right identity: m >>= return == m"
             (for-all-gen render gen
                          (lambda (m)
                            (eqf (flatmap point m) m))))
    (it-prop "associativity: (m >>= k) >>= h == m >>= (\\x -> k x >>= h)"
             (for-all-gen render gen
                          (lambda (m)
                            (eqf (flatmap (lambda (_y) (point (* _y 2)))
                                          (flatmap (lambda (x) (point (+ x 1))) m))
                                 (flatmap (lambda (x)
                                            (flatmap (lambda (_y) (point (* _y 2)))
                                                     (point (+ x 1))))
                                          m)))))))

;; Traversable: the identity law specialised to the `Maybe` applicative
;; (`Some` is `Maybe`'s `pure`, and `traverse pure == pure`), so
;; `traverse Some t == Some t`.  `eqm` compares two `(Maybe (t Integer))`.
(: traversable-laws
   ((Traversable t) =>
    (-> (-> (Maybe (t Integer)) (-> (Maybe (t Integer)) Boolean))
        (-> (-> (t Integer) String)
            (-> (Gen (t Integer)) Test)))))
(define (traversable-laws eqm render gen)
  (describe "Traversable laws"
    (it-prop "identity (Maybe applicative): traverse Some t == Some t"
             (for-all-gen render gen
                          (lambda (t)
                            (eqm (traverse (lambda (x) (Some x)) t)
                                 (Some t)))))))

#lang rackton

;; Property tests that the newly-attached protocol laws actually HOLD on
;; real prelude / stdlib instances.  Prelude protocols are loaded without
;; rackton/unit, so they do not auto-emit `<Class>-laws` bundles (that
;; mechanism is for protocols defined alongside the import); instead we
;; drive each RUNNABLE law as a native property over a concrete instance.
;; The type-check-only laws (Prod/Coprod β/η, the mtl and Apply/ComonadApply
;; laws, Bifunctor composition) are verified by the prelude/stdlib simply
;; compiling — inference checks every `#:laws` body when prelude-env builds.
;;
;; Written in the native framework end to end: `run-suite` panics on any
;; failure so `raco test` reports non-zero.

(require "../unit.rkt"
         rackton/control/comonad)

;; ----- generators ---------------------------------------------------

(: gi (Gen Integer))
(define gi (int-range -50 50))

(: gen-list-int (Gen (List Integer)))
(define gen-list-int (gen-list gi))

;; ----- local equalities (Identity has no prelude Eq instance) -------

(: eq-id (-> (Identity Integer) (-> (Identity Integer) Boolean)))
(define (eq-id a b) (== (extract a) (extract b)))

(: eq-id2 (-> (Identity (Identity Integer)) (-> (Identity (Identity Integer)) Boolean)))
(define (eq-id2 a b) (eq-id (extract a) (extract b)))

(: eq-id3 (-> (Identity (Identity (Identity Integer)))
              (-> (Identity (Identity (Identity Integer))) Boolean)))
(define (eq-id3 a b) (eq-id2 (extract a) (extract b)))

;; ----- the suite ----------------------------------------------------

(: suite (List Test))
(define suite
  (list
   ;; Integral: quot/rem and div/mod reconstruct the dividend (guarded
   ;; against a zero divisor, as in the law itself).
   (it-prop "Integral quot-rem reconstructs the dividend"
     (for-all (gen-pair gi gi)
       (lambda (p)
         (match p
           [(Pair x y)
            (if (== y 0) #t (== (+ (* (quot x y) y) (rem x y)) x))]))))
   (it-prop "Integral div-mod reconstructs the dividend"
     (for-all (gen-pair gi gi)
       (lambda (p)
         (match p
           [(Pair x y)
            (if (== y 0) #t (== (+ (* (div x y) y) (mod x y)) x))]))))

   ;; Real: to-rational is monotone on Integer.
   (it-prop "Real to-rational is monotone"
     (for-all (gen-pair gi gi)
       (lambda (p)
         (match p
           [(Pair x y)
            (if (<= x y) (<= (to-rational x) (to-rational y)) #t)]))))

   ;; Bifunctor identity on Pair: bimap id id = id.
   (it-prop "Bifunctor identity holds on Pair"
     (for-all (gen-pair gi gi)
       (lambda (p)
         (match (bimap (lambda (x) x) (lambda (x) x) p)
           [(Pair a b) (match p [(Pair c d) (and (== a c) (== b d))])]))))

   ;; Comonad laws on Identity (built inside each predicate, since
   ;; Identity has no Show instance for counterexample rendering).
   (it-prop "Comonad extract-duplicate"
     (for-all gi
       (lambda (n) (eq-id (extract (duplicate (Identity n))) (Identity n)))))
   (it-prop "Comonad fmap-extract-duplicate"
     (for-all gi
       (lambda (n) (eq-id (fmap extract (duplicate (Identity n))) (Identity n)))))
   (it-prop "Comonad duplicate-duplicate (coassociativity)"
     (for-all gi
       (lambda (n)
         (eq-id3 (duplicate (duplicate (Identity n)))
                 (fmap duplicate (duplicate (Identity n)))))))

   ;; Semigroup/Monoid laws on SHIPPED instances.  Until now the only
   ;; semigroup-laws invocation was against a deliberately-broken test
   ;; type (to prove the bundle catches failure); associativity of
   ;; `mappend` was verified on NO real instance.  Run the bundles over
   ;; String and (List Integer): semigroup-laws checks associativity,
   ;; monoid-laws the two-sided identity.
   (group-of "Semigroup/Monoid on shipped instances"
     (list
      (semigroup-laws gen-string)
      (semigroup-laws gen-list-int)
      (monoid-laws gen-string "")
      (monoid-laws gen-list-int (ann Nil (List Integer)))))))

(: main Unit)
(define main (run-io (run-suite "prelude/stdlib protocol laws" suite)))

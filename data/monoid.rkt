#lang rackton

;; rackton/data/monoid — Data.Monoid.
;;
;; The numeric Monoid newtypes over Integer (`Sum` additive, `Product`
;; multiplicative), the Boolean monoids (`All` conjunction, `Any`
;; disjunction), the composition monoid (`Endo`, endomorphisms under
;; `.`), and the order-flipping wrapper (`Dual`).  Moved out of the
;; auto-prelude (Phase 2): `mempty` / `mappend` for these types now require
;; `(require rackton/data/monoid)`.  The instances register cross-module
;; via the Enabler-A dispatch tables and Enabler-B coherence, so
;; importers resolve `mempty` at these types without any prelude
;; support.

(provide (all-defined-out))

(newtype Sum     (Sum     Integer))
(newtype Product (Product Integer))

(: get-sum     (-> Sum Integer))
(define (get-sum s)     (match s [(Sum n) n]))

(: get-product (-> Product Integer))
(define (get-product p) (match p [(Product n) n]))

(instance (Semigroup Sum)
  (define (mappend a b)
    (match a [(Sum x)
              (match b [(Sum y) (Sum (+ x y))])])))

(instance (Monoid Sum)
  (define mempty (Sum 0)))

(instance (Semigroup Product)
  (define (mappend a b)
    (match a [(Product x)
              (match b [(Product y) (Product (* x y))])])))

(instance (Monoid Product)
  (define mempty (Product 1)))

;; --- Boolean monoids: All (conjunction) / Any (disjunction) --------

(newtype All (MkAll Boolean))
(newtype Any (Any Boolean))

(: get-all (-> All Boolean))
(define (get-all a) (match a [(MkAll b) b]))

(: get-any (-> Any Boolean))
(define (get-any a) (match a [(Any b) b]))

(instance (Semigroup All)
  (define (mappend a b)
    (match a [(MkAll x) (match b [(MkAll y) (MkAll (and x y))])])))

(instance (Monoid All)
  (define mempty (MkAll #t)))

(instance (Semigroup Any)
  (define (mappend a b)
    (match a [(Any x) (match b [(Any y) (Any (or x y))])])))

(instance (Monoid Any)
  (define mempty (Any #f)))

;; --- Endo: endomorphisms (a -> a) under composition ----------------
;;
;; mappend composes (left after right, matching Haskell's `Endo (f . g)`)
;; and mempty is the identity function.  The instances need no
;; constraint on `a`: composition and identity are uniform.

(newtype (Endo a) (Endo (-> a a)))

(: app-endo (-> (Endo a) (-> a a)))
(define (app-endo e) (match e [(Endo f) f]))

(instance (Semigroup (Endo a))
  (define (mappend a b)
    (match a [(Endo f)
              (match b [(Endo g) (Endo (lambda (x) (f (g x))))])])))

(instance (Monoid (Endo a))
  (define mempty (Endo (lambda (x) x))))

;; --- Dual: the same Semigroup with its arguments flipped -----------
;;
;; Dual a mappend Dual b = Dual (b mappend a); mempty lifts the inner monoid's
;; identity.  The inner mappend / mempty come from the wrapped type's own
;; instance via the (Semigroup a) / (Monoid a) constraints.

(newtype (Dual a) (Dual a))

(: get-dual (-> (Dual a) a))
(define (get-dual d) (match d [(Dual x) x]))

(instance ((Semigroup a) => (Semigroup (Dual a)))
  (define (mappend a b)
    (match a [(Dual x) (match b [(Dual y) (Dual (mappend y x))])])))

(instance ((Monoid a) => (Monoid (Dual a)))
  (define mempty (Dual mempty)))

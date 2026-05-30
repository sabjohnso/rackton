#lang rackton

;; rackton/data/monoid — Data.Monoid.
;;
;; The numeric Monoid newtypes over Integer (`Sum` additive, `Product`
;; multiplicative), the Boolean monoids (`All` conjunction, `Any`
;; disjunction), the composition monoid (`Endo`, endomorphisms under
;; `.`), and the order-flipping wrapper (`Dual`).  Moved out of the
;; auto-prelude (Phase 2): `mempty` / `<>` for these types now require
;; `(require rackton/data/monoid)`.  The instances register cross-module
;; via the Enabler-A dispatch tables and Enabler-B coherence, so
;; importers resolve `mempty` at these types without any prelude
;; support.

(provide (all-defined-out))

(newtype Sum     (MkSum     Integer))
(newtype Product (MkProduct Integer))

(: get-sum     (-> Sum Integer))
(define (get-sum s)     (match s [(MkSum n) n]))

(: get-product (-> Product Integer))
(define (get-product p) (match p [(MkProduct n) n]))

(instance (Semigroup Sum)
  (define (<> a b)
    (match a [(MkSum x)
              (match b [(MkSum y) (MkSum (+ x y))])])))

(instance (Monoid Sum)
  (define mempty (MkSum 0)))

(instance (Semigroup Product)
  (define (<> a b)
    (match a [(MkProduct x)
              (match b [(MkProduct y) (MkProduct (* x y))])])))

(instance (Monoid Product)
  (define mempty (MkProduct 1)))

;; --- Boolean monoids: All (conjunction) / Any (disjunction) --------

(newtype All (MkAll Boolean))
(newtype Any (MkAny Boolean))

(: get-all (-> All Boolean))
(define (get-all a) (match a [(MkAll b) b]))

(: get-any (-> Any Boolean))
(define (get-any a) (match a [(MkAny b) b]))

(instance (Semigroup All)
  (define (<> a b)
    (match a [(MkAll x) (match b [(MkAll y) (MkAll (and x y))])])))

(instance (Monoid All)
  (define mempty (MkAll #t)))

(instance (Semigroup Any)
  (define (<> a b)
    (match a [(MkAny x) (match b [(MkAny y) (MkAny (or x y))])])))

(instance (Monoid Any)
  (define mempty (MkAny #f)))

;; --- Endo: endomorphisms (a -> a) under composition ----------------
;;
;; <> composes (left after right, matching Haskell's `Endo (f . g)`)
;; and mempty is the identity function.  The instances need no
;; constraint on `a`: composition and identity are uniform.

(newtype (Endo a) (MkEndo (-> a a)))

(: app-endo (-> (Endo a) (-> a a)))
(define (app-endo e) (match e [(MkEndo f) f]))

(instance (Semigroup (Endo a))
  (define (<> a b)
    (match a [(MkEndo f)
              (match b [(MkEndo g) (MkEndo (lambda (x) (f (g x))))])])))

(instance (Monoid (Endo a))
  (define mempty (MkEndo (lambda (x) x))))

;; --- Dual: the same Semigroup with its arguments flipped -----------
;;
;; Dual a <> Dual b = Dual (b <> a); mempty lifts the inner monoid's
;; identity.  The inner <> / mempty come from the wrapped type's own
;; instance via the (Semigroup a) / (Monoid a) constraints.

(newtype (Dual a) (MkDual a))

(: get-dual (-> (Dual a) a))
(define (get-dual d) (match d [(MkDual x) x]))

(instance ((Semigroup a) => (Semigroup (Dual a)))
  (define (<> a b)
    (match a [(MkDual x) (match b [(MkDual y) (MkDual (<> y x))])])))

(instance ((Monoid a) => (Monoid (Dual a)))
  (define mempty (MkDual mempty)))

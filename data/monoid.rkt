#lang rackton

;; rackton/data/monoid — Data.Monoid.
;;
;; The additive (`Sum`) and multiplicative (`Product`) Monoid newtypes
;; over Integer.  Moved out of the auto-prelude (Phase 2): `mempty` /
;; `<>` for these types now require `(require rackton/data/monoid)`.
;; The instances register cross-module via the Enabler-A dispatch tables
;; and Enabler-B coherence, so importers resolve `mempty` at Sum/Product
;; without any prelude support.

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

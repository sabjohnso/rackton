#lang rackton

;; rackton/data/semigroup — Data.Semigroup.  Selection newtypes whose
;; `mappend` keeps the smaller / larger / first / last operand.  (`mappend` itself
;; and the Semigroup/Monoid classes are in the prelude.)
;;
;; Min/Max carry only Semigroup (no Monoid: that would need a bounded
;; identity, which Rackton's numeric types don't provide).

(provide (all-defined-out))

(newtype (Min a) (Min a))
(newtype (Max a) (Max a))
(newtype (First a) (First a))
(newtype (Last  a) (Last  a))

(: get-min   (-> (Min a) a))   (define (get-min m)   (match m [(Min x) x]))
(: get-max   (-> (Max a) a))   (define (get-max m)   (match m [(Max x) x]))
(: get-first (-> (First a) a)) (define (get-first m) (match m [(First x) x]))
(: get-last  (-> (Last a) a))  (define (get-last m)  (match m [(Last x) x]))

(instance ((Ord a) => (Semigroup (Min a)))
  (define (mappend a b)
    (match a [(Min x) (match b [(Min y) (if (< x y) a b)])])))

(instance ((Ord a) => (Semigroup (Max a)))
  (define (mappend a b)
    (match a [(Max x) (match b [(Max y) (if (> x y) a b)])])))

(instance (Semigroup (First a))
  (define (mappend a _b) a))

(instance (Semigroup (Last a))
  (define (mappend _a b) b))

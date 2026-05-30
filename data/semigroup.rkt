#lang rackton

;; rackton/data/semigroup — Data.Semigroup.  Selection newtypes whose
;; `<>` keeps the smaller / larger / first / last operand.  (`<>` itself
;; and the Semigroup/Monoid classes are in the prelude.)
;;
;; Min/Max carry only Semigroup (no Monoid: that would need a bounded
;; identity, which Rackton's numeric types don't provide).

(provide (all-defined-out))

(newtype (Min a) (MkMin a))
(newtype (Max a) (MkMax a))
(newtype (First a) (MkFirst a))
(newtype (Last  a) (MkLast  a))

(: get-min   (-> (Min a) a))   (define (get-min m)   (match m [(MkMin x) x]))
(: get-max   (-> (Max a) a))   (define (get-max m)   (match m [(MkMax x) x]))
(: get-first (-> (First a) a)) (define (get-first m) (match m [(MkFirst x) x]))
(: get-last  (-> (Last a) a))  (define (get-last m)  (match m [(MkLast x) x]))

(instance ((Ord a) => (Semigroup (Min a)))
  (define (<> a b)
    (match a [(MkMin x) (match b [(MkMin y) (if (< x y) a b)])])))

(instance ((Ord a) => (Semigroup (Max a)))
  (define (<> a b)
    (match a [(MkMax x) (match b [(MkMax y) (if (> x y) a b)])])))

(instance (Semigroup (First a))
  (define (<> a _b) a))

(instance (Semigroup (Last a))
  (define (<> _a b) b))

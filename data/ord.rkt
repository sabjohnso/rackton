#lang rackton

;; rackton/data/ord — Data.Ord.  (min / max / the comparison operators
;; are Ord methods in the prelude.)

(provide (all-defined-out))

;; (clamp lo hi x) — x confined to the inclusive range [lo, hi].
(: clamp ((Ord a) => (-> a (-> a (-> a a)))))
(define (clamp lo hi x) (max lo (min hi x)))

;; (min-by key x y) / (max-by key x y) — the argument with the
;; smaller / larger projected key (ties favour the first).
(: min-by ((Ord b) => (-> (-> a b) (-> a (-> a a)))))
(define (min-by key x y) (if (<= (key x) (key y)) x y))

(: max-by ((Ord b) => (-> (-> a b) (-> a (-> a a)))))
(define (max-by key x y) (if (>= (key x) (key y)) x y))

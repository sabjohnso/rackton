#lang rackton

;; rackton/data/function — Data.Function.  (id / const / flip / compose
;; are in the prelude.)

(provide (all-defined-out))

;; (on g f x y) = (g (f x) (f y)) — combine two values under a common
;; projection, e.g. (sort-by (on < key) …).
(: on (-> (-> b (-> b c)) (-> (-> a b) (-> a (-> a c)))))
(define (on g f x y) (g (f x) (f y)))

;; (apply-to x f) = (f x) — reverse application (Haskell's `&`).
(: apply-to (-> a (-> (-> a b) b)))
(define (apply-to x f) (f x))

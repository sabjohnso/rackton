#lang rackton

;; rackton/data/list/nonempty — Data.List.NonEmpty.  A list guaranteed
;; to have at least one element, so head / tail are total.

(provide (all-defined-out))

(data (NonEmpty a) (NonEmpty a (List a)))

;; construct from a head and a (possibly empty) tail.
(: nonempty (-> a (-> (List a) (NonEmpty a))))
(define (nonempty h t) (NonEmpty h t))

(: ne-head (-> (NonEmpty a) a))
(define (ne-head ne) (match ne [(NonEmpty h _) h]))

(: ne-tail (-> (NonEmpty a) (List a)))
(define (ne-tail ne) (match ne [(NonEmpty _ t) t]))

(: ne-to-list (-> (NonEmpty a) (List a)))
(define (ne-to-list ne) (match ne [(NonEmpty h t) (Cons h t)]))

(: ne-from-list (-> (List a) (Maybe (NonEmpty a))))
(define (ne-from-list xs)
  (match xs
    [(Nil)      None]
    [(Cons h t) (Some (NonEmpty h t))]))

(: ne-cons (-> a (-> (NonEmpty a) (NonEmpty a))))
(define (ne-cons x ne) (match ne [(NonEmpty h t) (NonEmpty x (Cons h t))]))

(: ne-map (-> (-> a b) (-> (NonEmpty a) (NonEmpty b))))
(define (ne-map f ne) (match ne [(NonEmpty h t) (NonEmpty (f h) (fmap f t))]))

(: ne-length (-> (NonEmpty a) Integer))
(define (ne-length ne) (match ne [(NonEmpty _ t) (+ 1 (length t))]))

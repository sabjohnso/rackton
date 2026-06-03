#lang rackton

;; rackton/data/foldable — Data.Foldable.  Generic folds over any
;; (Foldable t) (the prelude's instances are List and Maybe).  foldr /
;; length / to-list / sum are the prelude Foldable members; these are
;; the derived combinators.

(provide (all-defined-out))

;; fold-map: map each element into a monoid and combine (Haskell foldMap).
(: fold-map ((Monoid b) (Foldable t) => (-> (-> a b) (-> (t a) b))))
(define (fold-map f t)
  (foldr (lambda (x acc) (mappend (f x) acc)) mempty t))

;; fold: combine a foldable of monoid values (Haskell fold / mconcat).
(: fold ((Monoid m) (Foldable t) => (-> (t m) m)))
(define (fold t) (foldr (lambda (x acc) (mappend x acc)) mempty t))

;; any-of / all-of: existential / universal over a foldable.
(: any-of ((Foldable t) => (-> (-> a Boolean) (-> (t a) Boolean))))
(define (any-of p t) (foldr (lambda (x acc) (if (p x) #t acc)) #f t))

(: all-of ((Foldable t) => (-> (-> a Boolean) (-> (t a) Boolean))))
(define (all-of p t) (foldr (lambda (x acc) (if (p x) acc #f)) #t t))

;; elem-of: membership in any foldable.
(: elem-of ((Eq a) (Foldable t) => (-> a (-> (t a) Boolean))))
(define (elem-of x t) (foldr (lambda (y acc) (if (== x y) #t acc)) #f t))

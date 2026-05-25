#lang rackton

;; A Rackton library module — defines a Tree ADT and helper functions.

(provide (all-defined-out))

(define-data (Tree a) Leaf (Node (Tree a) a (Tree a)))

(: max-int (-> Integer (-> Integer Integer)))
(define (max-int a b)
  (if (< a b) b a))

(: tree-sum (-> (Tree Integer) Integer))
(define (tree-sum t)
  (match t
    [(Leaf)        0]
    [(Node l x r)  (+ x (+ (tree-sum l) (tree-sum r)))]))

(: tree-depth (-> (Tree a) Integer))
(define (tree-depth t)
  (match t
    [(Leaf)        0]
    [(Node l _ r)  (+ 1 (max-int (tree-depth l) (tree-depth r)))]))

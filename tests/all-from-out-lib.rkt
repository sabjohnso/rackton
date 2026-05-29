#lang rackton

;; Enabler C fixture: the leaf module whose exports get re-exported.

(provide (all-defined-out))

(data (Tree a) Leaf (Node (Tree a) a (Tree a)))

(: tree-size (-> (Tree a) Integer))
(define (tree-size t)
  (match t
    [(Leaf)       0]
    [(Node l _ r) (+ 1 (+ (tree-size l) (tree-size r)))]))

#lang racket/base

;; `#:deriving Eq Show` on an ADT auto-generates the matching instances.

(require rackunit
         "../main.rkt")

(rackton
  (data (Tree a)
    Leaf
    (Node (Tree a) a (Tree a))
    #:deriving Eq Show)

  (define t1 (Node Leaf 1 (Node Leaf 2 Leaf)))
  (define t2 (Node Leaf 1 (Node Leaf 2 Leaf)))
  (define t3 (Node Leaf 1 (Node Leaf 3 Leaf)))

  (: tree-eq Boolean)
  (define tree-eq (== t1 t2))

  (: tree-neq Boolean)
  (define tree-neq (== t1 t3))

  (: tree-show String)
  (define tree-show (show t1)))

(test-case "derived Eq compares structurally"
  (check-true  tree-eq)
  (check-false tree-neq))

(test-case "derived Show renders constructors"
  (check-equal? tree-show "(Node Leaf 1 (Node Leaf 2 Leaf))"))

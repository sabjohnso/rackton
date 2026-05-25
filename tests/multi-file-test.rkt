#lang racket/base

;; Exercises the multi-file feature: a Rackton client module
;; requires another Rackton module by path; the imported schemes survive
;; the boundary so the client can type-check uses of the imported
;; bindings without redeclaring them.

(require rackunit
         "../main.rkt")

(rackton
  (require "multi-file-lib.rkt")

  (: example-sum Integer)
  (define example-sum
    (tree-sum (Node Leaf 1 (Node Leaf 2 (Node Leaf 3 Leaf)))))

  (: example-depth Integer)
  (define example-depth
    (tree-depth (Node (Node Leaf 1 Leaf) 2 Leaf))))

(test-case "imported tree-sum types and runs"
  (check-equal? example-sum 6))

(test-case "imported tree-depth works on polymorphic Tree"
  (check-equal? example-depth 2))

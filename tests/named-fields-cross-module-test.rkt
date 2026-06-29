#lang racket/base

;; A data type's named fields cross a module boundary: the importer
;; recovers them from the sidecar so keyword construction `(C :f v)`
;; validates against the imported constructor's declared fields.

(require rackunit
         (for-syntax racket/base)
         "../main.rkt")

(rackton
  (require "named-fields-cross-module-lib.rkt")

  (: leaf (Tree Integer))
  (define leaf (Leaf :value 7))

  (: tree (Tree Integer))
  (define tree (Branch :left (Leaf :value 1) :right (Leaf :value 2)))

  (: leaf-val Integer)
  (define leaf-val (match leaf [(Leaf x) x] [(Branch _ _) 0]))

  (: tree-sum Integer)
  (define tree-sum
    (match tree
      [(Leaf x) x]
      [(Branch l r)
       (+ (match l [(Leaf x) x] [(Branch _ _) 0])
          (match r [(Leaf x) x] [(Branch _ _) 0]))])))

(test-case "imported named ctor supports keyword construction"
  (check-equal? leaf-val 7))

(test-case "imported multi-field named ctor builds in order"
  (check-equal? tree-sum 3))

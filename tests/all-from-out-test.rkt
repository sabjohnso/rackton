#lang racket/base

;; Enabler C regression: the client requires only the re-export module,
;; yet sees the leaf module's type (Tree), constructors (Leaf/Node), and
;; function (tree-size) — all carried through (all-from-out …).

(require rackunit
         "../main.rkt")

(rackton
  (require "all-from-out-mid.rkt")

  (: n Integer)
  (define n (tree-size (Node Leaf 5 (Node Leaf 6 Leaf)))))

(test-case "all-from-out re-exports types, ctors, and values"
  (check-equal? n 2))

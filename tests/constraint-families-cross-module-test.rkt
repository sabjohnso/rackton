#lang racket/base

;; A constraint family crosses a module boundary: the importer recovers
;; its clauses from the sidecar and reduces `(All Show xs)` over a
;; concrete imported list just as the defining module would.

(require rackunit
         (for-syntax racket/base)
         "../main.rkt")

(rackton
  (require "constraint-families-cross-module-lib.rkt")

  (data (Proxy a) MkProxy)
  (: witness ((All Show xs) => (-> (Proxy xs) Integer)))
  (define (witness p) 0)

  (: pr (Proxy (TCons Integer (TCons String TNil))))
  (define pr MkProxy)
  (: r Integer)
  (define r (witness pr)))

(test-case "an imported constraint family reduces in the importer"
  (check-equal? r 0))

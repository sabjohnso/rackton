#lang racket/base

;; A data family crosses a module boundary: the family tcon and its
;; instance constructors travel via the sidecar, so the importer builds
;; and matches values of each instance.

(require rackunit
         (for-syntax racket/base)
         "../main.rkt")

(rackton
  (require "data-families-cross-module-lib.rkt")

  (: b (Arr Boolean))
  (define b (MkBits 7))

  (: popcount (-> (Arr Boolean) Integer))
  (define (popcount a) (match a [(MkBits n) n]))

  (: r Integer)
  (define r (popcount b)))

(test-case "an imported data family builds and matches in the importer"
  (check-equal? r 7))

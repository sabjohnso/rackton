#lang rackton

;; (struct-out S) — bundles the struct's type, its single
;; constructor, and every field accessor (`S-<field>`).  A bare
;; `helper` is intentionally not exported.

(struct Point
  [x : Integer]
  [y : Integer])

(: helper (-> Integer Integer))
(define (helper n) (+ n 1))

(provide (struct-out Point))

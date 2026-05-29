#lang racket/base

;; The rackton/batteries umbrella re-exports every stdlib module, so a
;; single import brings in (so far) data/maybe and data/monoid.

(require rackunit
         "../main.rkt")

(rackton
  (require rackton/batteries)

  ;; from data/maybe
  (: r Integer)
  (define r (from-maybe 0 (Some 9)))

  ;; from data/monoid
  (: s Sum)
  (define s (<> (MkSum 3) (<> (MkSum 4) mempty))))

(test-case "batteries re-exports data/maybe and data/monoid"
  (check-equal? r 9)
  (check-equal? (get-sum s) 7))

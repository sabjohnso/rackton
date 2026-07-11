#lang racket/base

;; Enabler D regression: import a stdlib module by COLLECTION PATH
;; (rackton/data/maybe), not a relative path.  Must resolve and recover
;; the imported schemes.  (Already worked out of the box — this pins it.)

(require rackunit
         "../main.rkt")

(rackton
  (require rackton/data/maybe)

  (: r Integer)
  (define r (from-maybe 0 (Some 5)))

  (: r2 Integer)
  (define r2 (from-maybe 7 None))

  (: j Boolean)
  (define j (some? (Some 1)))

  (: n Boolean)
  (define n (none? None)))

(test-case "collection-path require of rackton/data/maybe"
  (check-equal? r 5)
  (check-equal? r2 7)
  (check-true j)
  (check-true n))

#lang racket/base

;; Phase 4: properties and the shrink loop.
;;
;; `for-all-gen` builds a Property from an explicit generator and a
;; predicate (and uses `show` to render counterexamples).  `run-property`
;; runs N cases from a starting seed; on the first failing case it walks
;; the shrink tree to the minimal failing value and reports it.

(require rackunit
         "../main.rkt")

(rackton
  (require "../unit/property.rkt")

  ;; A true property passes every case.
  (: passes Boolean)
  (define passes
    (match (run-property 50 12345
                         (for-all (int-range 0 100)
                                      (lambda (x) (== (+ x 0) x))))
      [(PropPassed _)   #t]
      [(PropFailed _ _) #f]))

  ;; A false property (x < 5) fails; shrinking finds the minimal failing
  ;; value, which is 5 (4 still satisfies x < 5).
  (: minimal String)
  (define minimal
    (match (run-property 50 12345
                         (for-all (int-range 0 100)
                                      (lambda (x) (< x 5))))
      [(PropPassed _)      "NO-FAILURE"]
      [(PropFailed shown _) shown])))

(test-case "a true property passes all cases"
  (check-true passes))

(test-case "a false property shrinks to the minimal counterexample"
  (check-equal? minimal "5"))

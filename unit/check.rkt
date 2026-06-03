#lang rackton

;; rackton/unit — assertion / check combinators.
;;
;; A check is a pure VALUE (`Assertion`), not a thrown exception, so the
;; runner aggregates results without aborting on the first failure.
;; Assertions form a `Semigroup` where the first failure wins (so an
;; `it` body can bundle several checks); `pass` is the identity element,
;; exported as a plain value rather than a return-typed `mempty` so it
;; resolves across module boundaries.
;;
;; Public API: CheckResult, Assertion, assertion-result, check-equal?,
;; check-not-equal?, check-true, check-false, fail, pass, all-checks.

(provide (data-out CheckResult)
         (data-out Assertion)
         assertion-result
         check-equal?
         check-not-equal?
         check-true
         check-false
         fail
         pass
         all-checks)

(data CheckResult
  CheckPass
  (CheckFail String))

(data Assertion (Assertion CheckResult))

(: assertion-result (-> Assertion CheckResult))
(define (assertion-result a)
  (match a [(Assertion r) r]))

;; First failure wins; otherwise pass.
(instance (Semigroup Assertion)
  (define (mappend x y)
    (match x
      [(Assertion (CheckFail _)) x]
      [(Assertion (CheckPass))   y])))

(: pass Assertion)
(define pass (Assertion CheckPass))

(: fail (-> String Assertion))
(define (fail msg) (Assertion (CheckFail msg)))

(: check-true (-> Boolean Assertion))
(define (check-true b)
  (if b pass (fail "expected #t but got #f")))

(: check-false (-> Boolean Assertion))
(define (check-false b)
  (if b (fail "expected #f but got #t") pass))

(: check-equal? ((Eq a) (Show a) => (-> a (-> a Assertion))))
(define (check-equal? actual expected)
  (if (== actual expected)
      pass
      (fail (string-append "expected "
                           (string-append (show expected)
                                          (string-append " but got "
                                                         (show actual)))))))

(: check-not-equal? ((Eq a) (Show a) => (-> a (-> a Assertion))))
(define (check-not-equal? actual unexpected)
  (if (== actual unexpected)
      (fail (string-append "expected a value other than " (show unexpected)))
      pass))

;; Combine a list of assertions, keeping the first failure.
(: all-checks (-> (List Assertion) Assertion))
(define (all-checks xs)
  (foldr (lambda (a acc) (mappend a acc)) pass xs))

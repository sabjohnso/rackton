#lang racket/base

;; Phase 5: assertion / check combinators.
;;
;; Checks are pure VALUES (an `Assertion`), never thrown exceptions, so
;; the runner can aggregate them without aborting.  A failing check
;; carries a message naming the values involved.  Assertions combine via
;; `Semigroup`, where the first failure wins.

;; rackunit is prefixed because rackton/unit's own check-* combinators
;; deliberately share rackunit's names; both are pulled into this
;; #lang racket/base harness, so they would otherwise collide.
(require (prefix-in ru: rackunit)
         "../main.rkt")

(rackton
  (require "../unit/check.rkt")

  ;; check-equal? passes on equal values.
  (: pass-eq Boolean)
  (define pass-eq
    (match (assertion-result (check-equal? 1 1))
      [(CheckPass)   #t]
      [(CheckFail _) #f]))

  ;; … and fails with a message naming expected and actual.
  (: fail-eq-msg String)
  (define fail-eq-msg
    (match (assertion-result (check-equal? 1 2))
      [(CheckPass)   "NO-FAIL"]
      [(CheckFail m) m]))

  ;; Combining assertions keeps the first failure.
  (: first-failure-wins Boolean)
  (define first-failure-wins
    (match (assertion-result (<> (fail "first") (fail "second")))
      [(CheckFail m) (== m "first")]
      [(CheckPass)   #f])))

(ru:test-case "check-equal? passes on equal values"
  (ru:check-true pass-eq))

(ru:test-case "check-equal? failure names expected and actual"
  (ru:check-equal? fail-eq-msg "expected 2 but got 1"))

(ru:test-case "Assertion <> keeps the first failure"
  (ru:check-true first-failure-wins))

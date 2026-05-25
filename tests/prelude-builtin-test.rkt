#lang racket/base

;; Exercises the built-in prelude: Num, Eq, Ord, Show, plus
;; the prelude ADTs (Maybe, List, Pair, Result, Unit) and combinators.
;; User code does NOT redeclare any of these — they're inherited.

(require rackunit
         "../main.rkt")

(rackton
  ;; Num: dispatch over Integer
  (define a (+ 1 2))
  (define b (* (- 10 3) 6))

  ;; Eq + Ord
  (define eq-test (== 1 1))
  (define ord-test (< 1 2))
  (define ord-default-gt (> 5 3))
  (define ord-default-le (<= 4 4))

  ;; Show
  (define show-int (show 42))
  (define show-bool (show #t))

  ;; Prelude Maybe
  (: from-maybe ((Eq a) => (-> a (-> (Maybe a) a))))
  (define (from-maybe d m)
    (match m
      [(None)   d]
      [(Some x) x]))

  (define just-five (Some 5))

  ;; Prelude List used recursively
  (: length-of (-> (List a) Integer))
  (define (length-of xs)
    (match xs
      [(Nil)        0]
      [(Cons _ rest) (+ 1 (length-of rest))]))

  (define list-3 (Cons 10 (Cons 20 (Cons 30 Nil))))

  ;; Combinators
  (define same-five ((const 5) "discarded"))
  (define identity-applied (id "x")))

(test-case "Num Integer dispatches via +, -, *"
  (check-equal? a 3)
  (check-equal? b 42))

(test-case "Eq/Ord dispatch"
  (check-true  eq-test)
  (check-true  ord-test)
  (check-true  ord-default-gt)
  (check-true  ord-default-le))

(test-case "Show dispatch"
  (check-equal? show-int  "42")
  (check-equal? show-bool "True"))

(test-case "prelude Maybe is auto-available"
  (check-equal? (from-maybe 0 None) 0)
  (check-equal? (from-maybe 0 (Some 7)) 7)
  (check-equal? (from-maybe 0 just-five) 5))

(test-case "prelude List"
  (check-equal? (length-of Nil) 0)
  (check-equal? (length-of list-3) 3))

(test-case "Combinators"
  (check-equal? same-five 5)
  (check-equal? identity-applied "x"))

#lang rackton

;; Exercises the built-in prelude: Num, Eq, Ord, Show, plus
;; the prelude ADTs (Maybe, List, Pair, Either, Unit) and combinators.
;; User code does NOT redeclare any of these — they're inherited.

(require "../unit.rkt")

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
(define show-list (show (Cons 1 (Cons 2 (Cons 3 Nil)))))
(define show-pair (show (Pair 1 2)))

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
(define identity-applied (id "x"))

(: suite (List Test))
(define suite
  (list
   (it "Num Integer dispatches via +, -, *"
       (all-checks
        (list (check-equal? a 3)
              (check-equal? b 42))))
   (it "Eq/Ord dispatch"
       (all-checks
        (list (check-true  eq-test)
              (check-true  ord-test)
              (check-true  ord-default-gt)
              (check-true  ord-default-le))))
   (it "Show dispatch"
       (all-checks
        (list (check-equal? show-int  "42")
              (check-equal? show-bool "True")
              (check-equal? show-list "[1 2 3]")
              ;; Pair is the binary tuple now, so it shows in tuple form.
              (check-equal? show-pair "(1, 2)"))))
   (it "prelude Maybe is auto-available"
       (all-checks
        (list (check-equal? (from-maybe 0 None) 0)
              (check-equal? (from-maybe 0 (Some 7)) 7)
              (check-equal? (from-maybe 0 just-five) 5))))
   (it "prelude List"
       (all-checks
        (list (check-equal? (length-of Nil) 0)
              (check-equal? (length-of list-3) 3))))
   (it "Combinators"
       (all-checks
        (list (check-equal? same-five 5)
              (check-equal? identity-applied "x"))))))

(: _ran Unit)
(define _ran (run-io (run-suite "prelude builtins" suite)))

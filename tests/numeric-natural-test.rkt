#lang rackton

;; rackton/numeric/natural — Numeric.Natural: a non-negative-Integer
;; newtype with Eq/Ord/Show/Num instances and checked construction.

(require rackton/numeric/natural
         "../unit.rkt")

;; round-trip construction
(: rt Integer) (define rt (num-from-natural (MkNatural 5)))

;; checked construction
(: to-ok-flag  Boolean) (define to-ok-flag  (match (num-to-natural 5)  [(Some _) #t] [(None) #f]))
(: to-ok-val   Integer) (define to-ok-val   (match (num-to-natural 5)  [(Some n) (num-from-natural n)] [(None) -1]))
(: to-neg-none Boolean) (define to-neg-none (match (num-to-natural -1) [(Some _) #f] [(None) #t]))

;; Num: + * - on Natural
(: add Integer) (define add (num-from-natural (+ (MkNatural 3) (MkNatural 4))))
(: mul Integer) (define mul (num-from-natural (* (MkNatural 3) (MkNatural 4))))
(: sub Integer) (define sub (num-from-natural (- (MkNatural 7) (MkNatural 2))))

;; Eq / Ord / Show
(: eq-t Boolean) (define eq-t (== (MkNatural 3) (MkNatural 3)))
(: lt-t Boolean) (define lt-t (< (MkNatural 2) (MkNatural 5)))
(: shown String) (define shown (show (MkNatural 42)))

;; ---------- assertions ---------------------------------------

(: suite (List Test))
(define suite
  (list
   (it "construction"
       (all-checks
        (list (check-equal? rt 5)
              (check-true  to-ok-flag)
              (check-equal? to-ok-val 5)
              (check-true  to-neg-none))))
   (it "Num"
       (all-checks
        (list (check-equal? add 7)
              (check-equal? mul 12)
              (check-equal? sub 5))))
   (it "Eq / Ord / Show"
       (all-checks
        (list (check-true  eq-t)
              (check-true  lt-t)
              (check-equal? shown "42"))))))

(: _ran Unit)
(define _ran (run-io (run-suite "numeric-natural" suite)))

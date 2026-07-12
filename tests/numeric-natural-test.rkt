#lang rackton

;; rackton/numeric/natural — Numeric.Natural: a non-negative-Integer
;; newtype with Eq/Ord/Show/Num instances and checked construction.

(require rackton/numeric/natural
         "../unit.rkt")

;; round-trip construction
(: rt Integer) (define rt (num-from-natural (Natural 5)))

;; checked construction
(: to-ok-flag  Boolean) (define to-ok-flag  (match (num-to-natural 5)  [(Some _) #t] [(None) #f]))
(: to-ok-val   Integer) (define to-ok-val   (match (num-to-natural 5)  [(Some n) (num-from-natural n)] [(None) -1]))
(: to-neg-none Boolean) (define to-neg-none (match (num-to-natural -1) [(Some _) #f] [(None) #t]))

;; Num: + * - on Natural
(: add Integer) (define add (num-from-natural (+ (Natural 3) (Natural 4))))
(: mul Integer) (define mul (num-from-natural (* (Natural 3) (Natural 4))))
(: sub Integer) (define sub (num-from-natural (- (Natural 7) (Natural 2))))

;; Lattice identities resolve at Natural (zero / one are return-typed),
;; and act as neutral elements for + / *.
(: nat-zero Integer) (define nat-zero (num-from-natural (ann zero Natural)))
(: nat-one  Integer) (define nat-one  (num-from-natural (ann one  Natural)))
(: add-id Integer) (define add-id (num-from-natural (+ zero (Natural 9))))
(: mul-id Integer) (define mul-id (num-from-natural (* one  (Natural 9))))
;; A function constrained by Additive-Semigroup accepts Natural (its
;; addition is exact and associative) — exercises that instance.
(: sg-sum ((Additive-Semigroup a) => (-> a (-> a (-> a a)))))
(define (sg-sum x y z) (+ (+ x y) z))
(: nat-sg-sum Integer)
(define nat-sg-sum (num-from-natural (sg-sum (Natural 1) (Natural 2) (Natural 3))))

;; Eq / Ord / Show
(: eq-t Boolean) (define eq-t (== (Natural 3) (Natural 3)))
(: lt-t Boolean) (define lt-t (< (Natural 2) (Natural 5)))
(: shown String) (define shown (show (Natural 42)))

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
    (it "lattice identities and Additive-Semigroup on Natural"
        (all-checks
          (list (check-equal? nat-zero 0)
                (check-equal? nat-one  1)
                (check-equal? add-id 9)
                (check-equal? mul-id 9)
                (check-equal? nat-sg-sum 6))))
    (it "Eq / Ord / Show"
        (all-checks
          (list (check-true  eq-t)
                (check-true  lt-t)
                (check-equal? shown "42"))))))

(: test-main (IO Unit))
(define test-main (run-suite "numeric-natural" suite))

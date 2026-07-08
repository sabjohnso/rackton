#lang rackton

;; Tests for rackton/temporal.  Increment 1: the Later / lob / Signal core.
;; Streams are infinite but productive — every `sig-take` of a finite prefix
;; terminates, including a long one (which also exercises the memoizing
;; `Lazy` behind `Later`).

(require "../unit.rkt"
         "../temporal.rkt")

(: nats (Signal Integer))
(define nats (sig-iterate (lambda (n) (+ n 1)) 0))

(: core-tests Test)
(define core-tests
  (group-of "rackton/temporal — Later / lob / Signal"
            (list
              (it "sig-repeat is constant"
                  (check-equal? (sig-take 3 (sig-repeat 7)) (list 7 7 7)))
              (it "sig-iterate counts up (nats via lob-guarded recursion)"
                  (check-equal? (sig-take 5 nats) (list 0 1 2 3 4)))
              (it "sig-map maps pointwise"
                  (check-equal? (sig-take 4 (sig-map (lambda (n) (* n 2)) nats))
                                (list 0 2 4 6)))
              (it "take of a long prefix terminates (productive + memoized)"
                  (check-equal? (length (sig-take 200 nats)) 200)))))

;; ----- increment 2: sig-zip, a worked stream (Fibonacci), functor laws

;; pointwise sum of two signals: nats + nats = the evens
(: evens (Signal Integer))
(define evens (sig-zip (lambda (a b) (+ a b)) nats nats))

;; Fibonacci via a paired state, iterated then projected — no two-step guard
(: fib-states (Signal (Pair Integer Integer)))
(define fib-states
  (sig-iterate (lambda (p) (match p [(Pair a b) (Pair b (+ a b))])) (Pair 0 1)))
(: fibs (Signal Integer))
(define fibs (sig-map (lambda (p) (match p [(Pair a b) a])) fib-states))

(: id-int (-> Integer Integer)) (define (id-int n) n)
(: f-inc (-> Integer Integer))  (define (f-inc n) (+ n 1))
(: f-dbl (-> Integer Integer))  (define (f-dbl n) (* n 2))

(: built-tests Test)
(define built-tests
  (group-of "rackton/temporal — sig-zip + worked streams"
            (list
              (it "sig-zip combines pointwise: nats + nats = evens"
                  (check-equal? (sig-take 5 evens) (list 0 2 4 6 8)))
              (it "Fibonacci"
                  (check-equal? (sig-take 8 fibs) (list 0 1 1 2 3 5 8 13))))))

(: law-tests Test)
(define law-tests
  (group-of "rackton/temporal — Signal functor laws"
            (list
              (it "identity: sig-map id s = s"
                  (check-equal? (sig-take 6 (sig-map id-int nats)) (sig-take 6 nats)))
              (it "composition: sig-map (g.f) = sig-map g . sig-map f"
                  (check-equal?
                    (sig-take 6 (sig-map (lambda (n) (f-dbl (f-inc n))) nats))
                    (sig-take 6 (sig-map f-dbl (sig-map f-inc nats))))))))

(: suite Test)
(define suite (group-of "rackton/temporal" (list core-tests built-tests law-tests)))

(: test-main (IO Unit))
(define test-main (run-suite-tree suite))

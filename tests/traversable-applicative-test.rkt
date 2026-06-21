#lang rackton

;; rackton/data/traversable + rackton/control/applicative.

(require rackton/data/traversable
         rackton/control/applicative
         "../unit.rkt")

;; sequence-a over List of Maybe
(: seq-ok  (Maybe (List Integer)))
(define seq-ok  (sequence-a (list (Some 1) (Some 2) (Some 3))))
(: seq-bad (Maybe (List Integer)))
(define seq-bad (sequence-a (list (Some 1) None (Some 3))))

;; for-t over List with a Maybe action
(: fort (Maybe (List Integer)))
(define fort (for-t (list 1 2 3) (lambda (x) (if (> x 0) (Some (* x 2)) None))))

;; lift-a3 over Maybe
(: l3-ok  (Maybe Integer))
(define l3-ok  (lift-a3 (lambda (a b c) (+ a (+ b c))) (Some 1) (Some 2) (Some 3)))
(: l3-bad (Maybe Integer))
(define l3-bad (lift-a3 (lambda (a b c) (+ a (+ b c))) (Some 1) None (Some 3)))

(: suite (List Test))
(define suite
  (list
   (it "sequence-a / for-t"
       (all-checks
        (list (check-equal? seq-ok  (Some (list 1 2 3)))
              (check-equal? seq-bad None)
              (check-equal? fort    (Some (list 2 4 6))))))
   (it "lift-a3"
       (all-checks
        (list (check-equal? l3-ok  (Some 6))
              (check-equal? l3-bad None))))))

(: main Unit)
(define main (run-io (run-suite "rackton/data/traversable + applicative" suite)))

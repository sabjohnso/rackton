#lang rackton

;; rackton/data/coerce — a general (lawless, From/Into-style) coercion
;; protocol.  `coerce` dispatches on the source type; the target is
;; resolved from the expected type.  These pin the shipped instances.

(require rackton/data/coerce
         rackton/numeric/natural
         "../unit.rkt")

;; Integer -> Float (the expected type fixes the target).
(: i2f Float)
(define i2f (coerce 3))

;; Rational -> Float.
(: r2f Float)
(define r2f (coerce (make-rational 1 2)))

;; Integer -> Rational.
(: i2r Rational)
(define i2r (coerce 5))

;; Float -> Integer (truncates toward zero).
(: f2i Integer)
(define f2i (coerce 3.9))

;; Natural -> Integer (total; the reverse is deliberately absent).
(: n2i Integer)
(define n2i (coerce (Natural 7)))

(: suite (List Test))
(define suite
  (list
    (it "numeric-tower coercions"
        (all-checks
          (list (check-true  (< (abs-float (- i2f 3.0)) 0.000001))
                (check-true  (< (abs-float (- r2f 0.5)) 0.000001))
                (check-equal? i2r (make-rational 5 1))
                (check-equal? f2i 3)
                (check-equal? n2i 7))))))

(: test-main (IO Unit))
(define test-main (run-suite "coerce" suite))

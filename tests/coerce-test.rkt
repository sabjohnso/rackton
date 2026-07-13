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

;; ----- property generators -----------------------------------------
;; Bounded well within 2^53, so Integer <-> Float is exact over it.
(: gi (Gen Integer))
(define gi (int-range -100000 100000))

;; Floats a/b (b forced non-zero).  Most draws are genuinely fractional
;; (so truncation drops a part), though b | a gives an integer-valued
;; float; the properties below hold either way.
(: gf-frac (Gen Float))
(define gf-frac
  (fmap (lambda (p)
          (match p [(Pair a b)
            (float-div (integer->float a)
                       (integer->float (if (== b 0) 1 b)))]))
        (gen-pair gi gi)))

(: suite (List Test))
(define suite
  (list
    (it "numeric-tower coercions"
        (all-checks
          (list (check-true  (< (abs-float (- i2f 3.0)) 0.000001))
                (check-true  (< (abs-float (- r2f 0.5)) 0.000001))
                (check-equal? i2r (make-rational 5 1))
                (check-equal? f2i 3)
                (check-equal? n2i 7))))
    ;; Integer -> Rational is exact and injective: every n maps to n/1.
    (it-prop "Integer->Rational is exact (n maps to n/1)"
             (for-all gi
                      (lambda (n)
                        (and (== (numerator   (ann (coerce n) Rational)) n)
                             (== (denominator (ann (coerce n) Rational)) 1)))))
    ;; Integer -> Float is exact within +-2^53, so Float -> Integer
    ;; round-trips it back unchanged.
    (it-prop "Integer->Float->Integer round-trips within range"
             (for-all gi
                      (lambda (n)
                        (== (ann (coerce (ann (coerce n) Float)) Integer) n))))
    ;; Float -> Integer truncates toward zero.  Two independent
    ;; discriminators pin it against the other roundings: it is symmetric
    ;; under negation (which floor / ceiling are not), and it never
    ;; increases magnitude (which round-to-nearest and round-away-from-
    ;; zero do).
    (it-prop "Float->Integer truncates toward zero"
             (for-all gf-frac
                      (lambda (x)
                        (and (== (ann (coerce (negate x)) Integer)
                                 (negate (ann (coerce x) Integer)))
                             (<= (integer->float (abs (ann (coerce x) Integer)))
                                 (abs-float x))))))))

(: test-main (IO Unit))
(define test-main (run-suite "coerce" suite))

#lang racket/base

;; rackton/data/ratio — approx-rational: the simplest Rational within a
;; given tolerance of a Float (Numeric.approxRational).

(require rackunit
         "../main.rkt")

(rackton
  (require rackton/data/ratio)

  ;; exact-ish simple fractions
  (: half-n  Integer) (define half-n  (numerator   (approx-rational 0.5 0.001)))
  (: half-d  Integer) (define half-d  (denominator (approx-rational 0.5 0.001)))
  (: third-n Integer) (define third-n (numerator   (approx-rational 0.3333333 0.001)))
  (: third-d Integer) (define third-d (denominator (approx-rational 0.3333333 0.001)))

  ;; negative input
  (: neg-n Integer) (define neg-n (numerator   (approx-rational -0.25 0.0001)))
  (: neg-d Integer) (define neg-d (denominator (approx-rational -0.25 0.0001)))

  ;; within tolerance + simple denominator for a harder value
  (: pi-approx Float)   (define pi-approx (to-float     (approx-rational 3.14159 0.01)))
  (: pi-den    Integer) (define pi-den    (denominator  (approx-rational 3.14159 0.01))))

;; ---------- assertions ---------------------------------------

(test-case "simplest within tolerance"
  (check-equal? half-n 1)  (check-equal? half-d 2)
  (check-equal? third-n 1) (check-equal? third-d 3))

(test-case "negative input"
  (check-equal? neg-n -1)
  (check-equal? neg-d 4))

(test-case "harder value stays within eps with a small denominator"
  (check-= pi-approx 3.14159 0.01)
  (check-true (<= pi-den 50)))

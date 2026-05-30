#lang rackton

;; rackton/data/ratio — Data.Ratio.  The prelude ships the
;; @racket[Rational] type with @racket[make-rational] / @racket[numerator]
;; / @racket[denominator] (and rationals are kept in lowest terms by the
;; runtime).  These are the derived operations.

(provide (all-defined-out))

;; (ratio n d) — alias for make-rational (Haskell `%`, named not infix).
(: ratio (-> Integer (-> Integer Rational)))
(define (ratio n d) (make-rational n d))

;; multiplicative inverse: d/n for n/d.
(: recip (-> Rational Rational))
(define (recip r) (make-rational (denominator r) (numerator r)))

;; convert to the nearest Float.
(: to-float (-> Rational Float))
(define (to-float r)
  (float-div (integer->float (numerator r)) (integer->float (denominator r))))

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

;; --- approxRational ------------------------------------------------
;;
;; (approx-rational x eps) is the simplest Rational within `eps` of `x`
;; — the one with the smallest denominator (then smallest numerator).
;; This is GHC's algorithm: search the open interval (x-eps, x+eps) by
;; the Stern-Brocot mediant, recursing on the continued-fraction
;; quotients.  `approx-simplest` / `approx-simplest-prime` are its
;; helpers (top-down: the intent first, the recursion below).

(: approx-rational (-> Float (-> Float Rational)))
(define (approx-rational x eps)
  (approx-simplest (to-rational (- x eps)) (to-rational (+ x eps))))

;; The simplest Rational in the closed interval [x, y].
(: approx-simplest (-> Rational (-> Rational Rational)))
(define (approx-simplest x y)
  (cond
    [(< y x)              (approx-simplest y x)]
    [(== x y)             x]
    [(> (numerator x) 0)  (approx-simplest-prime (numerator x) (denominator x)
                                                 (numerator y) (denominator y))]
    [(< (numerator y) 0)  (negate (approx-simplest-prime (negate (numerator y)) (denominator y)
                                                         (negate (numerator x)) (denominator x)))]
    [else                 (make-rational 0 1)]))

;; Assumes 0 < n/d < n2/d2; returns the simplest Rational strictly
;; between them, via the continued-fraction quotients of the endpoints.
(: approx-simplest-prime (-> Integer (-> Integer (-> Integer (-> Integer Rational)))))
(define (approx-simplest-prime n d n2 d2)
  (let ([q (quot n d)]
        [r (rem n d)])
    (cond
      [(== r 0)              (make-rational q 1)]
      [(/= q (quot n2 d2))   (make-rational (+ q 1) 1)]
      [else
       (let ([nd (approx-simplest-prime d2 (rem n2 d2) d r)])
         (make-rational (+ (* q (numerator nd)) (denominator nd))
                        (numerator nd)))])))

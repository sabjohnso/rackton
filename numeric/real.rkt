#lang rackton

;; rackton/numeric/real — Floating / RealFrac operations the prelude
;; doesn't already ship.  The prelude's Floating class supplies
;; @racket[pi] / @racket[exp] / @racket[log] / @racket[sqrt] /
;; @racket[sin] / @racket[cos] / @racket[tan] / @racket[**], and its
;; RealFrac class supplies @racket[truncate-real] (truncates toward
;; zero); these are the derived combinators.
;;
;; The inverse trig functions go through @racket[(racket …)] escapes
;; to racket/base's @racket[asin] / @racket[acos] / @racket[atan].  The
;; hyperbolic functions are derived from @racket[exp] rather than
;; escaping to racket/math (which isn't in the escape scope) — this
;; keeps the module dependency-free and expresses the defining algebra.

(provide (all-defined-out))

;; --- inverse trig (host primitives) --------------------------------

(: num-asin (-> Float Float))
(define (num-asin x) (racket Float (x) (asin x)))

(: num-acos (-> Float Float))
(define (num-acos x) (racket Float (x) (acos x)))

(: num-atan (-> Float Float))
(define (num-atan x) (racket Float (x) (atan x)))

;; --- hyperbolic trig (derived from exp) ----------------------------
;; sinh x = (eˣ − e⁻ˣ)/2,  cosh x = (eˣ + e⁻ˣ)/2,  tanh x = sinh/cosh.

(: num-sinh (-> Float Float))
(define (num-sinh x) (float-div (- (exp x) (exp (negate x))) 2.0))

(: num-cosh (-> Float Float))
(define (num-cosh x) (float-div (+ (exp x) (exp (negate x))) 2.0))

(: num-tanh (-> Float Float))
(define (num-tanh x) (float-div (num-sinh x) (num-cosh x)))

;; --- logarithms ----------------------------------------------------
;; logBase b x = log x / log b.

(: num-log-base (-> Float (-> Float Float)))
(define (num-log-base b x) (float-div (log x) (log b)))

;; --- RealFrac ------------------------------------------------------
;; properFraction x = (n, f) where n truncates x toward zero and
;; f = x − n is the fractional remainder (same sign as x).

(: num-proper-fraction (-> Float (Pair Integer Float)))
(define (num-proper-fraction x)
  (let ([n (truncate-real x)])
    (MkPair n (- x (integer->float n)))))

#lang rackton

;; rackton/numeric/conversions — conversions across the numeric tower.
;; The prelude ships the primitive coercions (@racket[integer->float],
;; @racket[float->integer] which truncates toward zero, and the
;; @racket[Real] method @racket[to-rational]); this module gives them a
;; uniform @racket[num-]-prefixed face, adds @racket[Rational] ->
;; @racket[Float], and the polymorphic @racket[realToFrac] (any
;; @racket[Real] to @racket[Float], the way Haskell uses it most).

(provide (all-defined-out))

;; --- primitive coercions (prelude faces) ---------------------------

(: num-integer->float (-> Integer Float))
(define (num-integer->float n) (integer->float n))

(: num-float->integer (-> Float Integer))
(define (num-float->integer x) (float->integer x))

(: num-to-rational ((Real a) => (-> a Rational)))
(define (num-to-rational x) (to-rational x))

;; --- Rational -> Float ---------------------------------------------

(: num-rational->float (-> Rational Float))
(define (num-rational->float r) (racket Float (r) (exact->inexact r)))

;; --- realToFrac: any Real to Float ---------------------------------
;; Routes through the Rational bridge: to-rational then exact->inexact.

(: num-real-to-frac ((Real a) => (-> a Float)))
(define (num-real-to-frac x) (num-rational->float (to-rational x)))

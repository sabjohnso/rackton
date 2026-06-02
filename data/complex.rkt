#lang rackton

;; rackton/data/complex — Data.Complex.  The prelude ships the
;; @racket[Complex] type with @racket[make-complex] / @racket[real-part]
;; / @racket[imag-part] / @racket[magnitude]; these are the derived
;; operations.  (Named @racket[mk-polar] / @racket[phase] rather than
;; racket/base's @racket[make-polar] / @racket[angle] so those stay
;; usable in @racket[(racket …)] escapes.)

(provide (all-defined-out))

;; complex conjugate: negate the imaginary part.
(: conjugate (-> Complex Complex))
(define (conjugate z) (make-complex (real-part z) (negate (imag-part z))))

;; phase angle in radians (Haskell `phase`).
(: phase (-> Complex Float))
(define (phase z) (atan2 (imag-part z) (real-part z)))

;; build from polar coordinates.
(: mk-polar (-> Float (-> Float Complex)))
(define (mk-polar r theta)
  (make-complex (* r (cos theta)) (* r (sin theta))))

;; unit complex at the given angle: cos θ + i sin θ.
(: cis (-> Float Complex))
(define (cis theta) (mk-polar 1.0 theta))

;; (magnitude, phase) pair.
(: polar (-> Complex (Pair Float Float)))
(define (polar z) (Pair (magnitude z) (phase z)))

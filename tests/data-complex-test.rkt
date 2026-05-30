#lang racket/base

;; rackton/data/complex — derived complex operations.  Chosen at angle 0
;; / the 3-4-5 triangle so the Float results are exact.

(require rackunit
         "../main.rkt")

(rackton
  (require rackton/data/complex)

  (: z Complex) (define z (make-complex 3.0 4.0))

  (: conj-re Float) (define conj-re (real-part (conjugate z)))
  (: conj-im Float) (define conj-im (imag-part (conjugate z)))
  (: mag Float)     (define mag (magnitude z))
  (: ph0 Float)     (define ph0 (phase (make-complex 1.0 0.0)))

  (: mp-re Float) (define mp-re (real-part (mk-polar 5.0 0.0)))
  (: mp-im Float) (define mp-im (imag-part (mk-polar 5.0 0.0)))
  (: cis-re Float)(define cis-re (real-part (cis 0.0)))
  (: cis-im Float)(define cis-im (imag-part (cis 0.0)))

  (: pol-mag Float) (define pol-mag (fst (polar z)))
  (: pol-ph  Float) (define pol-ph  (snd (polar (make-complex 1.0 0.0)))))

;; ---------- assertions ---------------------------------------

(test-case "conjugate / magnitude / phase"
  (check-equal? conj-re 3.0)
  (check-equal? conj-im -4.0)
  (check-equal? mag 5.0)
  (check-equal? ph0 0.0))

(test-case "mk-polar / cis"
  (check-equal? mp-re 5.0) (check-equal? mp-im 0.0)
  (check-equal? cis-re 1.0) (check-equal? cis-im 0.0))

(test-case "polar"
  (check-equal? pol-mag 5.0)
  (check-equal? pol-ph 0.0))

#lang racket/base

;; rackton/numeric/conversions — conversions across the numeric tower:
;; Integer <-> Float, to-rational, rational->float, realToFrac.

(require rackunit
         "../main.rkt")

(rackton
  (require rackton/numeric/conversions)

  (: i2f   Float)   (define i2f   (num-integer->float 3))
  (: f2i   Integer) (define f2i   (num-float->integer 3.9))
  (: f2i-n Integer) (define f2i-n (num-float->integer -3.9))

  (: tr-ok Boolean) (define tr-ok (== (num-to-rational 5) (make-rational 5 1)))

  (: r2f   Float)   (define r2f   (num-rational->float (make-rational 1 2)))

  ;; realToFrac over different Real instances
  (: rtf-rat Float) (define rtf-rat (num-real-to-frac (make-rational 3 4)))
  (: rtf-int Float) (define rtf-int (num-real-to-frac 2))
  (: rtf-flt Float) (define rtf-flt (num-real-to-frac 1.5)))

;; ---------- assertions ---------------------------------------

(test-case "Integer <-> Float"
  (check-= i2f 3.0 1e-9)
  (check-equal? f2i 3)
  (check-equal? f2i-n -3))

(test-case "to-rational"
  (check-true tr-ok))

(test-case "rational -> float"
  (check-= r2f 0.5 1e-9))

(test-case "realToFrac"
  (check-= rtf-rat 0.75 1e-9)
  (check-= rtf-int 2.0 1e-9)
  (check-= rtf-flt 1.5 1e-9))

#lang rackton

;; rackton/numeric/conversions — conversions across the numeric tower:
;; Integer <-> Float, to-rational, rational->float, realToFrac.

(require rackton/numeric/conversions
         "../unit.rkt")

(: i2f   Float)   (define i2f   (num-integer->float 3))
(: f2i   Integer) (define f2i   (num-float->integer 3.9))
(: f2i-n Integer) (define f2i-n (num-float->integer -3.9))

(: tr-ok Boolean) (define tr-ok (== (num-to-rational 5) (make-rational 5 1)))

(: r2f   Float)   (define r2f   (num-rational->float (make-rational 1 2)))

;; realToFrac over different Real instances
(: rtf-rat Float) (define rtf-rat (num-real-to-frac (make-rational 3 4)))
(: rtf-int Float) (define rtf-int (num-real-to-frac 2))
(: rtf-flt Float) (define rtf-flt (num-real-to-frac 1.5))

;; ---------- assertions ---------------------------------------

(: suite (List Test))
(define suite
  (list
   (it "Integer <-> Float"
       (all-checks
        (list (check-true (< (abs (- i2f 3.0)) 1e-9))
              (check-equal? f2i 3)
              (check-equal? f2i-n -3))))
   (it "to-rational"
       (check-true tr-ok))
   (it "rational -> float"
       (check-true (< (abs (- r2f 0.5)) 1e-9)))
   (it "realToFrac"
       (all-checks
        (list (check-true (< (abs (- rtf-rat 0.75)) 1e-9))
              (check-true (< (abs (- rtf-int 2.0)) 1e-9))
              (check-true (< (abs (- rtf-flt 1.5)) 1e-9)))))))

(: _ran Unit)
(define _ran (run-io (run-suite "numeric-conversions" suite)))

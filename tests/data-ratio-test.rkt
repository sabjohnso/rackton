#lang rackton

;; rackton/data/ratio — rational numbers reduced to lowest terms.

(require rackton/data/ratio
         "../unit.rkt")

(: rn Integer) (define rn (numerator   (ratio 2 4)))
(: rd Integer) (define rd (denominator (ratio 2 4)))
(: cn Integer) (define cn (numerator   (recip (ratio 2 3))))
(: cd Integer) (define cd (denominator (recip (ratio 2 3))))
(: tf Float)   (define tf (to-float (ratio 1 2)))

(: suite (List Test))
(define suite
  (list
    (it "data/ratio"
        (all-checks
          (list (check-equal? rn 1) (check-equal? rd 2)
                (check-equal? cn 3) (check-equal? cd 2)
                (check-equal? tf 0.5))))))

(: test-main (IO Unit))
(define test-main (run-suite "rackton/data/ratio" suite))

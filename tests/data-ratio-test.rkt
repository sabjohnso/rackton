#lang racket/base
(require rackunit "../main.rkt")

(rackton
  (require rackton/data/ratio)
  (: rn Integer) (define rn (numerator   (ratio 2 4)))
  (: rd Integer) (define rd (denominator (ratio 2 4)))
  (: cn Integer) (define cn (numerator   (recip (ratio 2 3))))
  (: cd Integer) (define cd (denominator (recip (ratio 2 3))))
  (: tf Float)   (define tf (to-float (ratio 1 2))))

(test-case "data/ratio"
  (check-equal? rn 1) (check-equal? rd 2)     ; 2/4 reduced to 1/2
  (check-equal? cn 3) (check-equal? cd 2)     ; recip 2/3 = 3/2
  (check-equal? tf 0.5))

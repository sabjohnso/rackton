#lang rackton

;; Enabler B regression: the shared instance-defining module (the apex
;; of the diamond).

(provide (all-defined-out))

(data Color Red Green)

(instance (Eq Color)
  (define (== x y)
    (match x
      [(Red)   (match y [(Red) #t] [(Green) #f])]
      [(Green) (match y [(Red) #f] [(Green) #t])])))

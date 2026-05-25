#lang rackton

;; A DIFFERENT module that ALSO declares Eq Color.  Importing both
;; sealed-abstract-types-lib-eq-a and sealed-abstract-types-lib-eq-b
;; should be a compile-time coherence error.

(define-data Color  Red  Green  Blue)

(define-instance (Eq Color)
  (define (== a b) #t))

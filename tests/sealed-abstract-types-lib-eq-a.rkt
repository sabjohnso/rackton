#lang rackton

;; Declares an Eq instance for a concrete type, to be combined with
;; sealed-abstract-types-lib-eq-b which redeclares it — importing
;; both should be rejected as a coherence violation.

(provide (all-defined-out))

(define-data Color  Red  Green  Blue)

(instance (Eq Color)
  (define (== a b)
    (match a
      [Red   (match b [Red   #t] [_  #f])]
      [Green (match b [Green #t] [_  #f])]
      [Blue  (match b [Blue  #t] [_  #f])])))

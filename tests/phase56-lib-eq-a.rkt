#lang rackton

;; Phase 56: declares an Eq instance for a concrete type, to be
;; combined with phase56-lib-eq-b which redeclares it — importing
;; both should be rejected as a coherence violation.

(define-data Color  Red  Green  Blue)

(define-instance (Eq Color)
  (define (== a b)
    (match a
      [Red   (match b [Red   #t] [_  #f])]
      [Green (match b [Green #t] [_  #f])]
      [Blue  (match b [Blue  #t] [_  #f])])))

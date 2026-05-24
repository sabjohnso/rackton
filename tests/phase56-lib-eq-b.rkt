#lang rackton

;; Phase 56: a DIFFERENT module that ALSO declares Eq Color.
;; Importing both phase56-lib-eq-a and phase56-lib-eq-b should be
;; a compile-time coherence error.

(define-data Color  Red  Green  Blue)

(define-instance (Eq Color)
  (define (== a b) #t))

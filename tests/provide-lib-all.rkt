#lang rackton

;; (all-defined-out) — both `foo` and `bar` escape.

(define foo 1)
(define bar 2)

(provide (all-defined-out))

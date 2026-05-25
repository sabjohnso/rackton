#lang rackton

;; (except-out (all-defined-out) name ...) — everything defined here
;; escapes except `internal-helper`.

(define foo 1)
(define bar 2)
(define internal-helper 99)

(provide (except-out (all-defined-out) internal-helper))

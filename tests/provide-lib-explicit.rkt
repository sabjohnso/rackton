#lang rackton

;; Explicit single-name (provide ...) — only `foo` should escape.

(define foo 1)
(define bar 2)

(provide foo)

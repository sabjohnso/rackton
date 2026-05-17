#lang racket/base

;; The Container class and its instance for Stack live in
;; multi-file-classes-lib.rkt.  We import both and use them without
;; redeclaring anything.

(require rackunit
         "../main.rkt")

(rackton
  (require "multi-file-classes-lib.rkt")

  (define stack-of-three
    (Push 1 (Push 2 (Push 3 Empty))))

  (: size-of-stack Integer)
  (define size-of-stack (size stack-of-three))

  (: stack-empty? Boolean)
  (define stack-empty? (empty? stack-of-three))

  (: empty-empty? Boolean)
  (define empty-empty? (empty? Empty)))

(test-case "imported instance dispatches correctly"
  (check-equal? size-of-stack 3)
  (check-false  stack-empty?)
  (check-true   empty-empty?))

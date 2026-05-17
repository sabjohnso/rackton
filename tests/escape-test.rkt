#lang racket/base

;; Tests for the (racket τ (vars) body) host-language escape.

(require rackunit
         racket/port
         "../main.rkt")

(rackton
  (define x (racket Integer () 42))
  (define (double n) (racket Integer (n) (* n 2)))
  (define (greet name)
    (racket String (name)
      (string-append "hello " name))))

(test-case "constant escape"
  (check-equal? x 42))

(test-case "escape uses an inbound rackton variable"
  (check-equal? (double 21) 42)
  (check-equal? (double -3) -6))

(test-case "escape returns a String"
  (check-equal? (greet "world") "hello world"))

#lang racket/base

;; Records via `define-struct`: single-ctor named-field types with
;; auto-generated accessors.

(require rackunit
         "../main.rkt")

(rackton
  (define-struct Point
    [x : Integer]
    [y : Integer])

  (define origin (Point 0 0))
  (define p1     (Point 3 4))

  (define p1-x (Point-x p1))
  (define p1-y (Point-y p1))

  (: distance-squared (-> Point Integer))
  (define (distance-squared p)
    (+ (* (Point-x p) (Point-x p))
       (* (Point-y p) (Point-y p))))

  (define d-origin (distance-squared origin))
  (define d-p1     (distance-squared p1))

  ;; Parametric struct
  (define-struct (Box a)
    [value : a]
    [label : String])

  (define b (Box 42 "answer"))
  (define b-value (Box-value b))
  (define b-label (Box-label b)))

(test-case "non-parametric record"
  (check-equal? p1-x 3)
  (check-equal? p1-y 4)
  (check-equal? d-origin 0)
  (check-equal? d-p1 25))

(test-case "parametric record"
  (check-equal? b-value 42)
  (check-equal? b-label "answer"))

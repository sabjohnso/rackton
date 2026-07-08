#lang rackton

;; Records via `struct`: single-ctor named-field types with
;; auto-generated accessors.

(require "../unit.rkt")

(struct Point
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
(struct (Box a)
  [value : a]
  [label : String])

(define b (Box 42 "answer"))
(define b-value (Box-value b))
(define b-label (Box-label b))

(: suite (List Test))
(define suite
  (list
    (it "non-parametric record"
        (all-checks
          (list (check-equal? p1-x 3)
                (check-equal? p1-y 4)
                (check-equal? d-origin 0)
                (check-equal? d-p1 25))))
    (it "parametric record"
        (all-checks
          (list (check-equal? b-value 42)
                (check-equal? b-label "answer"))))))

(: test-main (IO Unit))
(define test-main (run-suite "records" suite))

#lang racket/base

;; End-to-end tests for type-class support.

(require rackunit
         "../main.rkt")

(rackton
  (protocol (Eq a)
    (: == (-> a (-> a Boolean))))

  (instance (Eq Integer)
    (define (== x y) (= x y)))

  (instance (Eq Boolean)
    (define (== x y)
      (if x y (if y #f #t))))

  (data (Maybe a) None (Some a))

  (instance ((Eq a) => (Eq (Maybe a)))
    (define (== x y)
      (match x
        [(None)
         (match y
           [(None)   #t]
           [(Some _) #f])]
        [(Some xv)
         (match y
           [(None)   #f]
           [(Some yv) (== xv yv)])])))

  (: contains? ((Eq a) => (-> a (-> (Maybe a) Boolean))))
  (define (contains? target m)
    (match m
      [(None)   #f]
      [(Some x) (== x target)])))

;; ----- value-level checks -----

(test-case "Eq Integer"
  (check-true  (== 1 1))
  (check-false (== 1 2)))

(test-case "Eq Boolean"
  (check-true  (== #t #t))
  (check-true  (== #f #f))
  (check-false (== #t #f)))

(test-case "Eq (Maybe Integer) via instance with context"
  (check-true  (== None None))
  (check-false (== None (Some 1)))
  (check-true  (== (Some 1) (Some 1)))
  (check-false (== (Some 1) (Some 2))))

(test-case "polymorphic constrained function dispatches at runtime"
  (check-true  (contains? 1 (Some 1)))
  (check-false (contains? 1 (Some 2)))
  (check-false (contains? 1 None)))

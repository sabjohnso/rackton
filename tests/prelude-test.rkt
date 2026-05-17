#lang racket/base

;; Phase-2 prelude exercise: Eq with a default neq, Ord as a subclass of Eq,
;; instances over Integer and Maybe.  Class methods use distinct names
;; (`eq`, `neq`, `lt`, `gt`) so they don't shadow the builtin operators
;; the instances need to call.

(require rackunit
         "../main.rkt")

(rackton
  (define-class (Eq a)
    (: eq  (-> a (-> a Boolean)))
    (: neq (-> a (-> a Boolean)))
    (define (neq x y)
      (if (eq x y) #f #t)))

  (define-class ((Eq a) => (Ord a))
    (: lt (-> a (-> a Boolean)))
    (: gt (-> a (-> a Boolean)))
    (define (gt x y) (lt y x)))

  (define-instance (Eq Integer)
    (define (eq x y) (= x y)))

  (define-instance (Ord Integer)
    (define (lt x y) (< x y)))

  (define-data (Maybe a) None (Some a))

  (define-instance ((Eq a) => (Eq (Maybe a)))
    (define (eq x y)
      (match x
        [(None)
         (match y [(None) #t] [(Some _) #f])]
        [(Some xv)
         (match y [(None) #f] [(Some yv) (eq xv yv)])]))))

(test-case "default neq dispatches via eq"
  (check-true  (neq 1 2))
  (check-false (neq 1 1)))

(test-case "Ord gt via default that calls lt"
  (check-true  (gt 3 1))
  (check-false (gt 1 3)))

(test-case "Eq carries through ADT"
  (check-true  (eq None None))
  (check-false (eq None (Some 1)))
  (check-true  (neq (Some 1) (Some 2)))
  (check-false (neq (Some 1) (Some 1))))

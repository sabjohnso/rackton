#lang rackton

(define (id x) x)

(define (fact n)
  (if (== n 0) 1 (* n (fact (- n 1)))))

(define-data (Maybe a) None (Some a))

(: from-maybe (-> a (-> (Maybe a) a)))
(define (from-maybe d m)
  (match m
    [(None)   d]
    [(Some x) x]))

#lang racket/base

;; End-to-end tests: parse → infer → codegen → execute.

(require rackunit
         "../main.rkt")

(rackton
  (define (id x) x)

  (: const (-> a (-> b a)))
  (define (const x) (lambda (_y) x))

  (: compose (-> (-> b c) (-> (-> a b) (-> a c))))
  (define (compose f) (lambda (g) (lambda (x) (f (g x)))))

  (define (fact n)
    (if (= n 0) 1 (* n (fact (- n 1)))))

  (define-data (Maybe a) None (Some a))

  (: map-maybe (-> (-> a b) (-> (Maybe a) (Maybe b))))
  (define (map-maybe f m)
    (match m
      [(None)   None]
      [(Some x) (Some (f x))]))

  (define-data (Pair a b) (MkPair a b))

  (: swap (-> (Pair a b) (Pair b a)))
  (define (swap p)
    (match p [(MkPair x y) (MkPair y x)]))

  (define-data Color Red Green Blue)

  (: color-code (-> Color Integer))
  (define (color-code c)
    (match c
      [Red   0]
      [Green 1]
      [Blue  2])))

;; ----- value-level checks -----

(test-case "identity"
  (check-equal? (id 1) 1)
  (check-equal? (id "x") "x")
  (check-equal? (id (Some 7)) (Some 7)))

(test-case "const"
  (check-equal? ((const 5) 99) 5)
  (check-equal? ((const "hi") 'whatever) "hi"))

(test-case "compose"
  (define inc (lambda (n) (+ n 1)))
  (define dbl (lambda (n) (* n 2)))
  (check-equal? (((compose inc) dbl) 3) 7)
  (check-equal? (((compose dbl) inc) 3) 8))

(test-case "fact"
  (check-equal? (fact 0) 1)
  (check-equal? (fact 1) 1)
  (check-equal? (fact 5) 120)
  (check-equal? (fact 7) 5040))

(test-case "maybe"
  (check-equal? (map-maybe id None) None)
  (check-equal? (map-maybe (lambda (n) (+ n 1)) (Some 4))
                (Some 5)))

(test-case "pair"
  (check-equal? (swap (MkPair 1 "x")) (MkPair "x" 1)))

(test-case "enum-like"
  (check-equal? (color-code Red)   0)
  (check-equal? (color-code Green) 1)
  (check-equal? (color-code Blue)  2))

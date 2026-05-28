#lang racket/base

;; End-to-end tests for higher-kinded type classes:
;; Functor and Monad with instances for Maybe, List, and Result e.

(require rackunit
         "../main.rkt")

(rackton
  ;; A polymorphic function that uses fmap.  Its scheme picks up a
  ;; (Functor f) constraint automatically.
  (: square-all ((Functor f) => (-> (f Integer) (f Integer))))
  (define (square-all xs)
    (fmap (lambda (n) (* n n)) xs))

  ;; A bind chain over Maybe.
  (: add-one-then-double (-> Integer (Maybe Integer)))
  (define (add-one-then-double n)
    (flatmap (lambda (m) (Some (* m 2)))
             (Some (+ n 1))))

  ;; Result-typed plumbing.
  (: safe-divide (-> Integer (-> Integer (Result String Integer))))
  (define (safe-divide x y)
    (if (== y 0)
        (Err "divide by zero")
        (Ok (racket Integer (x y) (quotient x y))))))

;; ----- value-level checks -----

(test-case "fmap over Maybe"
  (check-equal? (square-all (Some 3)) (Some 9))
  (check-equal? (square-all None)     None))

(test-case "fmap over List"
  (check-equal? (square-all (Cons 1 (Cons 2 (Cons 3 Nil))))
                (Cons 1 (Cons 4 (Cons 9 Nil))))
  (check-equal? (square-all Nil) Nil))

(test-case "fmap over Result e"
  (check-equal? (square-all (Ok 4))       (Ok 16))
  (check-equal? (square-all (Err "bad"))  (Err "bad")))

(test-case "Monad Maybe — bind chain"
  (check-equal? (add-one-then-double 4)  (Some 10))
  (check-equal? (add-one-then-double 10) (Some 22)))

(test-case "Monad Result — bind chain composes errors"
  (define (div a b)
    (flatmap (lambda (q) (safe-divide q 1))
             (safe-divide a b)))
  (check-equal? (div 10 2) (Ok 5))
  (check-equal? (div 10 0) (Err "divide by zero")))

#lang rackton

;; End-to-end tests for higher-kinded type classes:
;; Functor and Monad with instances for Maybe, List, and Either e.

(require "../unit.rkt")

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

;; Either-typed plumbing.
(: safe-divide (-> Integer (-> Integer (Either String Integer))))
(define (safe-divide x y)
  (if (== y 0)
    (Left "divide by zero")
    (Right (racket Integer (x y) (quotient x y)))))

;; Bind chain composing Either errors.
(: div-chain (-> Integer (-> Integer (Either String Integer))))
(define (div-chain a b)
  (flatmap (lambda (q) (safe-divide q 1))
           (safe-divide a b)))

;; ----- value-level checks -----

(: suite (List Test))
(define suite
  (list
    (it "fmap over Maybe"
        (all-checks
          (list (check-equal? (square-all (Some 3)) (Some 9))
                (check-equal? (square-all None)     None))))
    (it "fmap over List"
        (all-checks
          (list (check-equal? (square-all (Cons 1 (Cons 2 (Cons 3 Nil))))
                              (Cons 1 (Cons 4 (Cons 9 Nil))))
                (check-equal? (square-all Nil) Nil))))
    (it "fmap over Either e"
        (all-checks
          (list (check-equal? (square-all (ann (Right 4) (Either String Integer)))
                              (ann (Right 16) (Either String Integer)))
                (check-equal? (square-all (ann (Left "bad") (Either String Integer)))
                              (ann (Left "bad") (Either String Integer))))))
    (it "Monad Maybe — bind chain"
        (all-checks
          (list (check-equal? (add-one-then-double 4)  (Some 10))
                (check-equal? (add-one-then-double 10) (Some 22)))))
    (it "Monad Either — bind chain composes errors"
        (all-checks
          (list (check-equal? (div-chain 10 2) (Right 5))
                (check-equal? (div-chain 10 0) (Left "divide by zero")))))))

(: test-main (IO Unit))
(define test-main (run-suite "hkt" suite))

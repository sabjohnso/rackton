#lang rackton

;; Partial application via case-lambda compilation.

(require "../unit.rkt")

;; ----- Partial application of an operator -----------------
(: inc (-> Integer Integer))
(define inc (+ 1))

(: inc-result Integer)
(define inc-result (inc 41))

;; ----- Partial application of a user-defined function ----
(: add3 (-> Integer (-> Integer (-> Integer Integer))))
(define (add3 a b c) (+ a (+ b c)))

(: add3-partial-1 (-> Integer (-> Integer Integer)))
(define add3-partial-1 (add3 10))

(: add3-partial-2 (-> Integer Integer))
(define add3-partial-2 (add3-partial-1 20))

(: add3-final Integer)
(define add3-final (add3-partial-2 30))

;; ----- Over-application across a returned lambda ----------
;; fma takes two params and RETURNS a one-param lambda.  In a
;; curried language (fma 3 4 5) must equal ((fma 3 4) 5): the
;; surplus argument flows into the returned lambda.
(: fma (-> Integer (-> Integer (-> Integer Integer))))
(define (fma a b) (lambda (c) (+ (* a b) c)))

(: fma-stepwise Integer)
(define fma-stepwise ((fma 3 4) 5))

(: fma-flat Integer)
(define fma-flat (fma 3 4 5))

;; Over-application through TWO single-param boundaries: every
;; lambda, not only multi-param ones, must absorb surplus args.
(: g3 (-> Integer (-> Integer (-> Integer Integer))))
(define (g3 a) (lambda (b) (lambda (c) (+ a (+ b c)))))

(: g3-flat Integer)
(define g3-flat (g3 1 2 3))

;; ----- Partial application of a class method (fmap) -------
(: lifted-inc (-> (Maybe Integer) (Maybe Integer)))
(define lifted-inc (fmap (+ 1)))

(: lifted-result1 (Maybe Integer))
(define lifted-result1 (lifted-inc (Some 41)))

(: lifted-result2 (Maybe Integer))
(define lifted-result2 (lifted-inc None))

;; ----- Partial application of a prelude function ---------
(: append-hello (-> String String))
(define append-hello (string-append "hello, "))

(: greeting String)
(define greeting (append-hello "world"))

;; ---------- assertions ----------------------------------------

(: suite (List Test))
(define suite
  (list
   (it "partial application of operator +"
       (check-equal? inc-result 42))
   (it "partial application of user-defined ternary function"
       (check-equal? add3-final 60))
   (it "over-application across a returned lambda equals stepwise"
       (all-checks
        (list (check-equal? fma-stepwise 17)
              (check-equal? fma-flat 17))))
   (it "over-application through two single-param boundaries"
       (check-equal? g3-flat 6))
   (it "partial application of class method fmap"
       (all-checks
        (list (check-equal? lifted-result1 (Some 42))
              (check-equal? lifted-result2 None))))
   (it "partial application of prelude string-append"
       (check-equal? greeting "hello, world"))))

(: _ran Unit)
(define _ran (run-io (run-suite "curried-application" suite)))

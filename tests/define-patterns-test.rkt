#lang racket/base

;; Pattern destructuring in `define` parameter positions, plus
;; multi-clause `define` forms (Haskell-style equational definitions).

(require rackunit
         "../main.rkt")

;; ----- Piece 1: single-form parameter destructuring -----------

(rackton
  (struct Point
    [x : Float]
    [y : Float]
    #:deriving Show Eq Ord)

  (: distance (-> Point Point Float))
  (define (distance (Point px py) (Point qx qy))
    (+ (- px qx) (- py qy)))

  (: r1 Float)
  (define r1 (distance (Point 0.0 0.0) (Point 3.0 4.0)))

  ;; Mixed: first parameter is a plain identifier, second is a pattern.
  (: scale (-> Integer (Pair Integer Integer) (Pair Integer Integer)))
  (define (scale k (MkPair x y)) (MkPair (* k x) (* k y)))

  (: r2 (Pair Integer Integer))
  (define r2 (scale 3 (MkPair 2 7)))

  (provide r1 r2))

(test-case "destructure two product-type args"
  (check-equal? r1 -7.0))

(test-case "mixed plain-id and pattern params"
  (check-equal? r2 (MkPair 6 21)))

;; ----- Piece 2: multi-clause defines --------------------------

(rackton
  (data MyList Nada (Mcons Integer MyList))

  ;; Two clauses; bare uppercase id `Nada` dispatches as a 0-arg
  ;; ctor pattern when there's more than one clause.
  (define (myhead (Mcons x _)) x)
  (define (myhead Nada)        0)

  (: r-cons Integer)
  (define r-cons (myhead (Mcons 7 Nada)))
  (: r-nada Integer)
  (define r-nada (myhead Nada))

  ;; Multi-clause with multiple arguments: `match*` lowering, no
  ;; tuple allocation.
  (define (both-some (Some x) (Some y)) (Some (+ x y)))
  (define (both-some _        _)        None)

  (: bs1 (Maybe Integer))
  (define bs1 (both-some (Some 3) (Some 4)))
  (: bs2 (Maybe Integer))
  (define bs2 (both-some (Some 3) None))
  (: bs3 (Maybe Integer))
  (define bs3 (both-some None (Some 4)))

  (provide r-cons r-nada bs1 bs2 bs3))

(test-case "multi-clause myhead: Mcons branch"
  (check-equal? r-cons 7))

(test-case "multi-clause myhead: Nada branch"
  (check-equal? r-nada 0))

(test-case "multi-clause two-arg: both Some"
  (check-equal? bs1 (Some 7)))

(test-case "multi-clause two-arg: one None on right"
  (check-equal? bs2 None))

(test-case "multi-clause two-arg: one None on left"
  (check-equal? bs3 None))

;; ----- Conflict cases (compile-time errors) -------------------

(require (for-syntax racket/base))
(define-syntax-rule (check-rackton-compile-error form ...)
  (check-exn
   exn:fail?
   (lambda ()
     (eval #'(rackton form ...)
           (variable-reference->namespace (#%variable-reference))))))

(test-case "arity mismatch between clauses is rejected"
  (check-rackton-compile-error
   (define (f x)      x)
   (define (f x y)    x)))

(test-case "value-form mixed with function-form for the same name is rejected"
  (check-rackton-compile-error
   (define g 5)
   (define (g x) x)))

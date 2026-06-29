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
    :deriving Show Eq Ord)

  (: distance (-> Point Point Float))
  (define (distance (Point px py) (Point qx qy))
    (+ (- px qx) (- py qy)))

  (: r1 Float)
  (define r1 (distance (Point 0.0 0.0) (Point 3.0 4.0)))

  ;; Mixed: first parameter is a plain identifier, second is a pattern.
  (: scale (-> Integer (Pair Integer Integer) (Pair Integer Integer)))
  (define (scale k (Pair x y)) (Pair (* k x) (* k y)))

  (: r2 (Pair Integer Integer))
  (define r2 (scale 3 (Pair 2 7)))

  (provide r1 r2))

(test-case "destructure two product-type args"
  (check-equal? r1 -7.0))

(test-case "mixed plain-id and pattern params"
  (check-equal? r2 (Pair 6 21)))

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

;; ----- Piece 3: multi-clause defines inside instances ---------

(rackton
  (data (NL a) (Kons a (NL a)) (Sole a) :deriving Eq Show)

  (protocol (Head (w :: (-> * *)))
    (: hd (-> (w a) a)))

  ;; One-argument method, two clauses.
  (instance (Head NL)
    (define (hd (Sole x))   x)
    (define (hd (Kons x _)) x))

  ;; Two-argument method, two clauses (match* lowering inside an instance).
  (instance (Functor NL)
    (define (fmap f (Sole x))    (Sole (f x)))
    (define (fmap f (Kons x xs)) (Kons (f x) (fmap f xs))))

  (: ihd1 Integer)
  (define ihd1 (hd (Sole 5)))
  (: ihd2 Integer)
  (define ihd2 (hd (Kons 9 (Sole 5))))
  (: ifm (NL Integer))
  (define ifm (fmap (+ 1) (Kons 1 (Sole 2))))

  (provide ihd1 ihd2 ifm Kons Sole))

(test-case "multi-clause instance method: first clause reachable"
  (check-equal? ihd1 5))
(test-case "multi-clause instance method: second clause reachable"
  (check-equal? ihd2 9))
(test-case "multi-clause two-arg instance method (fmap)"
  (check-equal? ifm (Kons 2 (Sole 3))))

;; ----- Piece 4: multi-clause defines as protocol defaults -----

(rackton
  (data (NLb a) (Konsb a (NLb a)) (Soleb a) :deriving Eq Show)

  ;; The default for `pick` is written equationally; the instance
  ;; provides nothing, so the multi-clause default must dispatch.
  (protocol (Pick (w :: (-> * *)))
    (: pick (-> (w a) a))
    (define (pick (Soleb x))   x)
    (define (pick (Konsb x _)) x))

  (instance (Pick NLb))

  (: pd1 Integer)
  (define pd1 (pick (Soleb 7)))
  (: pd2 Integer)
  (define pd2 (pick (Konsb 3 (Soleb 7))))

  (provide pd1 pd2))

(test-case "multi-clause protocol default: first clause reachable"
  (check-equal? pd1 7))
(test-case "multi-clause protocol default: second clause reachable"
  (check-equal? pd2 3))

;; ----- Piece 5: end-to-end Comonad (instance + defaults + :derive)

(rackton
  (protocol (Comonad (w :: (-> * *)))
    (: extract   (-> (w a) a))
    (: duplicate (-> (w a) (w (w a))))
    (: extend    (-> (-> (w a) b) (w a) (w b)))
    (define (duplicate wa) (extend id wa))
    (define (extend f wa)  (fmap f (duplicate wa)))
    :derive
    ((Functor
      (define (fmap f wa) (extend (compose f extract) wa)))))

  (data (Nonempty-List a)
    (Konsn a (Nonempty-List a))
    (Solen a)
    :deriving Eq Show)

  (instance (Functor Nonempty-List)
    (define (fmap f xs)
      (match xs
        [(Solen x)     (Solen (f x))]
        [(Konsn x xs)  (Konsn (f x) (fmap f xs))])))

  (instance (Comonad Nonempty-List)
    (define (extract (Solen x))   x)
    (define (extract (Konsn x _)) x)

    (define (duplicate (Solen x)) (Solen (Solen x)))
    (define (duplicate (Konsn x xs))
      (Konsn (Konsn x xs) (duplicate xs))))

  (define cm-res  (Konsn 1 (Konsn 2 (Solen 3))))
  (define cm-res2 (duplicate cm-res))
  ;; All suffixes, comonadic duplicate.
  (define cm-expected
    (Konsn (Konsn 1 (Konsn 2 (Solen 3)))
           (Konsn (Konsn 2 (Solen 3))
                  (Solen (Solen 3)))))

  (provide cm-res2 cm-expected))

(test-case "comonadic duplicate over a non-empty list (multi-clause instance)"
  (check-equal? cm-res2 cm-expected))

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

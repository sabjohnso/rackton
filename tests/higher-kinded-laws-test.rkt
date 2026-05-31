#lang rackton

;; Higher-kinded algebraic-law property tests.
;;
;; Exercises the functor / applicative / monad / traversable law
;; bundles from rackton/unit on the prelude's `Maybe` and `List`
;; instances.  This file is written in the Rackton native testing
;; framework end to end: the bundles build `Test` trees of seeded
;; properties, `run-tests` walks them in IO, and the module `panic`s if
;; any property fails so `raco test` sees a non-zero result.
;;
;; The law bundles are generic over the container, so they take the
;; pieces that cannot be recovered generically as explicit arguments:
;; an `eq` predicate and a `render` for `(f Integer)`, and — for the
;; applicative/monad point operation, which is return-typed — a rank-2
;; `point`.  Those explicit pieces are defined here for Maybe and List.

(require "../unit.rkt")

;; ----- equality and rendering for the container types ---------------

(: eq-maybe-int (-> (Maybe Integer) (-> (Maybe Integer) Boolean)))
(define (eq-maybe-int a b)
  (match a
    [(None)   (match b [(None) #t]   [(Some _) #f])]
    [(Some x) (match b [(None) #f]   [(Some y) (== x y)])]))

(: render-maybe (-> (Maybe Integer) String))
(define (render-maybe m)
  (match m
    [(None)   "None"]
    [(Some x) (string-append "Some " (integer->string x))]))

(: eq-list-int (-> (List Integer) (-> (List Integer) Boolean)))
(define (eq-list-int a b)
  (match a
    [(Nil)       (match b [(Nil) #t] [(Cons _ _) #f])]
    [(Cons x xs) (match b
                   [(Nil)       #f]
                   [(Cons y ys) (and (== x y) (eq-list-int xs ys))])]))

(: render-list (-> (List Integer) String))
(define (render-list xs)
  (string-append
   "["
   (string-append (foldr (lambda (n acc)
                           (string-append (integer->string n)
                                          (string-append " " acc)))
                         "" xs)
                  "]")))

;; Traversable's identity law produces a `Maybe`-wrapped container.
(: eq-maybe-maybe (-> (Maybe (Maybe Integer)) (-> (Maybe (Maybe Integer)) Boolean)))
(define (eq-maybe-maybe a b)
  (match a
    [(None)   (match b [(None) #t] [(Some _) #f])]
    [(Some x) (match b [(None) #f] [(Some y) (eq-maybe-int x y)])]))

(: eq-maybe-list (-> (Maybe (List Integer)) (-> (Maybe (List Integer)) Boolean)))
(define (eq-maybe-list a b)
  (match a
    [(None)    (match b [(None) #t] [(Some _) #f])]
    [(Some xs) (match b [(None) #f] [(Some ys) (eq-list-int xs ys)])]))

;; ----- generators ---------------------------------------------------

;; A mix of None and Some across the small integer range.
(: gen-maybe-int (Gen (Maybe Integer)))
(define gen-maybe-int
  (fmap (lambda (n) (if (< n 0) None (Some n))) (int-range -5 20)))

(: gen-list-int (Gen (List Integer)))
(define gen-list-int (gen-list (int-range -5 20)))

;; ----- the point (pure/return) operations ---------------------------
;;
;; The bundles take `point` monomorphically because `pure` is
;; return-typed.  `point` is at the element type; `point-fn` is the same
;; `pure` at the function type, which the applicative laws need.  Both
;; are just the constructor (`Some` / single-element list).

(: maybe-point (-> Integer (Maybe Integer)))
(define (maybe-point x) (Some x))

(: maybe-point-fn (-> (-> Integer Integer) (Maybe (-> Integer Integer))))
(define (maybe-point-fn g) (Some g))

(: list-point (-> Integer (List Integer)))
(define (list-point x) (Cons x Nil))

(: list-point-fn (-> (-> Integer Integer) (List (-> Integer Integer))))
(define (list-point-fn g) (Cons g Nil))

;; ----- the suite ----------------------------------------------------

(: suite (List Test))
(define suite
  (list
   (functor-laws     eq-maybe-int  render-maybe gen-maybe-int)
   (functor-laws     eq-list-int   render-list  gen-list-int)
   (applicative-laws eq-maybe-int render-maybe maybe-point maybe-point-fn gen-maybe-int)
   (applicative-laws eq-list-int  render-list  list-point  list-point-fn  gen-list-int)
   (monad-laws       eq-maybe-int render-maybe maybe-point gen-maybe-int)
   (monad-laws       eq-list-int  render-list  list-point  gen-list-int)
   (traversable-laws eq-maybe-maybe render-maybe gen-maybe-int)
   (traversable-laws eq-maybe-list  render-list  gen-list-int)))

;; ----- run, accumulate failures, panic if any -----------------------

(: run-all (-> (List Test) (IO Integer)))
(define (run-all tests)
  (foldr (lambda (t acc-io)
           (flatmap (lambda (s)
                      (flatmap (lambda (rest)
                                 (pure (+ (summary-failed s) rest)))
                               acc-io))
                    (run-tests t)))
         (pure 0)
         tests))

(: main (IO Unit))
(define main
  (flatmap (lambda (fails)
             (if (> fails 0)
                 (panic "higher-kinded laws failed")
                 (pure MkUnit)))
           (run-all suite)))

(: _ran Unit)
(define _ran (run-io main))

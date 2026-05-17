#lang racket/base

;; Rackton — compile-time side of the Phase-3 prelude.
;;
;; The prelude is itself a Rackton program: a list of class declarations,
;; instance declarations, ADT definitions, and combinators.  We parse and
;; elaborate that program at module-load time and expose the resulting
;; typing environment as `prelude-env`.  The runtime side (dispatch tables,
;; struct definitions, builtin instance registrations) lives in
;; private/prelude-runtime.rkt; the two must keep their names in sync.

(require "surface.rkt"
         "infer.rkt"
         "env.rkt")

(provide prelude-env)

;; ----- Prelude source ----------------------------------------------

(define prelude-source-forms
  '(;; --- Eq ----------------------------------------------------------

    (define-class (Eq a)
      (: == (-> a (-> a Boolean)))
      (: /= (-> a (-> a Boolean)))
      (define (/= x y) (if (== x y) #f #t)))

    ;; --- Ord (Eq is a superclass) -------------------------------

    (define-class ((Eq a) => (Ord a))
      (: <  (-> a (-> a Boolean)))
      (: >  (-> a (-> a Boolean)))
      (: <= (-> a (-> a Boolean)))
      (: >= (-> a (-> a Boolean)))
      (define (>  x y) (<  y x))
      (define (<= x y) (if (<  x y) #t (== x y)))
      (define (>= x y) (if (>  x y) #t (== x y))))

    ;; --- Num ----------------------------------------------------

    (define-class (Num a)
      (: + (-> a (-> a a)))
      (: - (-> a (-> a a)))
      (: * (-> a (-> a a))))

    ;; --- Show ---------------------------------------------------

    (define-class (Show a)
      (: show (-> a String)))

    ;; --- Builtin instances --------------------------------------
    ;; Bodies of the form `(racket τ (vars) 0)` etc. are placeholders;
    ;; only the type discipline matters here.  The actual runtime
    ;; implementations live in prelude-runtime.rkt.

    (define-instance (Num Integer)
      (define (+ x y) (racket Integer (x y) 0))
      (define (- x y) (racket Integer (x y) 0))
      (define (* x y) (racket Integer (x y) 0)))

    (define-instance (Eq Integer)
      (define (== x y) (racket Boolean (x y) #f)))

    (define-instance (Eq Boolean)
      (define (== x y) (if x y (if y #f #t))))

    (define-instance (Eq String)
      (define (== x y) (racket Boolean (x y) #f)))

    (define-instance (Ord Integer)
      (define (< x y) (racket Boolean (x y) #f)))

    (define-instance (Show Integer)
      (define (show x) (racket String (x) "")))

    (define-instance (Show Boolean)
      (define (show x) (if x "True" "False")))

    (define-instance (Show String)
      (define (show x) x))

    ;; --- ADTs ---------------------------------------------------

    (define-data (Maybe a)    None (Some a))
    (define-data (List a)     Nil  (Cons a (List a)))
    (define-data (Pair a b)   (MkPair a b))
    (define-data (Result e a) (Err e) (Ok a))
    (define-data Unit MkUnit)

    ;; --- Combinators --------------------------------------------

    (: id (-> a a))
    (define (id x) x)

    (: const (-> a (-> b a)))
    (define (const x) (lambda (_y) x))

    ;; --- Functor / Monad (higher-kinded) ----------------------

    (define-class (Functor (f :: (-> * *)))
      (: fmap (-> (-> a b) (-> (f a) (f b)))))

    (define-class ((Functor m) => (Monad (m :: (-> * *))))
      (: >>= (-> (m a) (-> (-> a (m b)) (m b)))))

    ;; Maybe
    (define-instance (Functor Maybe)
      (define (fmap f m)
        (match m
          [(None)   None]
          [(Some x) (Some (f x))])))

    (define-instance (Monad Maybe)
      (define (>>= m f)
        (match m
          [(None)   None]
          [(Some x) (f x)])))

    ;; List
    (define-instance (Functor List)
      (define (fmap f xs)
        (match xs
          [(Nil)        Nil]
          [(Cons h t)   (Cons (f h) (fmap f t))])))

    ;; Result e (the error type is fixed; we map over the success type)
    (define-instance (Functor (Result e))
      (define (fmap f r)
        (match r
          [(Err x) (Err x)]
          [(Ok  v) (Ok (f v))])))

    (define-instance (Monad (Result e))
      (define (>>= r f)
        (match r
          [(Err x) (Err x)]
          [(Ok  v) (f v)])))))

(define prelude-env
  (let ([forms (for/list ([f (in-list prelude-source-forms)])
                 (parse-top (datum->syntax #f f)))])
    (infer-program forms initial-env)))

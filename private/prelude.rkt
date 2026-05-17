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
          [(Ok  v) (f v)])))

    ;; --- Small stdlib ------------------------------------------

    (: not (-> Boolean Boolean))
    (define (not b) (if b #f #t))

    (: and (-> Boolean (-> Boolean Boolean)))
    (define (and a b) (if a b #f))

    (: or (-> Boolean (-> Boolean Boolean)))
    (define (or a b) (if a #t b))

    (: length (-> (List a) Integer))
    (define (length xs)
      (match xs
        [(Nil)         0]
        [(Cons _ rest) (+ 1 (length rest))]))

    (: foldr (-> (-> a (-> b b)) (-> b (-> (List a) b))))
    (define (foldr f z xs)
      (match xs
        [(Nil)        z]
        [(Cons h t)   (f h (foldr f z t))]))

    (: filter (-> (-> a Boolean) (-> (List a) (List a))))
    (define (filter p xs)
      (match xs
        [(Nil)        Nil]
        [(Cons h t)   (if (p h)
                          (Cons h (filter p t))
                          (filter p t))]))

    ;; --- Strings ----------------------------------------------

    (: string-length (-> String Integer))
    (define (string-length s) (racket Integer (s) 0))

    (: string-append (-> String (-> String String)))
    (define (string-append a b) (racket String (a b) ""))

    (: substring (-> String (-> Integer (-> Integer String))))
    (define (substring s start end) (racket String (s start end) ""))

    ;; --- Numeric helpers --------------------------------------

    (: mod (-> Integer (-> Integer Integer)))
    (define (mod a b) (racket Integer (a b) 0))

    (: div (-> Integer (-> Integer Integer)))
    (define (div a b) (racket Integer (a b) 0))

    (: abs (-> Integer Integer))
    (define (abs n) (racket Integer (n) 0))

    (: min (-> Integer (-> Integer Integer)))
    (define (min a b) (racket Integer (a b) 0))

    (: max (-> Integer (-> Integer Integer)))
    (define (max a b) (racket Integer (a b) 0))

    (: integer->string (-> Integer String))
    (define (integer->string n) (racket String (n) ""))

    (: string->integer (-> String (Maybe Integer)))
    (define (string->integer s) (racket (Maybe Integer) (s) None))

    ;; --- IO ---------------------------------------------------

    (define-data (IO a))

    (define-instance (Functor IO)
      (define (fmap f io) (racket (IO b) (f io) #f)))

    (define-instance (Monad IO)
      (define (>>= io f) (racket (IO b) (io f) #f)))

    (: print     (-> String (IO Unit)))
    (define (print s) (racket (IO Unit) (s) #f))

    (: println   (-> String (IO Unit)))
    (define (println s) (racket (IO Unit) (s) #f))

    (: read-line (IO String))
    (define read-line (racket (IO String) () #f))

    (: pure-io   (-> a (IO a)))
    (define (pure-io x) (racket (IO a) (x) #f))

    (: run-io    (-> (IO a) a))
    (define (run-io io) (racket a (io) #f))))

(define prelude-env
  (let ([forms (for/list ([f (in-list prelude-source-forms)])
                 (parse-top (datum->syntax #f f)))])
    (infer-program forms initial-env)))

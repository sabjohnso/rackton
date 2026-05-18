#lang racket/base

;; Rackton — runtime side of the Phase-3 prelude.
;;
;; Defines the ADT structs that ship with the language, the per-method
;; dispatch tables for every prelude class, the generic-method functions,
;; and the built-in instance registrations for `Integer`, `Boolean`, and
;; `String`.  The compile-time side — class-info / instance-info entries
;; in the typing env — lives in env.rkt's `prelude-env`.
;;
;; Built-in Racket operators are imported under `rkt:` prefixes so they
;; don't clash with the Rackton method names we want to expose.

(require (rename-in racket/base
                    [+  rkt:+]  [-  rkt:-]  [*  rkt:*]  [/  rkt:/]
                    [<  rkt:<]  [>  rkt:>]  [=  rkt:=]
                    [<= rkt:<=] [>= rkt:>=]
                    [not rkt:not]
                    [and rkt:and]
                    [or  rkt:or]
                    [length    rkt:length]
                    [substring rkt:substring]
                    [string-length rkt:string-length]
                    [string-append rkt:string-append]
                    [modulo  rkt:modulo]
                    [quotient rkt:quotient]
                    [abs rkt:abs]
                    [min rkt:min]
                    [max rkt:max]
                    [number->string rkt:number->string]
                    [string->number rkt:string->number]
                    [read-line  rkt:read-line]
                    [reverse    rkt:reverse]
                    [append     rkt:append]
                    [sort       rkt:sort]
                    [sqrt       rkt:sqrt]
                    [exact->inexact rkt:exact->inexact]
                    [inexact->exact rkt:inexact->exact]
                    [truncate   rkt:truncate])
         racket/format
         racket/match
         racket/file
         "adt.rkt"
         "dict.rkt")

(provide
 ;; ADTs (constructors usable as expressions and as match patterns)
 None Some Nil Cons MkPair Ok Err MkUnit

 ;; Class methods
 +  -  *
 ==  /=
 <  >  <=  >=
 show
 fmap
 >>=

 ;; Dispatch tables — exposed so user modules that declare new
 ;; instances (including derived ones) can register against them.
 $dispatch:+  $dispatch:-  $dispatch:*
 $dispatch:== $dispatch:/=
 $dispatch:<  $dispatch:>  $dispatch:<= $dispatch:>=
 $dispatch:show
 $dispatch:fmap
 $dispatch:>>=
 $dispatch:float-div

 ;; Combinators
 id compose flip const

 ;; Stdlib
 not and or length foldr filter

 ;; Strings
 string-length string-append substring
 ;; codegen-only helper for derived Show
 $show-concat

 ;; Numeric helpers
 mod div abs min max integer->string string->integer

 ;; IO
 print println read-line pure-io run-io

 ;; Mutable refs and file I/O
 make-ref read-ref write-ref
 read-file write-file file-exists?

 ;; List + Pair helpers
 reverse append zip take drop find sort
 fst snd swap

 ;; Panic
 panic

 ;; Immutable Map / Set
 empty-map map-insert map-lookup map-delete map-keys map-values map-size map-fold
 empty-set set-insert set-member? set-delete set-size set-to-list

 ;; List helpers
 concat-map group-by

 ;; Float
 float-div sqrt integer->float float->integer

 ;; Error handling
 try raise-io)

;; ----- ADTs -------------------------------------------------------

(define-data-ctor None 0)
(define-data-ctor Some 1)

(define-data-ctor Nil  0)
(define-data-ctor Cons 2)

(define-data-ctor MkPair 2)

(define-data-ctor Ok  1)
(define-data-ctor Err 1)

(define-data-ctor MkUnit 0)

;; ----- Class dispatch tables -------------------------------------

(define $dispatch:+  (make-hasheq))  (define-class-method +  $dispatch:+)
(define $dispatch:-  (make-hasheq))  (define-class-method -  $dispatch:-)
(define $dispatch:*  (make-hasheq))  (define-class-method *  $dispatch:*)
(define $dispatch:== (make-hasheq))  (define-class-method == $dispatch:==)
(define $dispatch:/= (make-hasheq))  (define-class-method /= $dispatch:/=)
(define $dispatch:<  (make-hasheq))  (define-class-method <  $dispatch:<)
(define $dispatch:>  (make-hasheq))  (define-class-method >  $dispatch:>)
(define $dispatch:<= (make-hasheq))  (define-class-method <= $dispatch:<=)
(define $dispatch:>= (make-hasheq))  (define-class-method >= $dispatch:>=)
(define $dispatch:show (make-hasheq))(define-class-method show $dispatch:show)
;; Functor's fmap dispatches on the SECOND argument (the container).
(define $dispatch:fmap (make-hasheq))(define-class-method fmap $dispatch:fmap 1)
;; Monad's bind dispatches on the FIRST argument (the wrapped value).
(define $dispatch:>>=  (make-hasheq))(define-class-method >>=  $dispatch:>>=  0)

;; ----- Num Integer ------------------------------------------------

(register-instance-method! $dispatch:+  'Integer (lambda (x y) (rkt:+  x y)))
(register-instance-method! $dispatch:-  'Integer (lambda (x y) (rkt:-  x y)))
(register-instance-method! $dispatch:*  'Integer (lambda (x y) (rkt:*  x y)))

;; ----- Num / Eq / Ord / Show Float --------------------------------

(register-instance-method! $dispatch:+  'Float (lambda (x y) (rkt:+ x y)))
(register-instance-method! $dispatch:-  'Float (lambda (x y) (rkt:- x y)))
(register-instance-method! $dispatch:*  'Float (lambda (x y) (rkt:* x y)))
(register-instance-method! $dispatch:== 'Float (lambda (x y) (rkt:= x y)))
(register-instance-method! $dispatch:/= 'Float (lambda (x y) (rkt:not (rkt:= x y))))
(register-instance-method! $dispatch:<  'Float (lambda (x y) (rkt:<  x y)))
(register-instance-method! $dispatch:>  'Float (lambda (x y) (rkt:>  x y)))
(register-instance-method! $dispatch:<= 'Float (lambda (x y) (rkt:<= x y)))
(register-instance-method! $dispatch:>= 'Float (lambda (x y) (rkt:>= x y)))
(register-instance-method! $dispatch:show 'Float (lambda (x) (rkt:number->string x)))

;; ----- Eq instances ----------------------------------------------

(register-instance-method! $dispatch:== 'Integer (lambda (x y) (rkt:= x y)))
(register-instance-method! $dispatch:== 'Boolean (lambda (x y) (if x y (not y))))
(register-instance-method! $dispatch:== 'String  (lambda (x y) (string=? x y)))

(register-instance-method! $dispatch:/= 'Integer (lambda (x y) (not (rkt:= x y))))
(register-instance-method! $dispatch:/= 'Boolean (lambda (x y) (not (if x y (not y)))))
(register-instance-method! $dispatch:/= 'String  (lambda (x y) (not (string=? x y))))

;; ----- Ord Integer -----------------------------------------------

(register-instance-method! $dispatch:<  'Integer (lambda (x y) (rkt:<  x y)))
(register-instance-method! $dispatch:>  'Integer (lambda (x y) (rkt:>  x y)))
(register-instance-method! $dispatch:<= 'Integer (lambda (x y) (rkt:<= x y)))
(register-instance-method! $dispatch:>= 'Integer (lambda (x y) (rkt:>= x y)))

;; ----- Show instances --------------------------------------------

(register-instance-method! $dispatch:show 'Integer
                           (lambda (x) (number->string x)))
(register-instance-method! $dispatch:show 'Boolean
                           (lambda (x) (if x "True" "False")))
(register-instance-method! $dispatch:show 'String
                           (lambda (x) (~a "\"" x "\"")))

;; ----- Combinators ----------------------------------------------

(define (id x) x)
(define (compose f g) (lambda (x) (f (g x))))
(define (flip f) (lambda (x y) (f y x)))
(define (const x) (lambda (_y) x))

;; ----- Stdlib ----------------------------------------------------

(define (not b) (if b #f #t))
(define (and a b) (if a b #f))
(define (or  a b) (if a #t b))

(define (length xs)
  (match xs
    [(Nil)        0]
    [(Cons _ t)   (rkt:+ 1 (length t))]))

(define (foldr f z xs)
  (match xs
    [(Nil)        z]
    [(Cons h t)   (f h (foldr f z t))]))

(define (filter p xs)
  (match xs
    [(Nil)        Nil]
    [(Cons h t)   (if (p h) (Cons h (filter p t)) (filter p t))]))

;; ----- Strings -------------------------------------------------

(define (string-length s) (rkt:string-length s))
(define (string-append a b) (rkt:string-append a b))
(define (substring s start end) (rkt:substring s start end))

;; A variadic concatenation used by derived Show instances.  The
;; Rackton-typed `string-append` is binary; this helper sidesteps the
;; binary signature for codegen-emitted strings.
(define $show-concat
  (lambda strs (apply rkt:string-append strs)))

;; ----- Numeric helpers -----------------------------------------

(define (mod a b) (rkt:modulo a b))
(define (div a b) (rkt:quotient a b))
(define (abs n) (rkt:abs n))
(define (min a b) (rkt:min a b))
(define (max a b) (rkt:max a b))
(define (integer->string n) (rkt:number->string n))
(define (string->integer s)
  (define n (rkt:string->number s))
  (if (rkt:and n (exact-integer? n)) (Some n) None))

;; ----- IO monad ------------------------------------------------

(struct $io (thunk) #:transparent)

(define (run-io io) (($io-thunk io)))

(define (print s)   ($io (lambda () (display   s) MkUnit)))
(define (println s) ($io (lambda () (displayln s) MkUnit)))
(define read-line
  ($io (lambda ()
         (define line (rkt:read-line))
         (if (eof-object? line) "" line))))
(define (pure-io x) ($io (lambda () x)))

(define (io-fmap f io)
  ($io (lambda () (f (run-io io)))))

(define (io-bind io f)
  ($io (lambda () (run-io (f (run-io io))))))

(register-instance-method! $dispatch:fmap '$io io-fmap)
(register-instance-method! $dispatch:>>=  '$io io-bind)

;; ----- Mutable refs (in IO) -----------------------------------

(define (make-ref v) ($io (lambda () (box v))))
(define (read-ref r) ($io (lambda () (unbox r))))
(define (write-ref r v) ($io (lambda () (set-box! r v) MkUnit)))

;; ----- File I/O ------------------------------------------------

(define (read-file path)
  ($io (lambda () (file->string path))))
(define (write-file path contents)
  ($io (lambda ()
         (with-output-to-file path #:exists 'replace
           (lambda () (display contents)))
         MkUnit)))
(define (file-exists? path)
  ($io (lambda () (rkt:file-exists?-impl path))))

;; Bridge for racket/base's file-exists?  We aliased it under our own
;; name above; here we reach Racket's definition directly.
(require (rename-in racket/base [file-exists? rkt:file-exists?-impl]))

;; ----- List helpers -------------------------------------------

(define (reverse xs)
  (let loop ([xs xs] [acc Nil])
    (match xs
      [(Nil) acc]
      [(Cons h t) (loop t (Cons h acc))])))

(define (append xs ys)
  (match xs
    [(Nil) ys]
    [(Cons h t) (Cons h (append t ys))]))

(define (zip as bs)
  (match as
    [(Nil) Nil]
    [(Cons a at)
     (match bs
       [(Nil) Nil]
       [(Cons b bt) (Cons (MkPair a b) (zip at bt))])]))

(define (take n xs)
  (cond
    [(rkt:<= n 0) Nil]
    [else
     (match xs
       [(Nil) Nil]
       [(Cons h t) (Cons h (take (rkt:- n 1) t))])]))

(define (drop n xs)
  (cond
    [(rkt:<= n 0) xs]
    [else
     (match xs
       [(Nil) Nil]
       [(Cons _ t) (drop (rkt:- n 1) t)])]))

(define (find p xs)
  (match xs
    [(Nil) None]
    [(Cons h t) (if (p h) (Some h) (find p t))]))

;; Merge sort over Rackton's < (Ord).  O(n log n) stable.
(define (split-at-runtime n xs)
  (cond
    [(rkt:= n 0) (MkPair Nil xs)]
    [else
     (match xs
       [(Nil) (MkPair Nil Nil)]
       [(Cons h t)
        (define rest (split-at-runtime (rkt:- n 1) t))
        (MkPair (Cons h (fst rest)) (snd rest))])]))

(define (merge-lists xs ys)
  (match xs
    [(Nil) ys]
    [(Cons hx tx)
     (match ys
       [(Nil) xs]
       [(Cons hy ty)
        (if (< hx hy)
            (Cons hx (merge-lists tx ys))
            (Cons hy (merge-lists xs ty)))])]))

(define (sort xs)
  (define n (length xs))
  (cond
    [(rkt:< n 2) xs]
    [else
     (define halves (split-at-runtime (rkt:quotient n 2) xs))
     (merge-lists (sort (fst halves)) (sort (snd halves)))]))

;; ----- Pair helpers -------------------------------------------

(define (fst p)  (match p [(MkPair a _) a]))
(define (snd p)  (match p [(MkPair _ b) b]))
(define (swap p) (match p [(MkPair a b) (MkPair b a)]))

;; ----- panic ---------------------------------------------------

(define (panic msg) (error 'rackton-panic msg))

;; ----- Immutable Map (backed by Racket's immutable hash) ------

(struct $map (h) #:transparent)
(struct $set (h) #:transparent)

(define empty-map ($map (hash)))
(define empty-set ($set (hash)))

(define (map-insert k v m) ($map (hash-set ($map-h m) k v)))
(define (map-lookup k m)
  (cond [(hash-has-key? ($map-h m) k) (Some (hash-ref ($map-h m) k))]
        [else None]))
(define (map-delete k m) ($map (hash-remove ($map-h m) k)))

;; Build a rackton List from a Racket sequence.
(define (rkt-seq->list xs)
  (let loop ([xs (reverse xs)] [acc Nil])
    (cond [(null? xs) acc]
          [else (loop (cdr xs) (Cons (car xs) acc))])))

(define (map-keys   m) (rkt-seq->list (hash-keys   ($map-h m))))
(define (map-values m) (rkt-seq->list (hash-values ($map-h m))))
(define (map-size   m) (hash-count ($map-h m)))

(define (map-fold f z m)
  (for/fold ([acc z]) ([(k v) (in-hash ($map-h m))])
    (((f k) v) acc)))

;; ----- Immutable Set ------------------------------------------

(define (set-insert x s) ($set (hash-set ($set-h s) x #t)))
(define (set-member? x s) (hash-has-key? ($set-h s) x))
(define (set-delete x s) ($set (hash-remove ($set-h s) x)))
(define (set-size s) (hash-count ($set-h s)))
(define (set-to-list s) (rkt-seq->list (hash-keys ($set-h s))))

;; ----- List helpers -------------------------------------------

(define (concat-map f xs)
  (foldr (lambda (x acc) (append (f x) acc)) Nil xs))

(define (group-by key xs)
  (foldr (lambda (x m)
           (define k (key x))
           (match (map-lookup k m)
             [(None)     (map-insert k (Cons x Nil) m)]
             [(Some lst) (map-insert k (Cons x lst) m)]))
         empty-map
         xs))

;; ----- Float ops ---------------------------------------------

;; Fractional method dispatcher
(define $dispatch:float-div (make-hasheq))
(define-class-method float-div $dispatch:float-div 0)
(register-instance-method! $dispatch:float-div 'Float
                           (lambda (x y) (rkt:/ x y)))

(define (sqrt x) (rkt:sqrt x))
(define (integer->float n) (rkt:exact->inexact n))
(define (float->integer x) (rkt:inexact->exact (rkt:truncate x)))

;; ----- try / raise-io ---------------------------------------

(define (try io)
  ($io (lambda ()
         (with-handlers ([exn:fail?
                          (lambda (e) (Err (exn-message e)))])
           (Ok (run-io io))))))

(define (raise-io msg)
  ($io (lambda () (error 'rackton-raise-io msg))))

;; ----- Functor / Monad instance impls ------------------------

;; Maybe — both `None` and `Some` tags share the same impl, which
;; pattern-matches at runtime.
(define maybe-fmap
  (lambda (f m)
    (match m
      [(None)   None]
      [(Some x) (Some (f x))])))
(register-instance-method! $dispatch:fmap '$ctor:None  maybe-fmap)
(register-instance-method! $dispatch:fmap '$ctor:Some  maybe-fmap)

(define maybe->>=
  (lambda (m f)
    (match m
      [(None)   None]
      [(Some x) (f x)])))
(register-instance-method! $dispatch:>>=  '$ctor:None  maybe->>=)
(register-instance-method! $dispatch:>>=  '$ctor:Some  maybe->>=)

;; List
(define (list-fmap f xs)
  (match xs
    [(Nil)        Nil]
    [(Cons h t)   (Cons (f h) (list-fmap f t))]))
(register-instance-method! $dispatch:fmap '$ctor:Nil   list-fmap)
(register-instance-method! $dispatch:fmap '$ctor:Cons  list-fmap)

;; Result e
(define result-fmap
  (lambda (f r)
    (match r
      [(Err x) (Err x)]
      [(Ok  v) (Ok (f v))])))
(register-instance-method! $dispatch:fmap '$ctor:Err   result-fmap)
(register-instance-method! $dispatch:fmap '$ctor:Ok    result-fmap)

(define result->>=
  (lambda (r f)
    (match r
      [(Err x) (Err x)]
      [(Ok  v) (f v)])))
(register-instance-method! $dispatch:>>=  '$ctor:Err   result->>=)
(register-instance-method! $dispatch:>>=  '$ctor:Ok    result->>=)

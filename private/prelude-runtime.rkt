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
                    [truncate   rkt:truncate]
                    [random     rkt:random]
                    [current-seconds rkt:current-seconds]
                    [getenv     rkt:getenv]
                    [current-command-line-arguments rkt:argv]
                    [path->string rkt:path->string]
                    [delete-file rkt:delete-file]
                    [make-directory rkt:make-directory]
                    [directory-list rkt:directory-list])
         (only-in racket/base
                  [string=? rkt:string=?])
         (only-in racket/string
                  [string-join   rkt:string-join])
         (only-in racket/base
                  [regexp-split  rkt:regexp-split]
                  [regexp-quote  rkt:regexp-quote])
         racket/format
         racket/match
         racket/file
         "adt.rkt"
         "dict.rkt")

(provide
 ;; ADTs (constructors usable as expressions and as match patterns)
 None Some Nil Cons MkPair Ok Err MkUnit
 MkSum MkProduct
 get-sum get-product

 ;; Class methods
 +  -  *
 ==  /=
 <  >  <=  >=
 show
 fmap
 <*> liftA2
 >>=
 bimap first second
 foldr length to-list sum
 <>
 traverse
 mconcat

 ;; Return-typed class methods (resolved at compile time per call site;
 ;; the `$pure:TCon` names are what the codegen emits after resolution).
 |$pure:Maybe| |$pure:List| |$pure:Result| |$pure:IO|
 |$mempty:String| |$mempty:List| |$mempty:Sum| |$mempty:Product|

 ;; Dispatch tables — exposed so user modules that declare new
 ;; instances (including derived ones) can register against them.
 $dispatch:+  $dispatch:-  $dispatch:*
 $dispatch:== $dispatch:/=
 $dispatch:<  $dispatch:>  $dispatch:<= $dispatch:>=
 $dispatch:show
 $dispatch:fmap
 $dispatch:<*>
 $dispatch:liftA2
 $dispatch:>>=
 $dispatch:bimap
 $dispatch:first
 $dispatch:second
 $dispatch:foldr
 $dispatch:length
 $dispatch:to-list
 $dispatch:<>
 $dispatch:traverse
 $dispatch:float-div

 ;; Combinators
 id compose flip const

 ;; Stdlib
 not and or filter

 ;; Strings
 string-length string-append substring
 string-prefix? string-split string-join
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
 try raise-io

 ;; System surface
 random-integer random-float current-time-seconds
 list-directory getenv argv delete-file make-directory)

;; ----- ADTs -------------------------------------------------------

(define-data-ctor None 0)
(define-data-ctor Some 1)

(define-data-ctor Nil  0)
(define-data-ctor Cons 2)

(define-data-ctor MkPair 2)

(define-data-ctor Ok  1)
(define-data-ctor Err 1)

(define-data-ctor MkUnit 0)

(define-data-ctor MkSum     1)
(define-data-ctor MkProduct 1)

;; ----- Class dispatch tables -------------------------------------

;; Each dispatch table comes with (pos arity) so the wrapper can decide
;; when enough arguments have been collected.  See define-class-method
;; in private/dict.rkt — it accepts both as exact-nonnegative-integers.
(define $dispatch:+  (make-hasheq))  (define-class-method +  $dispatch:+  0 2)
(define $dispatch:-  (make-hasheq))  (define-class-method -  $dispatch:-  0 2)
(define $dispatch:*  (make-hasheq))  (define-class-method *  $dispatch:*  0 2)
(define $dispatch:== (make-hasheq))  (define-class-method == $dispatch:== 0 2)
(define $dispatch:/= (make-hasheq))  (define-class-method /= $dispatch:/= 0 2)
(define $dispatch:<  (make-hasheq))  (define-class-method <  $dispatch:<  0 2)
(define $dispatch:>  (make-hasheq))  (define-class-method >  $dispatch:>  0 2)
(define $dispatch:<= (make-hasheq))  (define-class-method <= $dispatch:<= 0 2)
(define $dispatch:>= (make-hasheq))  (define-class-method >= $dispatch:>= 0 2)
(define $dispatch:show (make-hasheq))(define-class-method show $dispatch:show 0 1)
;; Functor's fmap dispatches on the SECOND argument (the container).
(define $dispatch:fmap (make-hasheq))(define-class-method fmap $dispatch:fmap 1 2)
;; Applicative's <*> dispatches on the FIRST argument (the f (a->b)).
;; liftA2 is provided via the class's default method body.
(define $dispatch:<*>    (make-hasheq))(define-class-method <*>    $dispatch:<*>    0 2)
(define $dispatch:liftA2 (make-hasheq))(define-class-method liftA2 $dispatch:liftA2 1 3)
;; Monad's bind dispatches on the FIRST argument (the wrapped value).
(define $dispatch:>>=  (make-hasheq))(define-class-method >>=  $dispatch:>>=  0 2)
;; Bifunctor's bimap/first/second dispatch on the value (the `p a b`).
;; bimap takes 3 args, value is arg 2.  first/second take 2 args, value is arg 1.
(define $dispatch:bimap  (make-hasheq))(define-class-method bimap  $dispatch:bimap  2 3)
(define $dispatch:first  (make-hasheq))(define-class-method first  $dispatch:first  1 2)
(define $dispatch:second (make-hasheq))(define-class-method second $dispatch:second 1 2)
;; Foldable's foldr dispatches on the container (arg 2). length/to-list
;; dispatch on the container as arg 0.
(define $dispatch:foldr   (make-hasheq))(define-class-method foldr   $dispatch:foldr   2 3)
(define $dispatch:length  (make-hasheq))(define-class-method length  $dispatch:length  0 1)
(define $dispatch:to-list (make-hasheq))(define-class-method to-list $dispatch:to-list 0 1)
;; Semigroup's <> dispatches on the first arg (the value carrying the
;; Semigroup-constrained type).
(define $dispatch:<> (make-hasheq))(define-class-method <> $dispatch:<> 0 2)
;; Traversable's traverse: user sees (a -> f b) -> t a -> f (t b), so
;; the t-container is at user-position 1.  The elaborator prepends one
;; dict argument (the resolved pure-impl), shifting dispatch position
;; to 2 and total arity to 3.
(define $dispatch:traverse (make-hasheq))
(define-class-method traverse $dispatch:traverse 2 3)

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
(define/curried (compose f g) (lambda (x) (f (g x))))
(define (flip f) (lambda (x y) (f y x)))
(define (const x) (lambda (_y) x))

;; ----- Stdlib ----------------------------------------------------

(define (not b) (if b #f #t))
(define/curried (and a b) (if a b #f))
(define/curried (or  a b) (if a #t b))

(define (filter p xs)
  (match xs
    [(Nil)        Nil]
    [(Cons h t)   (if (p h) (Cons h (filter p t)) (filter p t))]))

;; ----- Strings -------------------------------------------------

(define (string-length s) (rkt:string-length s))
(define/curried (string-append a b) (rkt:string-append a b))
(define/curried (substring s start end) (rkt:substring s start end))

(define/curried (string-prefix? p s)
  (and (rkt:<= (rkt:string-length p) (rkt:string-length s))
       (rkt:string=? p (rkt:substring s 0 (rkt:string-length p)))))

(define/curried (string-split sep s)
  ;; Split `s` on every occurrence of `sep`.  Behaves like Racket's
  ;; `regexp-split` on a literal regex but takes a plain string.
  (rkt-list->rackton
   (rkt:regexp-split (rkt:regexp-quote sep) s)))

(define/curried (string-join sep ss)
  ;; Inverse of string-split.  Collect the Rackton list down to a
  ;; Racket list, then use `string-join` from racket/string.
  (let loop ([ss ss] [acc '()])
    (match ss
      [(Nil)        (rkt:string-join (rkt:reverse acc) sep)]
      [(Cons h t)   (loop t (cons h acc))])))

;; A variadic concatenation used by derived Show instances.  The
;; Rackton-typed `string-append` is binary; this helper sidesteps the
;; binary signature for codegen-emitted strings.
(define $show-concat
  (lambda strs (apply rkt:string-append strs)))

;; ----- Numeric helpers -----------------------------------------

(define/curried (mod a b) (rkt:modulo a b))
(define/curried (div a b) (rkt:quotient a b))
(define (abs n) (rkt:abs n))
(define/curried (min a b) (rkt:min a b))
(define/curried (max a b) (rkt:max a b))
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

;; ----- pure (return-typed Applicative method) ---------------------
;; The Rackton-level `pure` is resolved at compile time by the
;; elaborator (see resolve-method-uses! in private/infer.rkt) to one
;; of these per-instance names based on the expected return type at
;; each call site.
(define (|$pure:Maybe|  x) (Some x))
(define (|$pure:List|   x) (Cons x Nil))
(define (|$pure:Result| x) (Ok x))
(define (|$pure:IO|     x) ($io (lambda () x)))

;; ----- Monoid mempty (return-typed) -------------------------------
(define |$mempty:String|  "")
(define |$mempty:List|    Nil)
(define |$mempty:Sum|     (MkSum     0))
(define |$mempty:Product| (MkProduct 1))

(define (get-sum     s) (match s [(MkSum n) n]))
(define (get-product p) (match p [(MkProduct n) n]))

;; ----- Semigroup <> -----------------------------------------------
(define (semigroup-list-<> xs ys)
  (match xs
    [(Nil)      ys]
    [(Cons h t) (Cons h (semigroup-list-<> t ys))]))
(register-instance-method! $dispatch:<> 'String
                           (lambda (a b) (rkt:string-append a b)))
(register-instance-method! $dispatch:<> '$ctor:Nil  semigroup-list-<>)
(register-instance-method! $dispatch:<> '$ctor:Cons semigroup-list-<>)
(register-instance-method! $dispatch:<> '$ctor:MkSum
                           (lambda (a b)
                             (match a [(MkSum x)
                                       (match b [(MkSum y) (MkSum (rkt:+ x y))])])))
(register-instance-method! $dispatch:<> '$ctor:MkProduct
                           (lambda (a b)
                             (match a [(MkProduct x)
                                       (match b [(MkProduct y) (MkProduct (rkt:* x y))])])))

;; ----- Traversable traverse ---------------------------------------
;; Each impl receives the resolved `pure-impl` as its leading argument
;; (the elaborator inserts it based on the inferred `f`).  Inner
;; effectful combinators like `fmap`/`liftA2` dispatch on their value
;; arg at runtime and so work generically for any `f`.

(define (traverse-Maybe-impl pure-impl f m)
  (match m
    [(None)   (pure-impl None)]
    [(Some x) (fmap Some (f x))]))
(register-instance-method! $dispatch:traverse '$ctor:None traverse-Maybe-impl)
(register-instance-method! $dispatch:traverse '$ctor:Some traverse-Maybe-impl)

(define (traverse-List-impl pure-impl f xs)
  (match xs
    [(Nil)         (pure-impl Nil)]
    [(Cons h rest) (liftA2 Cons (f h) (traverse-List-impl pure-impl f rest))]))
(register-instance-method! $dispatch:traverse '$ctor:Nil  traverse-List-impl)
(register-instance-method! $dispatch:traverse '$ctor:Cons traverse-List-impl)

(define (io-fmap f io)
  ($io (lambda () (f (run-io io)))))

(define (io-ap iof iox)
  ($io (lambda ()
         (define f (run-io iof))
         (define a (run-io iox))
         (f a))))

(define (io-liftA2 g x y)
  ($io (lambda ()
         (define a (run-io x))
         (define b (run-io y))
         (g a b))))

(define (io-bind io f)
  ($io (lambda () (run-io (f (run-io io))))))

(register-instance-method! $dispatch:fmap   '$io io-fmap)
(register-instance-method! $dispatch:<*>    '$io io-ap)
(register-instance-method! $dispatch:liftA2 '$io io-liftA2)
(register-instance-method! $dispatch:>>=    '$io io-bind)

;; ----- Mutable refs (in IO) -----------------------------------

(define (make-ref v) ($io (lambda () (box v))))
(define (read-ref r) ($io (lambda () (unbox r))))
(define/curried (write-ref r v) ($io (lambda () (set-box! r v) MkUnit)))

;; ----- File I/O ------------------------------------------------

(define (read-file path)
  ($io (lambda () (file->string path))))
(define/curried (write-file path contents)
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

(define/curried (append xs ys)
  (match xs
    [(Nil) ys]
    [(Cons h t) (Cons h (append t ys))]))

(define/curried (zip as bs)
  (match as
    [(Nil) Nil]
    [(Cons a at)
     (match bs
       [(Nil) Nil]
       [(Cons b bt) (Cons (MkPair a b) (zip at bt))])]))

(define/curried (take n xs)
  (cond
    [(rkt:<= n 0) Nil]
    [else
     (match xs
       [(Nil) Nil]
       [(Cons h t) (Cons h (take (rkt:- n 1) t))])]))

(define/curried (drop n xs)
  (cond
    [(rkt:<= n 0) xs]
    [else
     (match xs
       [(Nil) Nil]
       [(Cons _ t) (drop (rkt:- n 1) t)])]))

(define/curried (find p xs)
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

(define/curried (map-insert k v m) ($map (hash-set ($map-h m) k v)))
(define/curried (map-lookup k m)
  (cond [(hash-has-key? ($map-h m) k) (Some (hash-ref ($map-h m) k))]
        [else None]))
(define/curried (map-delete k m) ($map (hash-remove ($map-h m) k)))

;; Build a rackton List from a Racket sequence.
(define (rkt-seq->list xs)
  (let loop ([xs (reverse xs)] [acc Nil])
    (cond [(null? xs) acc]
          [else (loop (cdr xs) (Cons (car xs) acc))])))

(define (map-keys   m) (rkt-seq->list (hash-keys   ($map-h m))))
(define (map-values m) (rkt-seq->list (hash-values ($map-h m))))
(define (map-size   m) (hash-count ($map-h m)))

(define/curried (map-fold f z m)
  (for/fold ([acc z]) ([(k v) (in-hash ($map-h m))])
    (((f k) v) acc)))

;; ----- Immutable Set ------------------------------------------

(define/curried (set-insert x s) ($set (hash-set ($set-h s) x #t)))
(define/curried (set-member? x s) (hash-has-key? ($set-h s) x))
(define/curried (set-delete x s) ($set (hash-remove ($set-h s) x)))
(define (set-size s) (hash-count ($set-h s)))
(define (set-to-list s) (rkt-seq->list (hash-keys ($set-h s))))

;; ----- List helpers -------------------------------------------

(define/curried (concat-map f xs)
  (foldr (lambda (x acc) (append (f x) acc)) Nil xs))

(define/curried (group-by key xs)
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
(define-class-method float-div $dispatch:float-div 0 2)
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

;; ----- System surface ----------------------------------------

(define/curried (random-integer lo hi)
  ($io (lambda () (rkt:random lo hi))))

(define random-float
  ($io (lambda () (rkt:exact->inexact (rkt:random)))))

(define current-time-seconds
  ($io (lambda () (rkt:current-seconds))))

(define (rkt-list->rackton lst)
  (let loop ([lst (rkt:reverse lst)] [acc Nil])
    (cond
      [(null? lst) acc]
      [else (loop (cdr lst) (Cons (car lst) acc))])))

(define (list-directory path)
  ($io (lambda ()
         (rkt-list->rackton
          (map rkt:path->string (rkt:directory-list path))))))

(define (getenv name)
  ($io (lambda ()
         (define v (rkt:getenv name))
         (if v (Some v) None))))

(define argv
  ($io (lambda ()
         (rkt-list->rackton (vector->list (rkt:argv))))))

(define (delete-file path)
  ($io (lambda () (rkt:delete-file path) MkUnit)))

(define (make-directory path)
  ($io (lambda () (rkt:make-directory path) MkUnit)))

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

;; ----- Applicative instances ------------------------------------

;; Maybe
(define maybe-ap
  (lambda (mf mx)
    (match mf
      [(None)   None]
      [(Some f) (fmap f mx)])))
(register-instance-method! $dispatch:<*> '$ctor:None maybe-ap)
(register-instance-method! $dispatch:<*> '$ctor:Some maybe-ap)

;; Per-instance liftA2 impls (not derived from <*>/fmap so that
;; user-supplied multi-arg lambdas can be applied with both args at
;; once — see the note in prelude.rkt's Applicative class.)
(define (maybe-liftA2 g mx my)
  (match mx
    [(None)   None]
    [(Some x)
     (match my
       [(None)   None]
       [(Some y) (Some (g x y))])]))
(register-instance-method! $dispatch:liftA2 '$ctor:None maybe-liftA2)
(register-instance-method! $dispatch:liftA2 '$ctor:Some maybe-liftA2)

;; List — cartesian product semantics
(define (list-ap fs xs)
  (let cat ([fs fs])
    (match fs
      [(Nil)         Nil]
      [(Cons f rest)
       (let prepend ([mapped (list-fmap f xs)])
         (match mapped
           [(Nil)      (cat rest)]
           [(Cons h t) (Cons h (prepend t))]))])))
(register-instance-method! $dispatch:<*> '$ctor:Nil  list-ap)
(register-instance-method! $dispatch:<*> '$ctor:Cons list-ap)

(define (list-liftA2 g xs ys)
  ;; cartesian: for each x in xs, for each y in ys, (g x y).
  (let outer ([xs xs])
    (match xs
      [(Nil)        Nil]
      [(Cons x xr)
       (let inner ([ys ys])
         (match ys
           [(Nil)         (outer xr)]
           [(Cons y yr)   (Cons (g x y) (inner yr))]))])))
(register-instance-method! $dispatch:liftA2 '$ctor:Nil  list-liftA2)
(register-instance-method! $dispatch:liftA2 '$ctor:Cons list-liftA2)

;; Result e
(define result-ap
  (lambda (rf rx)
    (match rf
      [(Err e) (Err e)]
      [(Ok  f) (fmap f rx)])))
(register-instance-method! $dispatch:<*> '$ctor:Err result-ap)
(register-instance-method! $dispatch:<*> '$ctor:Ok  result-ap)

(define (result-liftA2 g rx ry)
  (match rx
    [(Err e) (Err e)]
    [(Ok x)
     (match ry
       [(Err e) (Err e)]
       [(Ok y)  (Ok (g x y))])]))
(register-instance-method! $dispatch:liftA2 '$ctor:Err result-liftA2)
(register-instance-method! $dispatch:liftA2 '$ctor:Ok  result-liftA2)

;; ----- Bifunctor instances --------------------------------------

(define pair-bimap
  (lambda (f g p)
    (match p
      [(MkPair x y) (MkPair (f x) (g y))])))
(register-instance-method! $dispatch:bimap '$ctor:MkPair pair-bimap)
(define pair-first  (lambda (f p) (pair-bimap f id      p)))
(define pair-second (lambda (g p) (pair-bimap id     g  p)))
(register-instance-method! $dispatch:first  '$ctor:MkPair pair-first)
(register-instance-method! $dispatch:second '$ctor:MkPair pair-second)

(define result-bimap
  (lambda (f g r)
    (match r
      [(Err e) (Err (f e))]
      [(Ok  v) (Ok  (g v))])))
(register-instance-method! $dispatch:bimap '$ctor:Err result-bimap)
(register-instance-method! $dispatch:bimap '$ctor:Ok  result-bimap)
(define result-first  (lambda (f r) (result-bimap f id    r)))
(define result-second (lambda (g r) (result-bimap id    g r)))
(register-instance-method! $dispatch:first  '$ctor:Err result-first)
(register-instance-method! $dispatch:first  '$ctor:Ok  result-first)
(register-instance-method! $dispatch:second '$ctor:Err result-second)
(register-instance-method! $dispatch:second '$ctor:Ok  result-second)

;; ----- Foldable instances ---------------------------------------

;; List
(define (list-foldr f z xs)
  (match xs
    [(Nil)         z]
    [(Cons h rest) (f h (list-foldr f z rest))]))
(register-instance-method! $dispatch:foldr '$ctor:Nil  list-foldr)
(register-instance-method! $dispatch:foldr '$ctor:Cons list-foldr)

;; Maybe
(define (maybe-foldr f z m)
  (match m
    [(None)   z]
    [(Some x) (f x z)]))
(register-instance-method! $dispatch:foldr '$ctor:None maybe-foldr)
(register-instance-method! $dispatch:foldr '$ctor:Some maybe-foldr)

;; Defaults expressed via dispatched foldr — registered once per ctor.
(define (default-length xs)
  (foldr (lambda (_x n) (rkt:+ n 1)) 0 xs))
(define (default-to-list xs)
  (foldr (lambda (x acc) (Cons x acc)) Nil xs))
(define (sum xs)
  (foldr (lambda (a b) (rkt:+ a b)) 0 xs))

;; ----- mconcat (free function with needs-dict signature) ----------
;; Receives the resolved `mempty` for `a` as a leading argument (the
;; elaborator inserts it at every call site based on Phase 24's
;; free-function detection in var-dict-requirements).  Inner `<>`
;; calls dispatch on the running accumulator's tag and remain
;; generic.
(define (mconcat mempty-impl xs)
  (foldr (lambda (x acc) (<> x acc)) mempty-impl xs))

(for ([tag (in-list '($ctor:Nil $ctor:Cons $ctor:None $ctor:Some))])
  (register-instance-method! $dispatch:length  tag default-length)
  (register-instance-method! $dispatch:to-list tag default-to-list))

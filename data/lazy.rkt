#lang rackton

;; rackton/data/lazy — first-class laziness for a strict language.
;;
;; `Lazy a` is an opaque, memoizing deferred computation: build one with
;; the `delay` form (`(delay e)` desugars to `(make-lazy (lambda (_) e))`,
;; so `e` is not evaluated at construction) and run it with `force`, which
;; evaluates at most once and caches the result (call-by-need).  The
;; runtime lives in rackton/private/lazy-runtime.
;;
;; `Stream a` is a lazy cons-list whose tail is deferred, so producers may
;; be infinite while consumers take only a finite prefix.

(provide (all-defined-out))

;; ----- Lazy --------------------------------------------------------

(data (Lazy a))

;; `make-lazy` is the target of the `delay` form; user code normally
;; writes `(delay e)` rather than calling it directly.
(foreign make-lazy (-> (-> Unit a) (Lazy a))
         #:from rackton/private/lazy-runtime)

;; Force a Lazy, computing it the first time and caching thereafter.
(foreign force (-> (Lazy a) a)
         #:from rackton/private/lazy-runtime #:as lazy-force)

;; ----- Stream ------------------------------------------------------
;;
;; A lazy cons-list: the tail of `SCons` is a `Lazy`, so a producer can
;; be infinite while a consumer forces only the prefix it needs.

(data (Stream a)
  SNil
  (SCons a (Lazy (Stream a))))

;; First element, if any.
(: stream-head (-> (Stream a) (Maybe a)))
(define (stream-head s)
  (match s
    [(SNil)      None]
    [(SCons x _) (Some x)]))

;; Drop the first element (forces one tail).  Empty stays empty.
(: stream-tail (-> (Stream a) (Stream a)))
(define (stream-tail s)
  (match s
    [(SNil)       SNil]
    [(SCons _ lz) (force lz)]))

;; The first `n` elements as a strict List; forces only the tails needed.
(: stream-take (-> Integer (-> (Stream a) (List a))))
(define (stream-take n s)
  (if (<= n 0)
      Nil
      (match s
        [(SNil)       Nil]
        [(SCons x lz) (Cons x (stream-take (- n 1) (force lz)))])))

;; Map a function over a stream, keeping the tail deferred.
(: stream-map (-> (-> a b) (-> (Stream a) (Stream b))))
(define (stream-map f s)
  (match s
    [(SNil)       SNil]
    [(SCons x lz) (SCons (f x) (delay (stream-map f (force lz))))]))

;; Keep only elements satisfying `p`, lazily.
(: stream-filter (-> (-> a Boolean) (-> (Stream a) (Stream a))))
(define (stream-filter p s)
  (match s
    [(SNil) SNil]
    [(SCons x lz)
     (if (p x)
         (SCons x (delay (stream-filter p (force lz))))
         (stream-filter p (force lz)))]))

;; Concatenate two streams lazily.
(: stream-append (-> (Stream a) (-> (Stream a) (Stream a))))
(define (stream-append xs ys)
  (match xs
    [(SNil)       ys]
    [(SCons x lz) (SCons x (delay (stream-append (force lz) ys)))]))

;; ----- Stream producers (infinite) ---------------------------------

;; An infinite stream of a single repeated value.
(: stream-repeat (-> a (Stream a)))
(define (stream-repeat x) (SCons x (delay (stream-repeat x))))

;; x, (f x), (f (f x)), …
(: stream-iterate (-> (-> a a) (-> a (Stream a))))
(define (stream-iterate f x) (SCons x (delay (stream-iterate f (f x)))))

;; n, n+1, n+2, …
(: stream-from (-> Integer (Stream Integer)))
(define (stream-from n) (SCons n (delay (stream-from (+ n 1)))))

;; A finite stream from a List.
(: list->stream (-> (List a) (Stream a)))
(define (list->stream xs)
  (match xs
    [(Nil)       SNil]
    [(Cons h t)  (SCons h (delay (list->stream t)))]))

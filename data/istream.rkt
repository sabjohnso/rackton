#lang rackton

;; rackton/data/istream — a guaranteed-infinite stream.  Where `Stream`
;; from rackton/data/lazy has an empty case (`SNil`), an `IStream` has
;; ONLY a cons — so it never ends.  Consequently `istream-head` and
;; `istream-tail` are total, and `istream-take n` always returns exactly
;; `n` elements.  The tail is a `Lazy IStream`, so the stream is
;; productive: a consumer forces only the prefix it inspects.
;;
;; IStream is the canonical infinite Comonad — the dual of the list-monad
;; `Stream`: `extract` is the head, `duplicate` is the infinite stream of
;; all tails.  Its `FunctorApply` zips positionwise (and, both operands
;; being infinite, never truncates), which is what makes `ComonadApply`
;; agree with the comonad.
;;
;; It is deliberately NOT a cartesian `Monad`: `flatmap` over an infinite
;; stream would concatenate infinitely many infinite streams and never
;; advance past the first element — it would diverge, never productive.
;; (Its only lawful monad is the exotic diagonal one, which we omit.)
;; There is likewise no `filter`, since filtering cannot promise an
;; infinite result; route through `istream->stream` then `stream-filter`.

(require rackton/data/lazy
         rackton/control/apply
         rackton/control/comonad)

(provide (all-defined-out))

(data (IStream a) (ICons a (Lazy (IStream a))))

;; The head — always present, so total.
(: istream-head (-> (IStream a) a))
(define (istream-head s) (match s [(ICons h _) h]))

;; Everything after the head — again an infinite stream, so total.
(: istream-tail (-> (IStream a) (IStream a)))
(define (istream-tail s) (match s [(ICons _ lz) (force lz)]))

;; The first `n` elements as a strict `List`; for `n >= 0` always
;; exactly `n`, since the stream never runs out.
(: istream-take (-> Integer (-> (IStream a) (List a))))
(define (istream-take n s)
  (if (<= n 0)
      Nil
      (match s [(ICons h lz) (Cons h (istream-take (- n 1) (force lz)))])))

;; Map over every element, keeping the tail deferred.
(: istream-map (-> (-> a b) (-> (IStream a) (IStream b))))
(define (istream-map f s)
  (match s [(ICons h lz) (ICons (f h) (delay (istream-map f (force lz))))]))

;; Combine two infinite streams positionwise with a curried `f`.
(: istream-zip-with (-> (-> a (-> b c)) (-> (IStream a) (-> (IStream b) (IStream c)))))
(define (istream-zip-with f xs ys)
  (match xs
    [(ICons x lzx)
     (match ys
       [(ICons y lzy)
        (ICons ((f x) y)
               (delay (istream-zip-with f (force lzx) (force lzy))))])]))

;; --- producers -------------------------------------------------------

;; A single value, repeated forever.
(: istream-repeat (-> a (IStream a)))
(define (istream-repeat x) (ICons x (delay (istream-repeat x))))

;; x, (f x), (f (f x)), …
(: istream-iterate (-> (-> a a) (-> a (IStream a))))
(define (istream-iterate f x) (ICons x (delay (istream-iterate f (f x)))))

;; n, n+1, n+2, …
(: istream-from (-> Integer (IStream Integer)))
(define (istream-from n) (ICons n (delay (istream-from (+ n 1)))))

;; Embed into the (possibly-empty) `Stream` type — always an `SCons`.
(: istream->stream (-> (IStream a) (Stream a)))
(define (istream->stream s)
  (match s [(ICons h lz) (SCons h (delay (istream->stream (force lz))))]))

;; --- class instances -------------------------------------------------
;;
;; Functor, the zippy FunctorApply, and the infinite Comonad
;; (extract = head, duplicate = the stream of all tails).  Methods are
;; written out in full because an imported class's default bodies do not
;; cross the module boundary, so an instance must supply a complete set.

(instance (Functor IStream)
  (define (fmap f s) (istream-map f s)))

(instance (FunctorApply IStream)
  ;; positionwise application — both operands infinite, so no truncation.
  (define (apply ff fx) (istream-zip-with (lambda (g) (lambda (x) (g x))) ff fx))
  (define (liftF2 g fa fb) (istream-zip-with g fa fb)))

(instance (Comonad IStream)
  (define (extract s) (match s [(ICons h _) h]))
  ;; duplicate = the infinite stream of all tails: this stream, then its
  ;; tail, then its tail's tail, …
  (define (duplicate s) (ICons s (delay (duplicate (istream-tail s)))))
  (define (extend f w) (istream-map f (duplicate w))))

(instance (ComonadApply IStream)
  (define (coapply ff fx) (apply ff fx)))

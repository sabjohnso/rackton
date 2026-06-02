#lang rackton

;; rackton/unit — laziness primitives.
;;
;; Tenet: integrated shrinking (Phase 2 onward) represents a generated
;; value together with its tree of shrink candidates.  That tree is
;; unbounded, and Rackton is strict, so the children must live behind a
;; thunk or construction would diverge.  `Lazy` is that thunk; `Stream`
;; is a lazy cons-list whose tail is deferred.  Both are ordinary
;; single-/multi-ctor `data` types — the deferral is entirely in the
;; `(-> Unit a)` field of `Lazy`, evaluated only by `force-lazy`.
;;
;; Public API: Lazy, Stream, force-lazy, delay-lazy, stream-take.

(provide (data-out Lazy)
         (data-out Stream)
         force-lazy
         delay-lazy
         stream-take
         stream-map
         stream-append)

;; A deferred computation.  Constructing `(Lazy th)` does not run
;; `th`; only `force-lazy` does.
(data (Lazy a) (Lazy (-> Unit a)))

;; A lazy cons-stream: the tail of `SCons` is wrapped in `Lazy`, so a
;; producer can be infinite while consumers take only a finite prefix.
(data (Stream a)
  SNil
  (SCons a (Lazy (Stream a))))

(: force-lazy (-> (Lazy a) a))
(define (force-lazy l)
  (match l
    [(Lazy th) (th Unit)]))

;; Wrap an already-evaluated value as a (trivially) deferred one.
(: delay-lazy (-> a (Lazy a)))
(define (delay-lazy x) (Lazy (lambda (_) x)))

;; The first `n` elements of a stream, as a strict List.  Forces only
;; the `n` tails it actually needs.
(: stream-take (-> Integer (-> (Stream a) (List a))))
(define (stream-take n s)
  (if (<= n 0)
      Nil
      (match s
        [(SNil)       Nil]
        [(SCons x lz) (Cons x (stream-take (- n 1) (force-lazy lz)))])))

;; Map over a stream, keeping it lazy (the tail stays deferred).
(: stream-map (-> (-> a b) (-> (Stream a) (Stream b))))
(define (stream-map f s)
  (match s
    [(SNil)       SNil]
    [(SCons x lz) (SCons (f x) (Lazy (lambda (_) (stream-map f (force-lazy lz)))))]))

;; Concatenate two streams lazily.
(: stream-append (-> (Stream a) (-> (Stream a) (Stream a))))
(define (stream-append xs ys)
  (match xs
    [(SNil)       ys]
    [(SCons x lz) (SCons x (Lazy (lambda (_) (stream-append (force-lazy lz) ys))))]))

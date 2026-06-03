#lang racket/base

;; Runtime impls for rackton/data/lazy — memoizing (call-by-need) thunks.
;;
;; Companion-runtime pattern (see private/containers-runtime.rkt): the
;; typed surface lives in the #lang rackton module rackton/data/lazy, the
;; hand-written Racket impls live here and are reached via `foreign`.
;;
;; A `Lazy` wraps a thunk and a memo cell.  The surface `delay` form
;; desugars to `(make-lazy (lambda (_) e))`, so the thunk is a one-arg
;; `(-> Unit a)` on the Rackton side; `lazy-force` calls it once with
;; Unit, caches the result, and returns the cached value on every later
;; force.  This is the deferred-computation analogue of the `$io` thunk
;; in prelude-runtime.rkt — but memoizing.

(require (only-in "prelude-runtime.rkt" Unit))

(provide make-lazy lazy-force)

(struct $lazy (thunk [value #:mutable] [forced? #:mutable]))

(define (make-lazy thunk) ($lazy thunk #f #f))

(define (lazy-force l)
  (if ($lazy-forced? l)
      ($lazy-value l)
      (let ([v (($lazy-thunk l) Unit)])
        (set-$lazy-value!  l v)
        (set-$lazy-forced?! l #t)
        v)))

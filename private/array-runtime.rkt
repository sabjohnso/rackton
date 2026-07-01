#lang racket/base

;; Rackton — fixed-size array runtime: the REPRESENTATION INTERFACE.
;;
;; This module is the single home of an array's memory layout.  Codegen
;; (and any sibling) touches arrays only through the operations exported
;; here, never through raw vector calls — so the backing representation
;; can be reimplemented (strided, lazy, contiguous, …) without changing
;; codegen, the type system, or any user program.
;;
;; First implementation: an opaque struct wrapping a Racket vector.  The
;; struct (rather than a bare vector) keeps array VALUES distinct from
;; tuples — which are bare vectors — at runtime dispatch (private/dict.rkt).
;;
;;   rackton-array-from-list : (listof a)        -> (Array n a)   ; listing
;;   rackton-array-make      : nat (-> nat a)    -> (Array n a)   ; sized builder
;;   rackton-array-ref       : (Array n a) nat   -> a            ; element read
;;   rackton-array-length    : (Array n a)       -> nat          ; element count

(provide rackton-array?
         rackton-array-from-list
         rackton-array-make
         rackton-array-ref
         rackton-array-length
         rackton-array-take
         rackton-array-drop
         flatten-major
         flatten-minor
         array-map
         array-imap
         array-fold
         array-foldr
         array-rotate)

;; The handle.  `vec` is the current backing store; it is an
;; implementation detail and must not escape this module.  Prefab (not a
;; plain transparent struct) so its type has a single global identity:
;; an array built in one module instantiation — e.g. the REPL's eval
;; namespace — is still recognized by `rackton-array?` / the accessors
;; in another (the REPL's own instance), which a per-instantiation
;; transparent struct would not be.
(struct rkt-array (vec) #:prefab)

;; The SOLE constructor.  It freezes the backing vector immutable, so an
;; Array is a pure Tier-V value: nothing mutates it (no array-set!
;; exists), and an Array of Tier-V elements is serialization- and
;; place-eligible (see the "Runtime-representation tiers" section of the
;; developer guide).  Prefab structs cannot carry a coercing `#:guard`,
;; so the invariant lives here; every
;; array-building operation below routes through this (directly or via
;; `rackton-array-from-list`).
(define (array-of vec) (rkt-array (vector->immutable-vector vec)))

;; A predicate on the opaque handle (for display / dispatch), without
;; exposing the constructor or accessor.
(define (rackton-array? v) (rkt-array? v))

(define (rackton-array-from-list elems)
  (array-of (list->vector elems)))

(define (rackton-array-make n f)
  (array-of (build-vector n f)))

(define (rackton-array-ref a i)
  (vector-ref (rkt-array-vec a) i))

(define (rackton-array-length a)
  (vector-length (rkt-array-vec a)))

;; Collapse one level of nesting — `(Array n (Array m a))` → `(Array (* n
;; m) a)` — recovering n and m from the array lengths.  The two differ
;; only in element order:
;;   flatten-major — row-major / C-order: the OUTER index varies slowest,
;;     so each inner array is laid down whole, in turn.
;;   flatten-minor — column-major / Fortran-order: the OUTER index varies
;;     fastest, so we sweep down column j across every inner array first.
;; Both go through the representation interface (rackton-array-ref /
;; -length), so they are independent of the backing layout.
(define (flatten-major arr)
  (define n (rackton-array-length arr))
  (rackton-array-from-list
   (for*/list ([i (in-range n)]
               [j (in-range (rackton-array-length (rackton-array-ref arr i)))])
     (rackton-array-ref (rackton-array-ref arr i) j))))

;; Slicing: `take` keeps the first k elements, `drop` keeps the rest
;; from index k.  The inference layer guarantees 0 ≤ k ≤ length, so these
;; need no bounds guard.  (`split-at` is built at the codegen site as the
;; Pair of a take and a drop, so the tuple constructor stays in
;; prelude-runtime and this module needn't depend on it.)
(define (rackton-array-take a k)
  (rackton-array-from-list
   (for/list ([i (in-range k)]) (rackton-array-ref a i))))

(define (rackton-array-drop a k)
  (rackton-array-from-list
   (for/list ([i (in-range k (rackton-array-length a))]) (rackton-array-ref a i))))

;; Size-preserving map and a strict left fold.  Both recover the length
;; from the array, so they work at any (including polymorphic) size.
;; These are user-facing prelude functions with CURRIED types, so — like
;; a compiled Rackton lambda — they accept every prefix arity (full or
;; partial application, or being passed as a value).  `f` is itself
;; curried, so it is applied one argument at a time.
(define (array-map* f a)
  (rackton-array-from-list
   (for/list ([i (in-range (rackton-array-length a))]) (f (rackton-array-ref a i)))))
(define array-map
  (case-lambda
    [(f a) (array-map* f a)]
    [(f)   (lambda (a) (array-map* f a))]))

;; Indexed map: element `i` of the result is `(f i (ref a i))`.  `f` is
;; curried (`(-> Integer (-> a b))`), applied one argument at a time.
(define (array-imap* f a)
  (rackton-array-from-list
   (for/list ([i (in-range (rackton-array-length a))])
     ((f i) (rackton-array-ref a i)))))
(define array-imap
  (case-lambda
    [(f a) (array-imap* f a)]
    [(f)   (lambda (a) (array-imap* f a))]))

(define (array-fold* f z a)
  (for/fold ([acc z]) ([i (in-range (rackton-array-length a))])
    ((f acc) (rackton-array-ref a i))))
(define array-fold
  (case-lambda
    [(f z a) (array-fold* f z a)]
    [(f z)   (lambda (a) (array-fold* f z a))]
    [(f)     (lambda (z) (lambda (a) (array-fold* f z a)))]))

;; Cyclic rotation, size-preserving: result element `i` is input element
;; `(i + k) mod n`.  Positive `k` rotates left (brings element `k` to the
;; front), negative `k` rotates right; `k` wraps modulo the size.  Empty
;; arrays rotate to themselves.
(define (array-rotate* k a)
  (define n (rackton-array-length a))
  (cond
    [(= n 0) a]
    [else
     (define s (modulo k n))          ; 0..n-1 even for negative k
     (rackton-array-from-list
      (for/list ([i (in-range n)])
        (rackton-array-ref a (modulo (+ i s) n))))]))
(define array-rotate
  (case-lambda
    [(k a) (array-rotate* k a)]
    [(k)   (lambda (a) (array-rotate* k a))]))

;; Right fold: `f x0 (f x1 (… (f x_{n-1} z)))`.  `f` is curried
;; (`(-> a (-> b b))`), so it is applied one argument at a time.
(define (array-foldr* f z a)
  (let loop ([i (sub1 (rackton-array-length a))] [acc z])
    (cond
      [(< i 0) acc]
      [else (loop (sub1 i) ((f (rackton-array-ref a i)) acc))])))
(define array-foldr
  (case-lambda
    [(f z a) (array-foldr* f z a)]
    [(f z)   (lambda (a) (array-foldr* f z a))]
    [(f)     (lambda (z) (lambda (a) (array-foldr* f z a)))]))

(define (flatten-minor arr)
  (define n (rackton-array-length arr))
  (define m (if (> n 0) (rackton-array-length (rackton-array-ref arr 0)) 0))
  (rackton-array-from-list
   (for*/list ([j (in-range m)]
               [i (in-range n)])
     (rackton-array-ref (rackton-array-ref arr i) j))))

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

(provide rackton-array-from-list
         rackton-array-make
         rackton-array-ref
         rackton-array-length)

;; The opaque handle.  `vec` is the current backing store; it is an
;; implementation detail and must not escape this module.
(struct rkt-array (vec) #:transparent)

(define (rackton-array-from-list elems)
  (rkt-array (list->vector elems)))

(define (rackton-array-make n f)
  (rkt-array (build-vector n f)))

(define (rackton-array-ref a i)
  (vector-ref (rkt-array-vec a) i))

(define (rackton-array-length a)
  (vector-length (rkt-array-vec a)))

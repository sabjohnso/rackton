#lang racket/base

;; Phase 5 — fixed-size arrays.  `(Array n a)` is an array of `n`
;; elements of type `a`, with the size `n` carried in the type as a
;; type-level Nat (see Phases 3–4).  Build one with the listing form
;; `(array e …)` (size = element count) or the sized builder
;; `(build-array n f)` (`n` a literal, `f : (-> Integer a)`); read an
;; element with `(aref arr i)` — `i` a literal, bounds-checked at compile
;; time against a concrete size.  The element layout is hidden behind the
;; representation interface in private/array-runtime.rkt.

(require rackunit
         (for-syntax racket/base)
         "../main.rkt")

(define-syntax-rule (check-rackton-compile-error form ...)
  (check-exn
   exn:fail?
   (lambda ()
     (eval #'(rackton form ...)
           (variable-reference->namespace (#%variable-reference))))))

;; ----- listing construction + indexed access ----------------------

(rackton
  (: a3 (Array 3 Integer))
  (define a3 (array 10 20 30))

  (: a3-0 Integer) (define a3-0 (aref a3 0))
  (: a3-2 Integer) (define a3-2 (aref a3 2)))

(test-case "array listing builds a sized array; aref reads elements"
  (check-equal? a3-0 10)
  (check-equal? a3-2 30))

;; ----- the sized builder -------------------------------------------

(rackton
  (: squares (Array 4 Integer))
  (define squares (build-array 4 (lambda (i) (* i i))))

  (: sq0 Integer) (define sq0 (aref squares 0))
  (: sq3 Integer) (define sq3 (aref squares 3)))

(test-case "build-array initializes each slot from its index"
  (check-equal? sq0 0)
  (check-equal? sq3 9))

;; ----- the size is the element type's only constraint -------------

(rackton
  (: strs (Array 2 String))
  (define strs (array "a" "b"))
  (: s1 String) (define s1 (aref strs 1)))

(test-case "arrays are homogeneous in the element type"
  (check-equal? s1 "b"))

;; ----- compile-time checks ----------------------------------------

(test-case "aref out of bounds (concrete size) is a compile error"
  (check-rackton-compile-error
   (: x Integer)
   (define x (aref (array 1 2 3) 5))))

(test-case "a non-literal aref index is a compile error"
  (check-rackton-compile-error
   (: x Integer)
   (define x (let ([i 0]) (aref (array 1 2 3) i)))))

(test-case "array size mismatch against an annotation is a type error"
  (check-rackton-compile-error
   (: x (Array 2 Integer))
   (define x (array 1 2 3))))

(test-case "a non-Nat array size is a kind error"
  (check-rackton-compile-error
   (: x (Array Integer Integer))
   (define x (array 1 2 3))))

(test-case "heterogeneous array elements are a type error"
  (check-rackton-compile-error
   (: x (Array 2 Integer))
   (define x (array 1 "two"))))

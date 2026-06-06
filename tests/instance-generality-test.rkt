#lang racket/base

;; An instance method's body must be at least as general as the class
;; method's declared signature.  A body that is *more specific* — one
;; that constrains the method's universally-quantified type variables or
;; the instance head's variables — must be rejected at compile time.
;;
;; Without this check an over-specific `fmap` (mapping over the wrong
;; field of a pair, or ignoring its function argument) would typecheck,
;; because the expected method type would be built from flexible
;; unification variables that simply collapse together.  These tests pin
;; the generality requirement and guard against over-rejecting correct
;; instances.

(require rackunit
         (for-syntax racket/base)
         "../main.rkt")

(define-syntax-rule (check-rackton-compile-error form ...)
  (check-exn
   exn:fail?
   (lambda ()
     (eval #'(rackton form ...)
           (variable-reference->namespace (#%variable-reference))))))

(define-syntax-rule (check-rackton-compiles form ...)
  (check-not-exn
   (lambda ()
     (eval #'(rackton form ...)
           (variable-reference->namespace (#%variable-reference))))))

;; --- the exact reported case: prelude Pair, no synthetic data type -----
;;
;; `(Functor (Pair a))` with `fmap` mapping the *fixed* first component
;; rather than the varying second one.  This pins both that the instance
;; is rejected and — via the partner positive case — that a correct
;; instance over the same prelude `Pair` still compiles, guarding against
;; the variable-capture defect (the class param `f := (Pair a)` must not
;; capture fmap's own quantified `a`).

(test-case "reported case: Functor (Pair a) over the prelude Pair, fixed field, rejected"
  (check-rackton-compile-error
   (instance (Functor (Pair a))
     (define (fmap f (Pair a b))
       (Pair (f a) b)))))

(test-case "reported case partner: Functor (Pair a) over the prelude Pair, varying field, compiles"
  (check-rackton-compiles
   (instance (Functor (Pair a))
     (define (fmap f (Pair a b))
       (Pair a (f b))))
   (define demo (fmap (lambda (x) (+ x 1)) (Pair "k" 41)))))

;; --- negative cases: over-specific instance methods --------------------

(test-case "fmap that ignores its function argument is rejected"
  (check-rackton-compile-error
   (data (Box a) (Box a))
   (instance (Functor Box)
     (define (fmap f (Box x)) (Box x)))))

(test-case "Functor (Pair a) mapping over the fixed field is rejected"
  (check-rackton-compile-error
   (data (Pair2 a b) (Pair2 a b))
   (instance (Functor (Pair2 a))
     (define (fmap f (Pair2 a b))
       (Pair2 (f a) b)))))

(test-case "Functor over an Either-like type mapping the fixed field is rejected"
  (check-rackton-compile-error
   (data (E a b) (L a) (R b))
   (instance (Functor (E a))
     (define (fmap f e)
       (match e
         [(L x) (L (f x))]
         [(R y) (R y)])))))

;; --- positive cases: correct instances still compile -------------------

(test-case "Functor (Pair a) mapping over the varying field compiles"
  (check-rackton-compiles
   (data (Pair2 a b) (Pair2 a b))
   (instance (Functor (Pair2 a))
     (define (fmap f (Pair2 a b))
       (Pair2 a (f b))))))

(test-case "Functor over an Either-like type mapping the varying field compiles"
  (check-rackton-compiles
   (data (E a b) (L a) (R b))
   (instance (Functor (E a))
     (define (fmap f e)
       (match e
         [(L x) (L x)]
         [(R y) (R (f y))])))))

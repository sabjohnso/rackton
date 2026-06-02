#lang racket/base

;; Confirms that ill-typed Rackton programs are rejected at *compile time*.

(require rackunit
         (for-syntax racket/base)
         "../main.rkt")

(define-syntax-rule (check-rackton-compile-error form ...)
  (check-exn
   exn:fail?
   (lambda ()
     ;; expand-to-top-form forces macro expansion of the rackton block
     ;; in this lexical context.
     (eval #'(rackton form ...)
           (variable-reference->namespace (#%variable-reference))))))

(test-case "if branches must agree"
  (check-rackton-compile-error
   (define x (if #t 1 "bad"))))

(test-case "if condition must be Boolean"
  (check-rackton-compile-error
   (define x (if 1 1 2))))

(test-case "applying a non-function fails"
  (check-rackton-compile-error
   (define x (1 2))))

(test-case "constructor arity is enforced"
  (check-rackton-compile-error
   (data (Maybe a) None (Some a))
   (define x (match None [(Some a b) a]))))

(test-case "polymorphic declaration is enforced (skolemization)"
  (check-rackton-compile-error
   (: bad (-> a a))
   (define (bad x) 0))) ; body specializes a to Integer

(test-case "non-exhaustive match on an ADT is rejected"
  (check-rackton-compile-error
   (data X A B C)
   (define (f x) (match x [A 1] [B 2])))) ; missing C

(test-case "non-exhaustive match on Boolean is rejected"
  (check-rackton-compile-error
   (define (f x) (match x [#t 1]))))

(test-case "match without catchall on Integer is rejected"
  (check-rackton-compile-error
   (define (f n) (match n [0 99]))))

(test-case "match with a wildcard is always exhaustive"
  (define (ok-rackton)
    (eval #'(rackton (define (f n) (match n [_ 99])))
          (variable-reference->namespace (#%variable-reference))))
  (check-not-exn ok-rackton))

(test-case "type-equality constraint rejects mismatched types"
  (check-rackton-compile-error
   (: pair-eq ((~ a b) => (-> a (-> b (Pair a b)))))
   (define (pair-eq x y) (Pair x y))
   (define bad (pair-eq 7 "hi"))))

(test-case "rank-N: monomorphic lambda rejected where polymorphic expected"
  ;; (lambda (x) 42) specializes its body to Integer regardless of x,
  ;; so it cannot inhabit (forall a. a -> a).
  (check-rackton-compile-error
   (: pair-id (-> (All (a) (-> a a)) (Pair Integer String)))
   (define (pair-id f) (Pair (f 7) (f "hi")))
   (define bad (pair-id (lambda (x) 42)))))

(test-case "rank-N: integer-only lambda rejected at polymorphic argument"
  ;; (lambda (x) (+ x 1)) specializes a to Integer; the second call
  ;; site (f "hi") needs a to be String, so this can't pass.
  (check-rackton-compile-error
   (: pair-id (-> (All (a) (-> a a)) (Pair Integer String)))
   (define (pair-id f) (Pair (f 7) (f "hi")))
   (define bad (pair-id (lambda (x) (+ x 1))))))

(test-case "associated type: instance missing #:type binding is rejected"
  (check-rackton-compile-error
   (protocol (Sized (c :: *))
     (#:type Index)
     (: size-of (-> c (Index c))))
   ;; No (#:type (Index = ...)) — instance is incomplete.
   (instance (Sized (List a))
     (define (size-of xs) (length xs)))))

(test-case "record update: unknown field is rejected"
  (check-rackton-compile-error
   (struct Point [x : Integer] [y : Integer])
   (define bad (update (Point 1 2) [z 5]))))

(test-case "record update: type mismatch on field value is rejected"
  (check-rackton-compile-error
   (struct Point [x : Integer] [y : Integer])
   (define bad (update (Point 1 2) [x "not-int"]))))

;; ----- superclass obligations on instances -------------------------

(test-case "instance missing a superclass instance is rejected (Ord needs Eq)"
  (check-rackton-compile-error
   (data Foo MkFoo)
   ;; No (instance (Eq Foo)) — Ord's Eq superclass is unsatisfied.
   (instance (Ord Foo)
     (define (< x y) #f))))

(test-case "instance missing a #:requires superclass is rejected"
  (check-rackton-compile-error
   (data (Box a) (MkBox a))
   (protocol (Pointed (w :: (-> * *)))
     (#:requires (Functor w))
     (: point (-> a (w a))))
   ;; No (instance (Functor Box)) — the #:requires constraint fails.
   (instance (Pointed Box)
     (define (point x) (MkBox x)))))

(test-case "instance with its superclass present is accepted"
  (check-not-exn
   (lambda ()
     (eval #'(rackton
              (data Foo MkFoo)
              (instance (Eq Foo)
                (define (== x y) #t))
              (instance (Ord Foo)
                (define (< x y) #f)))
           (variable-reference->namespace (#%variable-reference))))))

(test-case "parametric instance discharges its superclass via context"
  (check-not-exn
   (lambda ()
     (eval #'(rackton
              (data (Box a) (MkBox a))
              (instance ((Eq a) => (Eq (Box a)))
                (define (== x y) #t))
              (instance ((Ord a) => (Ord (Box a)))
                (define (< x y) #f)))
           (variable-reference->namespace (#%variable-reference))))))

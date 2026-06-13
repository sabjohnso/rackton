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

;; ArrowLoop ships no `(->)` instance — a strict function cannot tie the
;; recursive feedback knot — so `arrow-loop` over a plain function must
;; be rejected for want of an `(ArrowLoop (->))` instance.  (Category /
;; Arrow / ArrowChoice / ArrowApply over `(->)` all resolve fine; this
;; pins the deliberate ArrowLoop gap.)
(test-case "arrow-loop over a plain function has no instance"
  (check-rackton-compile-error
   (define (step p) (match p [(Pair a c) (Pair a c)]))
   (define run (arrow-loop step))))

;; proc `rec` desugars to `arrow-loop`, so a `rec` over the function
;; arrow is likewise rejected — there is no `(ArrowLoop (->))` instance.
(test-case "proc rec over the function arrow is rejected"
  (check-rackton-compile-error
   (: inc (-> Integer Integer))
   (define (inc x) (+ x 1))
   (: loopy (-> Integer Integer))
   (define loopy
     (proc (x)
       (rec [s <- (feed (arr inc) s)])
       (feed (arr inc) s)))))

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

;; ----- superclass existence (checked at protocol-definition time) -----

(test-case "a protocol's superclass must name a defined class"
  ;; Functr is uppercase (so it passes the syntactic class-name check)
  ;; but no such class exists; this must error at the protocol, not
  ;; silently compile.
  (check-rackton-compile-error
   (protocol (Foo (t => Functr))
     (: foo (-> (t a) (t a))))))

(test-case "an undefined #:requires superclass is rejected"
  (check-rackton-compile-error
   (protocol (Bar a)
     (#:requires (Nonesuch a))
     (: bar (-> a a)))))

(test-case "a forward-referenced superclass is fine"
  ;; Sub names Super before Super is declared; the check runs after all
  ;; classes are registered, so order does not matter.
  (check-not-exn
   (lambda ()
     (eval #'(rackton
              (protocol (Sub (a => Super)) (: subm (-> a a)))
              (protocol (Super a) (: superm (-> a a))))
           (variable-reference->namespace (#%variable-reference))))))

(test-case "a prelude class is a valid superclass"
  (check-not-exn
   (lambda ()
     (eval #'(rackton
              (protocol (MyOrd (a => Eq)) (: cmp (-> a (-> a Boolean)))))
           (variable-reference->namespace (#%variable-reference))))))

(test-case "the ~ equality predicate is not flagged as a missing superclass"
  (check-not-exn
   (lambda ()
     (eval #'(rackton
              (protocol (Same a b) (#:requires (~ a b)) (: same (-> a b))))
           (variable-reference->namespace (#%variable-reference))))))

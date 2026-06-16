#lang racket/base

;; The `#:laws` clause in a protocol body: named, quantified, type-checked
;; law declarations.  These are formal documentation attached to the
;; class — there is no runner here.  The tests pin two things:
;;
;;   - a well-formed `#:laws` clause elaborates (the quantifier may be
;;     written `All` or `∀`; binders carry per-binder type annotations;
;;     a law may use the class's own methods and any superclass method
;;     assumed by `#:requires`);
;;   - an ill-formed law is rejected at *compile time* (a non-Boolean
;;     body, an unbound binder, a method used at the wrong type, or an
;;     un-annotated binder).

(require rackunit
         (for-syntax racket/base)
         "../main.rkt")

;; Expand a rackton block in this lexical context, returning nothing.
;; Used both to assert that a block compiles (no exn) and, under
;; check-exn, that a block is rejected.
(define-syntax-rule (compile-rackton form ...)
  (eval #'(rackton form ...)
        (variable-reference->namespace (#%variable-reference))))

(define-syntax-rule (check-rackton-compile-error form ...)
  (check-exn exn:fail? (lambda () (compile-rackton form ...))))

;; ----- well-formed laws -----

(test-case "a law over the class's own method type-checks"
  (check-not-exn
   (lambda ()
     (compile-rackton
      (protocol (MyEq a)
        (: eqp (-> a (-> a Boolean)))
        #:laws
          ([reflexivity (All ([x : a]) (eqp x x))]))))))

(test-case "∀ is a synonym for All"
  (check-not-exn
   (lambda ()
     (compile-rackton
      (protocol (MyEq a)
        (: eqp (-> a (-> a Boolean)))
        #:laws
          ([reflexivity (∀ ([x : a]) (eqp x x))]))))))

(test-case "multiple laws in one clause, multiple binders each"
  (check-not-exn
   (lambda ()
     (compile-rackton
      (protocol (MyEq a)
        (: eqp (-> a (-> a Boolean)))
        #:laws
          ([reflexivity (All ([x : a]) (eqp x x))]
           [comparable  (All ([x : a] [y : a]) (eqp x y))]))))))

(test-case "a law may use a superclass method assumed via #:requires"
  (check-not-exn
   (lambda ()
     (compile-rackton
      (protocol (MySemigroup a)
        (#:requires (Eq a))
        (: combine (-> a (-> a a)))
        #:laws
          ([associativity
            (All ([x : a] [y : a] [z : a])
              (== (combine (combine x y) z)
                  (combine x (combine y z))))]))))))

(test-case "a law-local => context puts Eq in scope without a superprotocol"
  (check-not-exn
   (lambda ()
     (compile-rackton
      (protocol (MySemigroup2 a)
        (: combine (-> a (-> a a)))
        #:laws
          ([associativity ((Eq a) =>
            (All ([x : a] [y : a] [z : a])
              (== (combine (combine x y) z)
                  (combine x (combine y z)))))]))))))

(test-case "a higher-kinded law names the concrete-element Eq instance"
  (check-not-exn
   (lambda ()
     (compile-rackton
      (protocol (MyFunctor (f :: (-> * *)))
        (: fmap2 (-> (-> a b) (-> (f a) (f b))))
        #:laws
          ([identity ((Eq (f Integer)) =>
            (All ([xs : (f Integer)]) (== (fmap2 (lambda (x) x) xs) xs)))]))))))

(test-case "a higher-kinded law is generic over its element types"
  ;; The element type need not be concrete: a law may universally
  ;; quantify over element variables that are not class parameters and
  ;; assume an `Eq` for the container at those variables.  Each such
  ;; variable is skolemized, so the unifier cannot re-orient it and the
  ;; equation's goal stays in step with the assumed `(Eq (f a))`.
  (check-not-exn
   (lambda ()
     (compile-rackton
      (protocol (MyFunctor2 (f :: (-> * *)))
        (: fmap2 (-> (-> a b) (-> (f a) (f b))))
        #:laws
          ([identity ((Eq (f a)) =>
            (All ([xs : (f a)]) (== (fmap2 (lambda (x) x) xs) xs)))]))))))

(test-case "a generic higher-kinded law with a composed function on one side"
  ;; The composition law applies a quantified `(-> a b)` binder inside a
  ;; lambda on one side of the equation — `(lambda (x) (g (h x)))` — and
  ;; the plain `fmap g . fmap h` on the other.  This is the case the free
  ;; element variable broke: the lambda's result type must stay pinned to
  ;; the law's `c`, matching the `(Eq (f c))` hypothesis.
  (check-not-exn
   (lambda ()
     (compile-rackton
      (protocol (MyFunctor3 (f :: (-> * *)))
        (: fmap2 (-> (-> a b) (-> (f a) (f b))))
        #:laws
          ([composition ((Eq (f c)) =>
            (All ([g : (-> b c)] [h : (-> a b)] [xs : (f a)])
              (== (fmap2 (lambda (x) (g (h x))) xs)
                  (fmap2 g (fmap2 h xs)))))]))))))

(test-case "a generic higher-kinded law still needs its Eq in the context"
  ;; Skolemizing the element variable does not conjure an `Eq`: a generic
  ;; law that compares `(f a)` values without assuming `(Eq (f a))` is
  ;; still rejected, exactly as the concrete-element case is.
  (check-rackton-compile-error
   (protocol (MyFunctor4 (f :: (-> * *)))
     (: fmap2 (-> (-> a b) (-> (f a) (f b))))
     #:laws
       ([identity
         (All ([xs : (f a)]) (== (fmap2 (lambda (x) x) xs) xs))]))))

(test-case "a law may compare results at a concrete type whose instance is in scope"
  ;; succ-step compares Integers and adds with `+`, relying on the
  ;; prelude's (Eq Integer)/(Num Integer) — which exist only because
  ;; laws are checked after instance registration, not during the class
  ;; elaboration pass.
  (check-not-exn
   (lambda ()
     (compile-rackton
      (protocol (MyEnum a)
        (: to-int (-> a Integer))
        (: step (-> a a))
        #:laws
          ([step-increments
            (All ([x : a]) (== (to-int (step x)) (+ (to-int x) 1)))]))))))

;; ----- ill-formed laws are rejected at compile time -----

(test-case "comparing results without declaring Eq is rejected"
  (check-rackton-compile-error
   (protocol (MySemigroup3 a)
     (: combine (-> a (-> a a)))
     #:laws
       ([associativity (All ([x : a] [y : a] [z : a])
                         (== (combine (combine x y) z)
                             (combine x (combine y z))))]))))

(test-case "a law body that is not Boolean is rejected"
  (check-rackton-compile-error
   (protocol (MyEq a)
     (: eqp (-> a (-> a Boolean)))
     #:laws
       ([bad (All ([x : a]) x)]))))

(test-case "an unbound binder in a law body is rejected"
  (check-rackton-compile-error
   (protocol (MyEq a)
     (: eqp (-> a (-> a Boolean)))
     #:laws
       ([bad (All ([x : a]) (eqp x y))]))))

(test-case "a method used at the wrong type is rejected"
  (check-rackton-compile-error
   (protocol (MyEq a)
     (: eqp (-> a (-> a Boolean)))
     #:laws
       ([bad (All ([x : a] [n : Integer]) (eqp x n))]))))

(test-case "an un-annotated binder is rejected"
  (check-rackton-compile-error
   (protocol (MyEq a)
     (: eqp (-> a (-> a Boolean)))
     #:laws
       ([bad (All (x) (eqp x x))]))))

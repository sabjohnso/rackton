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
        (#:laws
          ([reflexivity (All ([x : a]) (eqp x x))])))))))

(test-case "∀ is a synonym for All"
  (check-not-exn
   (lambda ()
     (compile-rackton
      (protocol (MyEq a)
        (: eqp (-> a (-> a Boolean)))
        (#:laws
          ([reflexivity (∀ ([x : a]) (eqp x x))])))))))

(test-case "multiple laws in one clause, multiple binders each"
  (check-not-exn
   (lambda ()
     (compile-rackton
      (protocol (MyEq a)
        (: eqp (-> a (-> a Boolean)))
        (#:laws
          ([reflexivity (All ([x : a]) (eqp x x))]
           [comparable  (All ([x : a] [y : a]) (eqp x y))])))))))

(test-case "a law may use a superclass method assumed via #:requires"
  (check-not-exn
   (lambda ()
     (compile-rackton
      (protocol (MySemigroup a)
        (#:requires (Eq a))
        (: combine (-> a (-> a a)))
        (#:laws
          ([associativity
            (All ([x : a] [y : a] [z : a])
              (== (combine (combine x y) z)
                  (combine x (combine y z))))])))))))

;; ----- ill-formed laws are rejected at compile time -----

(test-case "a law body that is not Boolean is rejected"
  (check-rackton-compile-error
   (protocol (MyEq a)
     (: eqp (-> a (-> a Boolean)))
     (#:laws
       ([bad (All ([x : a]) x)])))))

(test-case "an unbound binder in a law body is rejected"
  (check-rackton-compile-error
   (protocol (MyEq a)
     (: eqp (-> a (-> a Boolean)))
     (#:laws
       ([bad (All ([x : a]) (eqp x y))])))))

(test-case "a method used at the wrong type is rejected"
  (check-rackton-compile-error
   (protocol (MyEq a)
     (: eqp (-> a (-> a Boolean)))
     (#:laws
       ([bad (All ([x : a] [n : Integer]) (eqp x n))])))))

(test-case "an un-annotated binder is rejected"
  (check-rackton-compile-error
   (protocol (MyEq a)
     (: eqp (-> a (-> a Boolean)))
     (#:laws
       ([bad (All (x) (eqp x x))])))))

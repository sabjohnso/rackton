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
   (define-data (Maybe a) None (Some a))
   (define x (match None [(Some a b) a]))))

(test-case "polymorphic declaration is enforced (skolemization)"
  (check-rackton-compile-error
   (: bad (-> a a))
   (define (bad x) 0))) ; body specializes a to Integer

(test-case "non-exhaustive match on an ADT is rejected"
  (check-rackton-compile-error
   (define-data X A B C)
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

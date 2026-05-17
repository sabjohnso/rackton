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

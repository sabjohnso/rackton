#lang racket/base

;; Sealed abstract types + module-level coherence.

(require rackunit
         (for-syntax racket/base)
         "../main.rkt")

(define-syntax-rule (check-rackton-compile-error form ...)
  (check-exn
   exn:fail?
   (lambda ()
     (eval #'(rackton form ...)
           (variable-reference->namespace (#%variable-reference))))))

(rackton
  (require "sealed-abstract-types-lib-counter.rkt")

  ;; Public API works.
  (: c0 Counter)
  (define c0 (make-counter 7))

  (: c1 Counter)
  (define c1 (inc-counter c0))

  (: v Integer)
  (define v (counter-value c1)))

(test-case "abstract type: public API works across modules"
  (check-equal? v 8))

(test-case "abstract type: ctor MkCounter NOT exported"
  ;; The ctor isn't re-exported, so a client can't construct or
  ;; pattern-match against it.
  (check-rackton-compile-error
   (require "sealed-abstract-types-lib-counter.rkt")
   (define bad (MkCounter 99))))

(test-case "abstract type: ctor pattern in match is rejected"
  (check-rackton-compile-error
   (require "sealed-abstract-types-lib-counter.rkt")
   (define (peek c) (match c [(MkCounter n) n]))))

(test-case "module coherence: importing two modules that both declare Eq for the same type is rejected"
  (check-rackton-compile-error
   (require "sealed-abstract-types-lib-eq-a.rkt")
   (require "sealed-abstract-types-lib-eq-b.rkt")))

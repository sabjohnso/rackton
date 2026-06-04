#lang racket/base

;; A multi-parameter type class needs as many type arguments in an
;; instance head as the class was declared with.  `Arrow` has two
;; parameters — the arrow `cat` and its product `p` — so the prelude
;; instance is written `(Arrow (->) Pair)`.
;;
;; Writing only `(Arrow (Kleisli m))` (omitting the product) used to
;; type-check the head and then fail deep inside a method body with a
;; mismatch against an undetermined skolem `p` — a message that blamed
;; the body for an under-applied head.  These tests pin a head-level
;; diagnostic that names the class and the expected argument count.

(require rackunit
         (for-syntax racket/base)
         "../main.rkt")

(define-syntax-rule (catch-rackton-error form ...)
  (with-handlers ([exn:fail? exn-message])
    (eval #'(rackton form ...)
          (variable-reference->namespace (#%variable-reference)))
    (error 'catch-rackton-error "expected an error, none raised")))

(test-case "under-applied Arrow instance head reports the missing type argument"
  (define msg
    (catch-rackton-error
     (data (Kleisli m a b) (Kleisli (-> a (m b))))
     (instance ((Monad m) => (Category (Kleisli m)))
       (define ident (Kleisli pure))
       (define (comp (Kleisli f) (Kleisli g))
         (Kleisli (lambda (x) (flatmap f (g x))))))
     (instance ((Monad m) => (Arrow (Kleisli m)))
       (define (arr f) (Kleisli (compose pure f))))))
  ;; The message must name the class and explain the arity, and must
  ;; NOT be the old confusing method-body mismatch about an `p` skolem.
  (check-regexp-match #rx"Arrow" msg)
  (check-regexp-match #rx"argument" msg)
  (check-regexp-match #rx"2" msg)
  (check-false (regexp-match? #rx"method .* body has type" msg)))

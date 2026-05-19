#lang racket/base

;; Phase-19: Semigroup + Monoid (the second customer for return-typed
;; dispatch from Phase 18).

(require rackunit
         (for-syntax racket/base)
         "../main.rkt")

(rackton
  ;; ----- Semigroup <> --------------------------------------
  (: greet String)
  (define greet (<> "hello, " "world"))

  (: ones-and-twos (List Integer))
  (define ones-and-twos (<> (Cons 1 Nil) (Cons 2 (Cons 3 Nil))))

  ;; ----- Monoid mempty -------------------------------------
  (: empty-string String)
  (define empty-string (ann mempty String))

  (: empty-list (List Integer))
  (define empty-list (ann mempty (List Integer)))

  ;; ----- Monoid identity laws ------------------------------
  (: left-id (-> String String))
  (define (left-id s) (<> (ann mempty String) s))

  (: right-id (-> String String))
  (define (right-id s) (<> s (ann mempty String)))

  (: left-id-list (-> (List Integer) (List Integer)))
  (define (left-id-list xs) (<> (ann mempty (List Integer)) xs))

  ;; ----- Partial application of <> --------------------------
  (: prefixer (-> String String))
  (define prefixer (<> "[!] "))

  (: warned String)
  (define warned (prefixer "danger")))

;; ---------- assertions ----------------------------------------

(test-case "Semigroup <> on String"
  (check-equal? greet "hello, world"))

(test-case "Semigroup <> on List"
  (check-equal? ones-and-twos
                (Cons 1 (Cons 2 (Cons 3 Nil)))))

(test-case "Monoid mempty on String"
  (check-equal? empty-string ""))

(test-case "Monoid mempty on List"
  (check-equal? empty-list Nil))

(test-case "Monoid left identity on String"
  (check-equal? (left-id "x") "x")
  (check-equal? (right-id "y") "y"))

(test-case "Monoid left identity on List"
  (check-equal? (left-id-list (Cons 1 (Cons 2 Nil)))
                (Cons 1 (Cons 2 Nil))))

(test-case "Partial application of <>"
  (check-equal? warned "[!] danger"))

;; ----- ambiguity rejected at compile time -------------------

(define-syntax-rule (check-rackton-compile-error form ...)
  (check-exn
   exn:fail?
   (lambda ()
     (eval #'(rackton form ...)
           (variable-reference->namespace (#%variable-reference))))))

(test-case "mempty with no type context is rejected"
  (check-rackton-compile-error
   (define x mempty)))

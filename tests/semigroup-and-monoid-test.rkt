#lang racket/base

;; Semigroup + Monoid (the second customer for return-typed dispatch).

(require rackunit
         (for-syntax racket/base)
         "../main.rkt")

(rackton
  ;; ----- Semigroup mappend --------------------------------------
  (: greet String)
  (define greet (mappend "hello, " "world"))

  (: ones-and-twos (List Integer))
  (define ones-and-twos (mappend (Cons 1 Nil) (Cons 2 (Cons 3 Nil))))

  ;; ----- Monoid mempty -------------------------------------
  (: empty-string String)
  (define empty-string (ann mempty String))

  (: empty-list (List Integer))
  (define empty-list (ann mempty (List Integer)))

  ;; ----- Monoid identity laws ------------------------------
  (: left-id (-> String String))
  (define (left-id s) (mappend (ann mempty String) s))

  (: right-id (-> String String))
  (define (right-id s) (mappend s (ann mempty String)))

  (: left-id-list (-> (List Integer) (List Integer)))
  (define (left-id-list xs) (mappend (ann mempty (List Integer)) xs))

  ;; ----- Partial application of mappend --------------------------
  (: prefixer (-> String String))
  (define prefixer (mappend "[!] "))

  (: warned String)
  (define warned (prefixer "danger")))

;; ---------- assertions ----------------------------------------

(test-case "Semigroup mappend on String"
  (check-equal? greet "hello, world"))

(test-case "Semigroup mappend on List"
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

(test-case "Partial application of mappend"
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

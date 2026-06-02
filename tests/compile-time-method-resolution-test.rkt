#lang racket/base

;; Free-function compile-time resolution + mconcat.

(require rackunit
         (for-syntax racket/base)
         "../main.rkt")

(rackton
  (require rackton/data/monoid)
  ;; ----- mconcat over String --------------------------------
  (: glued String)
  (define glued
    (mconcat (Cons "a" (Cons "b" (Cons "c" Nil)))))

  (: empty-strs String)
  (define empty-strs (mconcat (ann Nil (List String))))

  ;; ----- mconcat over (List Integer) — Monoid (List a) ------
  (: flat (List Integer))
  (define flat
    (mconcat (Cons (Cons 1 Nil)
                   (Cons (Cons 2 (Cons 3 Nil))
                         (Cons Nil
                               (Cons (Cons 4 Nil) Nil))))))

  ;; ----- mconcat over Sum ----------------------------------
  (: total Sum)
  (define total
    (mconcat (Cons (Sum 3) (Cons (Sum 5) (Cons (Sum 7) Nil)))))

  (: empty-total Sum)
  (define empty-total (mconcat (ann Nil (List Sum))))

  ;; ----- mconcat over Product ------------------------------
  (: factorial-5 Product)
  (define factorial-5
    (mconcat (Cons (Product 1)
                   (Cons (Product 2)
                         (Cons (Product 3)
                               (Cons (Product 4)
                                     (Cons (Product 5) Nil))))))))

;; ---------- assertions ------------------------------------------

(test-case "mconcat over String"
  (check-equal? glued "abc"))

(test-case "mconcat over empty String list yields mempty"
  (check-equal? empty-strs ""))

(test-case "mconcat over (List Integer) flattens"
  (check-equal? flat
                (Cons 1 (Cons 2 (Cons 3 (Cons 4 Nil))))))

(test-case "mconcat over Sum"
  (check-equal? total (Sum 15)))

(test-case "mconcat over empty Sum list yields (Sum 0)"
  (check-equal? empty-total (Sum 0)))

(test-case "mconcat over Product computes 5!"
  (check-equal? factorial-5 (Product 120)))

;; ----- ambiguity rejected at compile time -----------------

(define-syntax-rule (check-rackton-compile-error form ...)
  (check-exn
   exn:fail?
   (lambda ()
     (eval #'(rackton form ...)
           (variable-reference->namespace (#%variable-reference))))))

(test-case "mconcat of an unascribed Nil is rejected"
  (check-rackton-compile-error
   (define x (mconcat Nil))))

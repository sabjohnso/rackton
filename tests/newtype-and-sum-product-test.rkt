#lang racket/base

;; Newtypes + Sum/Product monoids.

(require rackunit
         (for-syntax racket/base)
         "../main.rkt")

(rackton
  (require rackton/data/monoid)
  ;; ----- Sum: additive monoid ----------------------------------
  (: s-add Sum)
  (define s-add (<> (Sum 3) (Sum 5)))

  (: s-id-l Sum)
  (define s-id-l (<> (ann mempty Sum) (Sum 7)))

  (: s-id-r Sum)
  (define s-id-r (<> (Sum 7) (ann mempty Sum)))

  ;; ----- Product: multiplicative monoid ------------------------
  (: p-mul Product)
  (define p-mul (<> (Product 3) (Product 5)))

  (: p-id-l Product)
  (define p-id-l (<> (ann mempty Product) (Product 7)))

  (: p-id-r Product)
  (define p-id-r (<> (Product 7) (ann mempty Product)))

  ;; ----- accessors ---------------------------------------------
  (: sum-out Integer)
  (define sum-out (get-sum (Sum 42)))

  (: product-out Integer)
  (define product-out (get-product (Product 7)))

  ;; ----- folding integers into a Sum ---------------------------
  ;; (foldr <> mempty xs) — `mempty` is pinned to Sum by ascription.
  (: total Sum)
  (define total
    (foldr (lambda (n acc) (<> (Sum n) acc))
           (ann mempty Sum)
           (Cons 1 (Cons 2 (Cons 3 (Cons 4 Nil))))))

  ;; ----- folding integers into a Product -----------------------
  (: factorial-4 Product)
  (define factorial-4
    (foldr (lambda (n acc) (<> (Product n) acc))
           (ann mempty Product)
           (Cons 1 (Cons 2 (Cons 3 (Cons 4 Nil)))))))

;; ---------- assertions ------------------------------------------

(test-case "Semigroup Sum"
  (check-equal? s-add (Sum 8)))

(test-case "Sum identity laws"
  (check-equal? s-id-l (Sum 7))
  (check-equal? s-id-r (Sum 7)))

(test-case "Semigroup Product"
  (check-equal? p-mul (Product 15)))

(test-case "Product identity laws"
  (check-equal? p-id-l (Product 7))
  (check-equal? p-id-r (Product 7)))

(test-case "Accessors unwrap"
  (check-equal? sum-out 42)
  (check-equal? product-out 7))

(test-case "Sum monoid over a list of integers"
  (check-equal? total (Sum 10)))

(test-case "Product monoid over a list of integers"
  (check-equal? factorial-4 (Product 24)))

;; ----- newtype validation rejected at compile time --------

(define-syntax-rule (check-rackton-compile-error form ...)
  (check-exn
   exn:fail?
   (lambda ()
     (eval #'(rackton form ...)
           (variable-reference->namespace (#%variable-reference))))))

(test-case "newtype rejects multi-field ctor"
  (check-rackton-compile-error
   (newtype Bad (BadCtor Integer Integer))))

(test-case "newtype rejects multiple ctors"
  (check-rackton-compile-error
   (newtype Bad (A Integer) (B Integer))))

#lang racket/base

;; Constraint families (Feature 4, higher-order): a `constraint-family`
;; computes a CONSTRAINT from type arguments.  Its clause right-hand
;; sides are lists of constraints that may recurse and may apply a
;; parameter as a constraint head — so `(All Show xs)` expands to a
;; `Show` obligation on every element of the promoted list `xs`.
;;
;; Observed through a phantom `Proxy` indexed by a type-level list: a
;; function constrained by `(All Show xs)` is callable at a proxy for a
;; concrete list iff every element has a `Show` instance.

(require rackunit
         (for-syntax racket/base)
         "../main.rkt")

(define-syntax-rule (compile-error-message form ...)
  (with-handlers ([exn:fail? exn-message])
    (eval #'(rackton form ...)
          (variable-reference->namespace (#%variable-reference)))
    (fail "expected a compile error but the program compiled")))

(rackton
  (data (TList a) TNil (TCons a (TList a)))
  (data (Proxy a) MkProxy)

  ;; All c xs : the constraint "c holds of every element of xs".
  (constraint-family (All c xs)
    [c TNil         = ]
    [c (TCons x xs) = (c x) (All c xs)])

  (: witness ((All Show xs) => (-> (Proxy xs) Integer)))
  (define (witness p) 0)

  ;; Integer and String both have Show, so (All Show [Integer, String])
  ;; reduces to satisfiable obligations.
  (: pr (Proxy (TCons Integer (TCons String TNil))))
  (define pr MkProxy)
  (: r Integer)
  (define r (witness pr)))

(test-case "All Show xs reduces to a Show obligation per element"
  (check-equal? r 0))

(test-case "a list element without the instance is rejected"
  (define msg (compile-error-message
               (data (TList2 a) TNil2 (TCons2 a (TList2 a)))
               (data (Proxy2 a) MkProxy2)
               (constraint-family (All2 c xs)
                 [c TNil2         = ]
                 [c (TCons2 x xs) = (c x) (All2 c xs)])
               (data NoShow MkNoShow)
               (: witness2 ((All2 Show xs) => (-> (Proxy2 xs) Integer)))
               (define (witness2 p) 0)
               (: pr2 (Proxy2 (TCons2 NoShow TNil2)))
               (define pr2 MkProxy2)
               (: r2 Integer)
               (define r2 (witness2 pr2))))
  (check-regexp-match #rx"Show|instance" msg))

(test-case "a non-terminating constraint family hits its budget"
  (define msg (compile-error-message
               (data (TL a) Nl (Cn a (TL a)))
               (data (Px a) MkPx)
               (constraint-family (Loopy c xs)
                 [c xs = (Loopy c xs)])         ; never reaches a base case
               (: w ((Loopy Show xs) => (-> (Px xs) Integer)))
               (define (w p) 0)
               (: px (Px (Cn Integer Nl)))
               (define px MkPx)
               (: rr Integer)
               (define rr (w px))))
  (check-regexp-match #rx"budget" msg))

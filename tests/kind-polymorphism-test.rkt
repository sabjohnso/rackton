#lang racket/base

;; Kind polymorphism (Layer A): a data type's residual parameter kinds are
;; GENERALISED rather than defaulted to `*`.  A phantom/unused parameter
;; therefore becomes `forall k. …` and the constructor kind-checks at any
;; kind in different places — the foundation for promoted parameterised
;; datatypes (Layer B).

(require rackunit
         (for-syntax racket/base)
         "../main.rkt")

(define-syntax-rule (kind-error-message form ...)
  (with-handlers ([exn:fail? exn-message])
    (eval #'(rackton form ...)
          (variable-reference->namespace (#%variable-reference)))
    (fail "expected a kind error but the program compiled")))

;; ----- a phantom parameter is kind-polymorphic ----------------------

;; `Proxy`'s parameter `a` is never used in a constructor field, so its
;; kind is unconstrained.  Under generalisation `Proxy :: forall k. k -> *`,
;; so `Proxy` may be applied both to a `*`-kinded type (Integer) and to a
;; `(* -> *)`-kinded constructor (Maybe2) in the same module.
(rackton
  (data (Proxy a) MkProxy)
  (data (Maybe2 a) None2 (Some2 a))        ; Maybe2 :: * -> *

  (: p-int (Proxy Integer))                ; Proxy at kind *
  (define p-int MkProxy)

  (: p-maybe (Proxy Maybe2))               ; Proxy at kind (* -> *)
  (define p-maybe MkProxy)

  (: both (Pair (Proxy Integer) (Proxy Maybe2)))
  (define both (Pair MkProxy MkProxy)))

(test-case "a phantom parameter generalises and applies at distinct kinds"
  ;; The block above compiled — Proxy kind-checked at both `*` and
  ;; `(* -> *)`.  The values themselves are opaque MkProxy/Pair instances.
  (check-false (eq? p-int #f))
  (check-false (eq? p-maybe #f))
  (check-false (eq? both #f)))

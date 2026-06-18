#lang racket/base

;; Polymorphic data kinds (Layer B): a PARAMETERISED datatype is promoted
;; to the kind level, so `(List κ)` is a reusable KIND for any kind κ.  The
;; promoted constructors are kind-polymorphic — `Nil :: forall k. List k`,
;; `Cons :: forall k. k -> List k -> List k` — so one promoted `List`
;; indexes types at element kind `Ty`, at element kind `Nat`, and serves as
;; the stack-machine's stack shape (replacing the bespoke monomorphic
;; `Stack`/`SPush` of datakinds-test.rkt).

(require rackunit
         (for-syntax racket/base)
         "../main.rkt")

(define-syntax-rule (kind-error-message form ...)
  (with-handlers ([exn:fail? exn-message])
    (eval #'(rackton form ...)
          (variable-reference->namespace (#%variable-reference)))
    (fail "expected a kind error but the program compiled")))

;; ----- one promoted List, used at two element kinds -----------------

(rackton
  (data Ty TInt TBool)                       ; monomorphic → kind Ty
  (data (List a) Nil (Cons a (List a)))       ; parameterised → kind (List k)

  (data (Phantom a) MkPhantom)               ; forall k. k -> *

  ;; (Cons TInt Nil) :: (List Ty); (Cons 5 Nil) :: (List Nat).  The SAME
  ;; promoted List kind-checks at element kind Ty and at element kind Nat.
  (: at-ty (Phantom (Cons TInt Nil)))
  (define at-ty MkPhantom)

  (: at-nat (Phantom (Cons 5 Nil)))
  (define at-nat MkPhantom))

(test-case "one promoted List is reusable at element kinds Ty and Nat"
  (check-false (eq? at-ty #f))
  (check-false (eq? at-nat #f)))

;; ----- stack machine over a promoted (List Ty) ----------------------

(rackton
  (data (Expr a)
    (Lit : (-> Integer (Expr Integer)))
    (Add : (-> (Expr Integer) (Expr Integer) (Expr Integer))))

  (data Ty2 TInt2 TBool2)
  (data (Lst a) LNil (LCons a (Lst a)))

  ;; Code's stack shape is a promoted (Lst Ty2) — its kind
  ;; (Lst Ty2) -> (Lst Ty2) -> * is INFERRED from the promoted LCons/TInt2.
  (data (Code s t)
    (HALT  : (Code s s))
    (PUSHI : (-> Integer (Code (LCons TInt2 s) t) (Code s t)))
    (IADD  : (-> (Code (LCons TInt2 s) t)
                 (Code (LCons TInt2 (LCons TInt2 s)) t))))

  (: compile (-> (Expr a) (-> (Code (LCons TInt2 s) t) (Code s t))))
  (define (compile e k)
    (match e
      [(Lit n)   (PUSHI n k)]
      [(Add x y) (compile x (compile y (IADD k)))]))

  (: program (Code LNil (LCons TInt2 LNil)))
  (define program (compile (Add (Lit 2) (Lit 3)) HALT))

  (: size (-> (Code s t) Integer))
  (define (size c)
    (match c
      [(HALT)      0]
      [(PUSHI n k) (+ 1 (size k))]
      [(IADD k)    (+ 1 (size k))]))

  (: program-size Integer)
  (define program-size (size program)))

(test-case "stack machine over a promoted (Lst Ty2) typechecks and runs"
  (check-equal? program-size 3))

;; ----- kind safety: a non-list tail is rejected ---------------------

(test-case "a promoted Cons's tail must itself be a list"
  ;; ConsX's second argument must have kind (ListX k); a TyX tag is wrong.
  (define msg (kind-error-message
               (data TyX TIntX TBoolX)
               (data (ListX a) NilX (ConsX a (ListX a)))
               (data (CodeX s t)
                 (PUSHIX : (-> Integer (CodeX (ConsX TIntX TIntX) t)
                               (CodeX s t))))
               (: xx Integer)
               (define xx 0)))
  (check-regexp-match #rx"kind error" msg))

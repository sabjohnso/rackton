#lang racket/base

;; Type-level recursion (Feature 2): a closed family whose right-hand
;; side mentions the family recurses over promoted data.  Reduction is
;; the fixpoint walk of `normalize-type`; a fuel budget turns a
;; non-terminating family into a compile-time error rather than a hang.
;;
;; Reductions are observed through a phantom-indexed type plus a `same`
;; combiner that forces two phantom types to unify: the program compiles
;; iff the family reduced to the expected normal form.

(require rackunit
         (for-syntax racket/base)
         "../main.rkt")

(define-syntax-rule (compile-error-message form ...)
  (with-handlers ([exn:fail? exn-message])
    (eval #'(rackton form ...)
          (variable-reference->namespace (#%variable-reference)))
    (fail "expected a compile error but the program compiled")))

;; ----- recursion over a promoted List: Append -----------------------

(rackton
  (data Ty TInt TBool)
  (data (List a) Nil (Cons a (List a)))

  (type-family (Append xs ys)
    [Nil         ys = ys]
    [(Cons x zs) ys = (Cons x (Append zs ys))])

  (data (Phantom a) MkP)
  (: same (-> (Phantom a) (Phantom a) Boolean))
  (define (same x y) #t)

  ;; Append [TInt] [TBool] should reduce to [TInt, TBool].
  (: appended (Phantom (Append (Cons TInt Nil) (Cons TBool Nil))))
  (define appended MkP)
  (: expected (Phantom (Cons TInt (Cons TBool Nil))))
  (define expected MkP)

  ;; Compiles only if the two phantom types are equal — i.e. Append reduced.
  (: ok Boolean)
  (define ok (same appended expected)))

(test-case "a recursive family reduces over a promoted List (Append)"
  (check-true ok))

;; ----- recursion over promoted Peano naturals: Plus -----------------

(rackton
  (data Peano PZ (PS Peano))

  (type-family (Plus a b)
    [PZ     b = b]
    [(PS n) b = (PS (Plus n b))])

  (data (Box a) MkBox)
  (: agree (-> (Box a) (Box a) Boolean))
  (define (agree x y) #t)

  ;; 2 + 1 = 3
  (: sum  (Box (Plus (PS (PS PZ)) (PS PZ))))
  (define sum MkBox)
  (: three (Box (PS (PS (PS PZ)))))
  (define three MkBox)

  (: okP Boolean)
  (define okP (agree sum three)))

(test-case "a recursive family reduces over promoted Peano naturals (Plus)"
  (check-true okP))

;; ----- structure-changing recursion: Length (List ⇒ Peano) ----------

(rackton
  (data T2 A2 B2)
  (data (L2 a) N2 (C2 a (L2 a)))
  (data Nat2 Z2 (Sc2 Nat2))

  (type-family (Length xs)
    [N2        = Z2]
    [(C2 x zs) = (Sc2 (Length zs))])

  (data (Bx a) MkBx)
  (: eqv (-> (Bx a) (Bx a) Boolean))
  (define (eqv x y) #t)

  ;; length [A2, B2] = 2
  (: len (Bx (Length (C2 A2 (C2 B2 N2)))))
  (define len MkBx)
  (: two (Bx (Sc2 (Sc2 Z2))))
  (define two MkBx)

  (: okL Boolean)
  (define okL (eqv len two)))

(test-case "recursion can change the promoted structure (Length: List ⇒ Peano)"
  (check-true okL))

;; ----- fuel: a non-terminating family is a compile error ------------

(test-case "a non-terminating family exhausts its fuel budget"
  (define msg (compile-error-message
               (data (Ph a) MkPh)
               (type-family (Loop a)
                 [a = (Loop a)])           ; reduces to itself forever
               (: boom (Ph (Loop Integer)))
               (define boom MkPh)))
  (check-regexp-match #rx"budget|reduction" msg))

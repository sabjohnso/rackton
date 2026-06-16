#lang racket/base

;; DataKinds-style promotion: a monomorphic datatype is promoted to the
;; kind level, so its constructors can index other types and those
;; indices are KIND-CHECKED.  This gives the stack-machine compiler a
;; genuine guarantee (stack shapes are well-formed lists of tags), which
;; the phantom-`*` encoding could not enforce.

(require rackunit
         (for-syntax racket/base)
         "../main.rkt")

(define-syntax-rule (kind-error-message form ...)
  (with-handlers ([exn:fail? exn-message])
    (eval #'(rackton form ...)
          (variable-reference->namespace (#%variable-reference)))
    (fail "expected a kind error but the program compiled")))

;; ----- the kind-checked stack-machine compiler ----------------------

(rackton
  (data (Expr a)
    (Lit : (-> Integer (Expr Integer)))
    (Add : (-> (Expr Integer) (Expr Integer) (Expr Integer))))

  ;; Ordinary monomorphic datatypes — promoted to kinds Ty and Stack.
  (data Ty TInt TBool)
  (data Stack SEmpty (SPush Ty Stack))

  ;; Code is indexed by promoted Stack shapes.  Its kind
  ;; (Stack -> Stack -> *) is INFERRED from the constructors' use of the
  ;; promoted SPush/TInt — no kind annotation written.
  (data (Code s t)
    (HALT  : (Code s s))
    (PUSHI : (-> Integer (Code (SPush TInt s) t) (Code s t)))
    (IADD  : (-> (Code (SPush TInt s) t)
                 (Code (SPush TInt (SPush TInt s)) t))))

  (: compile (-> (Expr a) (-> (Code (SPush TInt s) t) (Code s t))))
  (define (compile e k)
    (match e
      [(Lit n)   (PUSHI n k)]
      [(Add x y) (compile x (compile y (IADD k)))]))

  ;; Start empty, end with one Integer tag pushed: SEmpty -> (SPush TInt SEmpty).
  (: program (Code SEmpty (SPush TInt SEmpty)))
  (define program (compile (Add (Lit 2) (Lit 3)) HALT))

  (: size (-> (Code s t) Integer))
  (define (size c)
    (match c
      [(HALT)      0]
      [(PUSHI n k) (+ 1 (size k))]
      [(IADD k)    (+ 1 (size k))]))

  (: program-size Integer)
  (define program-size (size program)))

(test-case "kind-checked stack-machine compiler typechecks and runs"
  (check-equal? program-size 3))

;; ----- kind safety: ill-kinded indices are rejected -----------------

;; Each negative block is eval'd into this module's namespace, so its
;; constructor names must be unique across the file (hence the A/B
;; suffixes — same shapes as above, fresh names).

(test-case "a promoted constructor rejects an argument of the wrong kind"
  ;; SPushA expects a TyA tag, but Integer has kind * — caught only by the
  ;; kind checker (the phantom-* encoding would have accepted it).
  (define msg (kind-error-message
               (data TyA TIntA TBoolA)
               (data StackA SEmptyA (SPushA TyA StackA))
               (data (CodeA s t)
                 (PUSHIA : (-> Integer (CodeA (SPushA Integer s) t)
                               (CodeA s t))))
               (: xa Integer)
               (define xa 0)))
  (check-regexp-match #rx"kind error" msg))

(test-case "a promoted constructor's tail must itself be a stack"
  ;; SPushB's second argument must have kind StackB; a TyB tag is wrong.
  (define msg (kind-error-message
               (data TyB TIntB TBoolB)
               (data StackB SEmptyB (SPushB TyB StackB))
               (data (CodeB s t)
                 (PUSHIB : (-> Integer (CodeB (SPushB TIntB TIntB) t)
                               (CodeB s t))))
               (: xb Integer)
               (define xb 0)))
  (check-regexp-match #rx"kind error" msg))

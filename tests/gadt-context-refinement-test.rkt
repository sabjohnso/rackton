#lang racket/base

;; GADT pattern-match refinement must reach the WHOLE arm context — the
;; types of in-scope bindings whose types mention the refined index, not
;; only the arm's expected result type.  This is what lets Gibbons'
;; type-safe stack-machine compiler thread a stack-shape index through a
;; continuation argument.

(require rackunit
         "../main.rkt")

(rackton
  (data (Expr a)
    (Lit  : (-> Integer (Expr Integer)))
    (BVal : (-> Boolean (Expr Boolean)))
    (Add  : (-> (Expr Integer) (Expr Integer) (Expr Integer))))

  ;; ---- 1. Refinement reaches a second in-scope binding -------------
  ;; Matching `Lit` refines a~Integer; the unrelated parameter `b : (Box a)`
  ;; must be refined to (Box Integer) so unwrapping it yields an Integer.
  (data (Box a) (MkBox a))

  (: use (-> (Expr a) (-> (Box a) Integer)))
  (define (use e b)
    (match e
      [(Lit n)  (match b [(MkBox x) (+ x n)])]
      [(Add x y) (match b [(MkBox z) z])]))

  (: used Integer)
  (define used (use (Lit 5) (MkBox 7)))

  ;; ---- 2. The type-safe stack-machine compiler ---------------------
  ;; Phantom type-level list as the stack shape.
  (data SNil        MkSNil)
  (data (SCons h t) MkSCons)

  ;; Machine code indexed by input -> output stack shape.
  (data (Code s t)
    (HALT  : (Code s s))
    (PUSHI : (-> Integer (Code (SCons Integer s) t) (Code s t)))
    (PUSHB : (-> Boolean (Code (SCons Boolean s) t) (Code s t)))
    (IADD  : (-> (Code (SCons Integer s) t)
                 (Code (SCons Integer (SCons Integer s)) t))))

  ;; compile : Expr a -> Code (SCons a s) t -> Code s t
  ;; The continuation `k` carries the post-push stack shape; matching `e`
  ;; must refine `a` inside `k`'s type for each arm to typecheck.
  (: compile (-> (Expr a) (-> (Code (SCons a s) t) (Code s t))))
  (define (compile e k)
    (match e
      [(Lit n)   (PUSHI n k)]
      [(BVal b)  (PUSHB b k)]
      [(Add x y) (compile x (compile y (IADD k)))]))

  ;; Compiling one expression nets a single Integer pushed onto the
  ;; stack: from the empty stack SNil the code ends at (SCons Integer SNil).
  ;; (Annotating SNil->SNil is correctly rejected by the type checker.)
  (: program (Code SNil (SCons Integer SNil)))
  (define program (compile (Add (Lit 2) (Lit 3)) HALT))

  ;; Count instructions — exercises matching the Code GADT and recursion,
  ;; and runs the generated code.
  (: size (-> (Code s t) Integer))
  (define (size c)
    (match c
      [(HALT)      0]
      [(PUSHI n k) (+ 1 (size k))]
      [(PUSHB b k) (+ 1 (size k))]
      [(IADD k)    (+ 1 (size k))]))

  (: program-size Integer)
  (define program-size (size program)))

(test-case "GADT refinement reaches a second in-scope binding"
  (check-equal? used 12))

(test-case "type-safe stack-machine compiler typechecks and runs"
  ;; PUSHI 2 (PUSHI 3 (IADD HALT)) — three instructions.
  (check-equal? program-size 3))

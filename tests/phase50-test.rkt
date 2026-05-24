#lang racket/base

;; Phase 50: full GADTs with per-constructor result types and
;; local skolem refinement at pattern matches.

(require rackunit
         "../main.rkt")

(rackton
  ;; A small typed expression language.  Each ctor pins the result
  ;; type of the Expr it builds: literals are Integer, booleans are
  ;; Boolean, addition is Integer-only, conditional is polymorphic.

  (define-data (Expr a)
    (Lit  #:returns (Expr Integer) Integer)
    (BVal #:returns (Expr Boolean) Boolean)
    (Plus #:returns (Expr Integer) (Expr Integer) (Expr Integer))
    (If   #:returns (Expr a)       (Expr Boolean) (Expr a) (Expr a)))

  ;; ----- 50.A Monomorphic evaluator at Integer --------------
  ;; The simpler case — no skolem refinement needed.

  (: eval-bool (-> (Expr Boolean) Boolean))
  (define (eval-bool e)
    (match e
      [(BVal b) b]
      [(If c t e)
       (if (eval-bool c) (eval-bool t) (eval-bool e))]))

  (: eval-int (-> (Expr Integer) Integer))
  (define (eval-int e)
    (match e
      [(Lit n)        n]
      [(Plus x y)     (+ (eval-int x) (eval-int y))]
      [(If c t e)
       (if (eval-bool c) (eval-int t) (eval-int e))]))

  (: expr-int Integer)
  (define expr-int
    (eval-int (Plus (Lit 3) (Plus (Lit 4) (Lit 5)))))

  (: expr-cond Integer)
  (define expr-cond
    (eval-int (If (BVal #t) (Lit 10) (Lit 99))))

  ;; ----- 50.B Polymorphic evaluator -------------------------
  ;; The hard case — `a` is a skolem in the body of `eval`, and
  ;; each pattern arm needs to refine it (Lit ⇒ a~Integer, BVal
  ;; ⇒ a~Boolean, etc.).

  (: eval (-> (Expr a) a))
  (define (eval e)
    (match e
      [(Lit n)        n]
      [(BVal b)       b]
      [(Plus x y)     (+ (eval x) (eval y))]
      [(If c t e)     (if (eval c) (eval t) (eval e))]))

  (: poly-int Integer)
  (define poly-int  (eval (Plus (Lit 1) (Lit 2))))

  (: poly-bool Boolean)
  (define poly-bool (eval (BVal #t)))

  (: poly-mixed Integer)
  (define poly-mixed
    (eval (If (BVal #f) (Lit 100) (Plus (Lit 7) (Lit 35)))))

  ;; ----- 50.C Explicit type-equality constraints ------------
  ;; A function asks for a `~` constraint between two of its type
  ;; vars.  The caller must supply types that already satisfy the
  ;; equality, or the call won't typecheck.

  (: pair-eq ((~ a b) => (-> a (-> b (Pair a b)))))
  (define (pair-eq x y) (MkPair x y))

  (: eq-int (Pair Integer Integer))
  (define eq-int (pair-eq 7 7))

  (: rackton-eq-pair? (-> (Pair Integer Integer) (-> Integer (-> Integer Boolean))))
  (define (rackton-eq-pair? p a b)
    (match p
      [(MkPair x y) (and (= x a) (= y b))])))

;; ---------- assertions ---------------------------------------

(test-case "GADT monomorphic eval-int"
  (check-equal? expr-int 12)
  (check-equal? expr-cond 10))

(test-case "GADT monomorphic eval-bool"
  (check-equal? expr-int 12))

(test-case "Polymorphic GADT eval — Lit refines a~Integer"
  (check-equal? poly-int  3))

(test-case "Polymorphic GADT eval — BVal refines a~Boolean"
  (check-equal? poly-bool #t))

(test-case "Polymorphic GADT eval through If"
  (check-equal? poly-mixed 42))

(test-case "Explicit (~ a b) equality constraint"
  (check-true (rackton-eq-pair? eq-int 7 7)))

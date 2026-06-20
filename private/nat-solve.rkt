#lang racket/base

;; Rackton — type-level natural-number arithmetic and the linear solver.
;;
;; Nat expressions are built from literals (`tnat`), nat-kinded type
;; variables, and the binary operators `+` and `*` (reserved type
;; constructors `(tapp (tcon '+) …)` / `(tapp (tcon '*) …)`).  This
;; module reduces such an expression to a LINEAR NORMAL FORM
;;
;;     const + Σ cᵢ·atomᵢ
;;
;; where each `atomᵢ` is a distinct nat-kinded variable or a canonical
;; opaque product (the only nonlinear shape we keep), and each `cᵢ` is a
;; nonzero integer coefficient.  `+` adds two forms; `*` either scales a
;; form by a constant (staying linear) or, when both operands are
;; non-constant, becomes a single opaque product atom (so `n*m ≡ m*n`).
;;
;; `solve-nat-equation σ τ` turns the equation σ = τ into `D = 0` and:
;;   - D empty  → the equation holds (empty substitution);
;;   - D empty with a nonzero constant → unsatisfiable (#f);
;;   - D is `c·v + k` for a single VARIABLE atom v → solve v = -k/c when
;;     that is a non-negative integer, else #f;
;;   - D has ≥ 2 atoms → eliminate a UNIT-coefficient variable, binding it
;;     to an expression in the rest, when that expression is a valid Nat
;;     (non-negative) and does not reintroduce the variable.  This is the
;;     arithmetic analogue of peeling `(S x) ~ (S y)`, so the built-in Nat
;;     can index GADTs (a depth-indexed Vec/stack) the way unary `S` does.
;;   - otherwise → stuck (#f).
;;
;; A rigid SKOLEM (a `tcon` standing for a universally-quantified Nat from
;; a declared signature or a GADT match) is an opaque atom — it carries
;; through but cannot be solved for.
;;
;; Out of scope here (see PLAN.org): user-facing `-` (truncated nat
;; subtraction is non-linear — the non-negativity gate keeps it out),
;; deferred residual constraints, and multi-variable solving with NO unit
;; coefficient (e.g. `2n ~ m+1`).  The solver subtracts over ℤ internally;
;; only the SOLVED value is required to be a valid Nat.

(provide solve-nat-equation
         nat-expr?
         normalize-nat-type
         deep-normalize-nats)

(require racket/match
         racket/list
         racket/set
         "types.rkt")

;; ----- linear form ---------------------------------------------------
;; `const` is an integer; `atoms` is an immutable (equal-keyed) hash from
;; an atom TYPE (a tvar or a canonical product tapp) to a nonzero integer.

(struct linform (const atoms) #:transparent)

(define (lf-const c) (linform c (hash)))

(define (lf+ a b)
  (define atoms
    (for/fold ([h (linform-atoms a)]) ([(k v) (in-hash (linform-atoms b))])
      (define nv (+ (hash-ref h k 0) v))
      (if (zero? nv) (hash-remove h k) (hash-set h k nv))))
  (linform (+ (linform-const a) (linform-const b)) atoms))

(define (lf-scale k a)
  (cond
    [(zero? k) (lf-const 0)]
    [else
     (linform (* k (linform-const a))
              (for/hash ([(key v) (in-hash (linform-atoms a))])
                (values key (* k v))))]))

(define (lf- a b) (lf+ a (lf-scale -1 b)))

;; ----- a stable ordering on atom types (for canonicalization) --------

(define (type-key t) (format "~s" (type->datum t)))

(define (type<=? a b) (string<=? (type-key a) (type-key b)))

;; The canonical product of two operand types: operands sorted, so
;; `n*m` and `m*n` produce the same atom.
(define (canon-product a b)
  (define-values (x y) (if (type<=? a b) (values a b) (values b a)))
  (make-tapp (tcon '*) (list x y)))

;; ----- nat expression → linear form (or #f if not a Nat type) --------

(define (nat->linform t)
  (match t
    [(tnat c) (lf-const c)]
    [(tvar _) (linform 0 (hash t 1))]
    ;; A skolem (a rigid `tcon` standing for a universally-quantified Nat
    ;; from a declared signature or a GADT match) is an opaque atom — it
    ;; cannot be solved FOR, but it must carry through, so that e.g.
    ;; `(+ n* 1) ~ (+ n* 1)` reduces to 0 and `(+ m 1) ~ (+ n* 1)` solves
    ;; `m := n*`.  Only skolems are atomized; an ordinary tcon (Integer,
    ;; …) in a Nat position stays #f, so a genuine kind clash still fails.
    [(tcon n) #:when (skolem-tcon-name? n) (linform 0 (hash t 1))]
    [(tapp (tcon '+) (list a b))
     (define la (nat->linform a))
     (define lb (nat->linform b))
     (and la lb (lf+ la lb))]
    [(tapp (tcon '*) (list a b))
     (define la (nat->linform a))
     (define lb (nat->linform b))
     (and la lb (nat-mul la lb))]
    [_ #f]))

;; Multiply two forms.  Constant × form scales (stays linear); otherwise
;; the product is a single opaque atom keyed by its canonical form.
(define (nat-mul la lb)
  (cond
    [(hash-empty? (linform-atoms la)) (lf-scale (linform-const la) lb)]
    [(hash-empty? (linform-atoms lb)) (lf-scale (linform-const lb) la)]
    [else
     (linform 0 (hash (canon-product (linform->type la) (linform->type lb)) 1))]))

;; ----- linear form → canonical type ----------------------------------
;; Only called on forms with non-negative coefficients/const (user nat
;; expressions); a ground form floors at 0.

(define (linform->type lf)
  (define c (linform-const lf))
  (define atoms (linform-atoms lf))
  (cond
    [(hash-empty? atoms) (tnat (max 0 c))]
    [else
     (define sorted
       (sort (hash->list atoms) string<=? #:key (lambda (p) (type-key (car p)))))
     (define terms
       (for/list ([p (in-list sorted)])
         (define atom (car p))
         (define k (cdr p))
         (if (= k 1) atom (make-tapp (tcon '*) (list (tnat k) atom)))))
     (define all (if (zero? c) terms (append terms (list (tnat c)))))
     (sum-types all)]))

(define (sum-types ts)
  (foldr (lambda (x acc) (make-tapp (tcon '+) (list x acc)))
         (last ts) (drop-right ts 1)))

;; ----- public: predicate, normalizer, solver -------------------------

;; Does `t` denote a Nat expression at its head — a literal or a `+`/`*`
;; application?  (A bare tvar is not included: a plain variable unifies
;; structurally; only when it sits inside a `+`/`*` is it a nat atom.)
(define (nat-expr? t)
  (or (tnat? t)
      (and (tapp? t)
           (tcon? (tapp-head t))
           (memq (tcon-name (tapp-head t)) '(+ *)))))

;; Reduce a nat expression to its canonical linear normal form, as a
;; type.  Non-nat types are returned unchanged.
(define (normalize-nat-type t)
  (define lf (nat->linform t))
  (if lf (linform->type lf) t))

;; Reduce every maximal Nat subterm of a type to normal form (so e.g.
;; `(Array (* 2 2) a)` displays as `(Array 4 a)`).  Used for display.
(define (deep-normalize-nats t)
  (cond
    [(nat-expr? t) (normalize-nat-type t)]
    [else
     (match t
       [(tapp h args)
        (make-tapp (deep-normalize-nats h) (map deep-normalize-nats args))]
       [(qual cs body)
        (mqual (map deep-normalize-nats cs) (deep-normalize-nats body))]
       [(pred c args) (pred c (map deep-normalize-nats args))]
       [(tforall vs body) (tforall vs (deep-normalize-nats body))]
       [(texists vs body) (texists vs (deep-normalize-nats body))]
       [_ t])]))

;; Solve σ = τ.  Returns a substitution (possibly empty) on success, or
;; #f when the equation is unsatisfiable or stuck.
(define (solve-nat-equation σ τ)
  (define L (nat->linform σ))
  (define R (nat->linform τ))
  (cond
    [(or (not L) (not R)) #f]
    [else
     (define D (lf- L R))
     (define c (linform-const D))
     (define atoms (linform-atoms D))
     (cond
       [(hash-empty? atoms) (and (zero? c) empty-subst)]
       [(and (= (hash-count atoms) 1) (tvar? (car (hash-keys atoms))))
        ;; Single unknown, ANY coefficient: solve to a literal by exact
        ;; division.  coeff·v + c = 0 → v = -c / coeff, a non-negative
        ;; integer.  (Non-unit coefficients are fine here — e.g. 2·n = 6
        ;; gives n := 3 — because the remainder is just the constant.)
        (define v (tvar-name (car (hash-keys atoms))))
        (define coeff (car (hash-values atoms)))
        (define-values (q r) (quotient/remainder (- c) coeff))
        (and (zero? r) (>= q 0) (subst-singleton v (tnat q)))]
       ;; Two or more atoms: eliminate a UNIT-coefficient variable by
       ;; binding it to an expression in the rest — the arithmetic
       ;; analogue of peeling `(S x) ~ (S y)`.
       [else (solve-by-unit-elim D)])]))

;; True when every coefficient and the constant of a linform are ≥ 0, so
;; the form denotes a manifestly non-negative Nat for all Nat atoms.
(define (lf-nonneg? lf)
  (and (>= (linform-const lf) 0)
       (for/and ([(_ coeff) (in-hash (linform-atoms lf))]) (>= coeff 0))))

;; Search the difference `D` for a variable atom with unit coefficient
;; whose elimination yields a well-formed binding, and return the first
;; such substitution (or #f).  For a chosen `v` with coefficient ±1,
;; `Rest = D − cᵥ·v` and `v = (-1/cᵥ)·Rest = (- cᵥ)·Rest` (since cᵥ = ±1).
;; Two soundness gates, both load-bearing:
;;   - occurs: the candidate must not mention `v` — dropping `v`'s linear
;;     term does NOT remove `v` from a nonlinear product atom like
;;     `(* v m)`, and binding `v := (* v m)` is an infinite type;
;;   - non-negativity: the candidate must be a valid Nat (no truncated
;;     subtraction such as `v := a − 1`).
(define (solve-by-unit-elim D)
  (define atoms (linform-atoms D))
  (for/or ([(atom coeff) (in-hash atoms)])
    (and (tvar? atom)
         (or (= coeff 1) (= coeff -1))
         (let* ([v       (tvar-name atom)]
                [rest    (linform (linform-const D) (hash-remove atoms atom))]
                [cand-lf (lf-scale (- coeff) rest)])
           (and (lf-nonneg? cand-lf)
                (let ([cand (linform->type cand-lf)])
                  (and (not (set-member? (type-vars cand) v))
                       (subst-singleton v cand))))))))

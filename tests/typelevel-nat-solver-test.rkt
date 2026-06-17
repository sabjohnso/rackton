#lang racket/base

;; Phase 4 — type-level Nat arithmetic and the linear solver.
;;
;; `+` and `*` are type-level operators of kind `Nat -> Nat -> Nat`.
;; Unifying two Nat expressions reduces them to a linear normal form and:
;;   - ground terms reduce and compare           ((+ 3 4) ~ 7);
;;   - a single unknown is solved                 ((+ n 3) ~ 7  ⟹ n=4);
;;   - `*`-by-a-constant scales                    ((* 2 n) ~ 6  ⟹ n=3);
;;   - `*` of two unknowns is one opaque atom      ((* n m) ~ (* m n) ok);
;;   - nonlinear / multi-unknown / no-Nat-solution equations fail.
;; (User-facing `-` and deferred residual constraints are out of scope
;; for this phase — see PLAN.org.)

(require rackunit
         (for-syntax racket/base)
         (only-in "../private/types.rkt"
                  tnat tvar tcon make-tapp tapp empty-subst subst-singleton)
         (only-in "../private/unify.rkt" unify exn:fail:unify?)
         (only-in "../private/nat-solve.rkt" normalize-nat-type)
         "../main.rkt")

(define (N n) (tnat n))
(define (V s) (tvar s))
(define (n+ a b) (make-tapp (tcon '+) (list a b)))
(define (n* a b) (make-tapp (tcon '*) (list a b)))

(define-syntax-rule (check-rackton-compile-error form ...)
  (check-exn
   exn:fail?
   (lambda ()
     (eval #'(rackton form ...)
           (variable-reference->namespace (#%variable-reference))))))

;; ----- ground reduction --------------------------------------------

(test-case "ground nat arithmetic reduces and compares"
  (check-equal? (unify (n+ (N 3) (N 4)) (N 7)) empty-subst)
  (check-equal? (unify (n* (N 2) (N 3)) (N 6)) empty-subst)
  (check-exn exn:fail:unify? (lambda () (unify (n+ (N 3) (N 4)) (N 8)))))

(test-case "normalize-nat-type folds ground arithmetic"
  (check-equal? (normalize-nat-type (n+ (N 3) (N 4))) (N 7))
  (check-equal? (normalize-nat-type (n* (N 2) (N 3))) (N 6)))

;; ----- single-unknown linear solving -------------------------------

(test-case "a single unknown in a sum is solved"
  (check-equal? (unify (n+ (V 'n) (N 3)) (N 7)) (subst-singleton 'n (N 4)))
  (check-equal? (unify (N 7) (n+ (N 3) (V 'n))) (subst-singleton 'n (N 4))))

(test-case "multiplication by a constant scales and solves"
  (check-equal? (unify (n* (N 2) (V 'n)) (N 6)) (subst-singleton 'n (N 3))))

(test-case "no Nat solution fails (non-integer or negative)"
  (check-exn exn:fail:unify? (lambda () (unify (n* (N 2) (V 'n)) (N 7))))
  (check-exn exn:fail:unify? (lambda () (unify (n+ (V 'n) (N 3)) (N 2)))))

;; ----- `*` of unknowns is a canonical opaque atom ------------------

(test-case "products of unknowns are commutative and self-equal"
  (check-equal? (unify (n* (V 'n) (V 'm)) (n* (V 'm) (V 'n))) empty-subst)
  (check-equal? (unify (n* (V 'n) (V 'm)) (n* (V 'n) (V 'm))) empty-subst))

(test-case "a nonlinear / multi-unknown equation is stuck (fails)"
  ;; product atom vs a constant — cannot solve.
  (check-exn exn:fail:unify? (lambda () (unify (n* (V 'n) (V 'm)) (N 5))))
  ;; two distinct unknowns in a sum — only one unknown is solved.
  (check-exn exn:fail:unify? (lambda () (unify (n+ (V 'n) (V 'm)) (N 7)))))

;; ----- nat expressions unify inside a larger type ------------------

(test-case "nat arithmetic solves in an argument position"
  ;; (Vec (+ n 1) a) ~ (Vec 5 a)  ⟹  n = 4
  (define lhs (make-tapp (tcon 'Vec) (list (n+ (V 'n) (N 1)) (V 'a))))
  (define rhs (make-tapp (tcon 'Vec) (list (N 5) (tcon 'Integer))))
  (define s (unify lhs rhs))
  (check-equal? (subst-singleton 'n (N 4)) (hash-remove s 'a)))

;; ----- kind integration: operators kind-check as Nat ---------------

(test-case "nat operators parse and kind-check (Nat where * expected = error)"
  (check-rackton-compile-error
   (: x (Maybe (+ 1 2)))
   (define x None))
  (check-rackton-compile-error
   (: y (Maybe (* 2 3)))
   (define y None)))

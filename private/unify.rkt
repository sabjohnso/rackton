#lang racket/base

;; Rackton — most-general Hindley–Milner unifier.
;;
;; (unify σ τ) returns a substitution θ such that
;;   apply-subst θ σ  =  apply-subst θ τ
;; or raises (exn:fail:unify ...) describing why no unifier exists.
;;
;; Tenets:
;;   - Algebra-driven: the unifier's contract is stated as algebraic laws
;;     (unifies-what-it-returns, idempotent, symmetric) verified with
;;     rackcheck in private/unify-test.rkt.
;;   - Readability-first: textbook recursive formulation; no union-find.
;;     If profiling later demands speed, the slow reference stays as ground
;;     truth.

(provide unify
         (struct-out exn:fail:unify))

(require racket/match
         racket/set
         racket/list
         "types.rkt")

(struct exn:fail:unify exn:fail (reason left right) #:transparent)

(define (raise-unify! reason left right)
  (raise (exn:fail:unify
          (format "cannot unify ~s with ~s (~a)"
                  (type->datum left) (type->datum right) reason)
          (current-continuation-marks)
          reason left right)))

;; ----- bind-var: handle the (tvar α) ↦ τ case ------------------------

(define (bind-var α τ original-left original-right)
  (cond
    [(equal? (tvar α) τ) empty-subst]
    [(set-member? (type-vars τ) α)
     (raise-unify! 'occurs original-left original-right)]
    [else (subst-singleton α τ)]))

;; ----- unify: principal interface ------------------------------------

(define (unify σ τ)
  (match* (σ τ)
    [((tvar α) _)            (bind-var α τ σ τ)]
    [(_ (tvar α))            (bind-var α σ σ τ)]
    [((tcon c) (tcon c))     empty-subst]
    [((tapp h1 args1) (tapp h2 args2))
     (unify-tapp h1 args1 h2 args2 σ τ)]
    [(_ _) (raise-unify! 'mismatch σ τ)]))

;; Unify two flat type applications, allowing for arity mismatches by
;; peeling off arguments from the right.  This is what lets `(c a)`
;; unify with `(Result String Integer)` by binding c to the partial
;; application `(Result String)` and a to Integer — the kind of
;; partial-application that higher-kinded polymorphism demands.
(define (unify-tapp h1 args1 h2 args2 outer-l outer-r)
  (cond
    [(and (null? args1) (null? args2))
     (unify h1 h2)]
    [(null? args1)
     (unify h1 (make-tapp h2 args2))]
    [(null? args2)
     (unify (make-tapp h1 args1) h2)]
    [else
     (define θ-last (unify (last args1) (last args2)))
     (define rest1 (apply-subst θ-last (make-tapp h1 (drop-right args1 1))))
     (define rest2 (apply-subst θ-last (make-tapp h2 (drop-right args2 1))))
     (define θ-rest (unify rest1 rest2))
     (subst-compose θ-rest θ-last)]))

;; Unify two parallel lists of types, folding the running substitution.
;; original-left/right are passed through only so that error messages
;; reference the outer types the user originally tried to unify.
(define (unify-many xs ys original-left original-right)
  (let loop ([xs xs] [ys ys] [θ empty-subst])
    (cond
      [(null? xs) θ]
      [else
       (define θ′ (unify (apply-subst θ (car xs))
                         (apply-subst θ (car ys))))
       (loop (cdr xs) (cdr ys) (subst-compose θ′ θ))])))

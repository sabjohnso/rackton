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
     (cond
       [(not (= (length args1) (length args2)))
        (raise-unify! 'arity σ τ)]
       [else
        (define θ-head (unify h1 h2))
        (define θ-args
          (unify-many (map (lambda (t) (apply-subst θ-head t)) args1)
                      (map (lambda (t) (apply-subst θ-head t)) args2)
                      σ τ))
        (subst-compose θ-args θ-head)])]
    [(_ _) (raise-unify! 'mismatch σ τ)]))

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

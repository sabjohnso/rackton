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
         raise-unify!
         (struct-out exn:fail:unify))

(require racket/match
         racket/set
         racket/list
         "types.rkt")

(struct exn:fail:unify exn:fail (reason left right) #:transparent)

(define (raise-unify! reason left right)
  ;; The human-readable message gets the diagnostic treatment (shared
  ;; tvar renaming, flattened arrows); the structured `left`/`right`
  ;; fields keep the raw types for programmatic consumers.
  (define-values (l r)
    (let ([ss (format-types (list left right))])
      (values (car ss) (cadr ss))))
  (raise (exn:fail:unify
          (format "cannot unify ~a with ~a (~a)" l r reason)
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
    [((tforall vs1 b1) (tforall vs2 b2))
     ;; Alpha-equivalent unification of two polymorphic
     ;; types.  Same arity required; rename one side's bound vars
     ;; to fresh names, then unify the bodies.  The result subst
     ;; must not mention either side's bound vars — if it does,
     ;; the types weren't really equivalent (one side leaked a
     ;; bound var) and we reject.
     (cond
       [(not (= (length vs1) (length vs2)))
        (raise-unify! 'arity σ τ)]
       [else
        (define fresh
          (for/list ([v (in-list vs1)])
            (gensym (format "$alpha.~a." v))))
        (define s1 (alpha-rename vs1 fresh b1))
        (define s2 (alpha-rename vs2 fresh b2))
        (define θ (unify s1 s2))
        (define escapes? (escapes-fresh? θ fresh))
        (cond
          [escapes? (raise-unify! 'escape σ τ)]
          [else θ])])]
    [(_ _) (raise-unify! 'mismatch σ τ)]))

;; Build a fresh-renaming substitution from a list of bound vars to
;; a list of fresh names, then apply to the body — returns the
;; renamed body.  Used by alpha-equivalent unification of tforalls.
(define (alpha-rename old-vars new-names body)
  (define s
    (for/fold ([s empty-subst]) ([o (in-list old-vars)]
                                  [n (in-list new-names)])
      (subst-extend s o (tvar n))))
  (apply-subst s body))

;; A unifier escapes its tforall scope if either its domain or its
;; image references one of the fresh-rename names — that would mean
;; a free tvar on one side got pinned to a bound var on the other.
(define (escapes-fresh? θ fresh)
  (define fresh-set (list->seteq fresh))
  (for/or ([(k v) (in-hash θ)])
    (or (set-member? fresh-set k)
        (set-member? (type-vars v) k)
        (for/or ([f (in-list fresh)])
          (set-member? (type-vars v) f)))))

;; Unify two flat type applications, allowing for arity mismatches by
;; peeling off arguments from the right.  This is what lets `(c a)`
;; unify with `(Either String Integer)` by binding c to the partial
;; application `(Either String)` and a to Integer — the kind of
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

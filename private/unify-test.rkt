#lang racket/base

;; Tests for private/unify.rkt: most-general Hindley–Milner unifier.
;;
;; Algebraic laws checked:
;;   - Unifies what it returns:
;;       unify σ τ  =  Just θ  ⇒  apply-subst θ σ  =  apply-subst θ τ
;;   - Idempotence on success: θ θ τ = θ τ (composing θ with itself adds nothing).
;;   - Symmetry: (unify σ τ) succeeds ⇔ (unify τ σ) succeeds, and both produce
;;     unifying substitutions (may differ syntactically).
;;   - Occurs check fires when forming an infinite type.

(module+ test
  (require rackunit
           rackcheck
           racket/match
           "types.rkt"
           "unify.rkt")

  ;; ----- generators --------------------------------------------------

  (define gen:tcon-name
    (gen:choice (gen:const 'Integer) (gen:const 'Boolean)
                (gen:const 'String) (gen:const 'List) (gen:const 'Maybe)))

  (define gen:tvar-name
    (gen:choice (gen:const 'a) (gen:const 'b) (gen:const 'c)))

  (define (gen:type depth)
    (cond
      [(<= depth 0)
       (gen:choice (gen:let ([n gen:tvar-name]) (tvar n))
                   (gen:let ([n gen:tcon-name]) (tcon n)))]
      [else
       (gen:choice
        (gen:let ([n gen:tvar-name]) (tvar n))
        (gen:let ([n gen:tcon-name]) (tcon n))
        (gen:let ([n gen:tcon-name]
                  [args (gen:list (gen:type (sub1 depth)) #:max-length 2)])
          (make-tapp (tcon n) args)))]))

  ;; ----- examples ----------------------------------------------------

  ;; Identical primitive types unify trivially.
  (check-equal? (unify t-int t-int) empty-subst)

  ;; A free type variable unifies with anything by binding to it.
  (let ([θ (unify (tvar 'a) t-int)])
    (check-equal? (apply-subst θ (tvar 'a)) t-int)
    (check-equal? (apply-subst θ t-int)     t-int))

  ;; Symmetric variant.
  (let ([θ (unify t-int (tvar 'a))])
    (check-equal? (apply-subst θ (tvar 'a)) t-int)
    (check-equal? (apply-subst θ t-int)     t-int))

  ;; Different primitive type constructors clash.
  (check-exn exn:fail:unify? (lambda () (unify t-int t-bool)))

  ;; Occurs check prevents (a ↦ List a).
  (check-exn exn:fail:unify?
             (lambda () (unify (tvar 'a) (tapp t-list (list (tvar 'a))))))

  ;; Function-type unification recurses on dom and cod.
  (let ([θ (unify (make-arrow (tvar 'a) (tvar 'a))
                  (make-arrow t-int (tvar 'b)))])
    (check-equal? (apply-subst θ (tvar 'a)) t-int)
    (check-equal? (apply-subst θ (tvar 'b)) t-int))

  ;; Constructor arity mismatch fails (here disguised as a kind clash).
  (check-exn exn:fail:unify?
             (lambda ()
               (unify (tapp t-list (list (tvar 'a)))
                      (tapp t-list (list (tvar 'a) (tvar 'b))))))

  ;; ----- properties -------------------------------------------------

  ;; If unify succeeds, the returned substitution actually unifies the inputs.
  (check-property
   (property unifier-unifies-when-it-succeeds
             ([σ (gen:type 2)] [τ (gen:type 2)])
     (define θ (with-handlers ([exn:fail:unify? (lambda (_) #f)]) (unify σ τ)))
     (or (not θ)
         (equal? (apply-subst θ σ) (apply-subst θ τ)))))

  ;; A type always unifies with itself; the unifier acts as identity on it.
  (check-property
   (property self-unification-is-identity ([t (gen:type 2)])
     (define θ (unify t t))
     (equal? (apply-subst θ t) t)))

  ;; Unification is symmetric in success.
  (check-property
   (property unification-is-symmetric ([σ (gen:type 2)] [τ (gen:type 2)])
     (define θ1 (with-handlers ([exn:fail:unify? (lambda (_) #f)]) (unify σ τ)))
     (define θ2 (with-handlers ([exn:fail:unify? (lambda (_) #f)]) (unify τ σ)))
     ;; Both should succeed or both should fail.
     (and (eq? (and θ1 #t) (and θ2 #t))
          ;; When successful, both unifiers act as unifiers.
          (or (not θ1)
              (and (equal? (apply-subst θ1 σ) (apply-subst θ1 τ))
                   (equal? (apply-subst θ2 σ) (apply-subst θ2 τ)))))))

  ;; Idempotence: applying θ twice is the same as applying it once.
  (check-property
   (property unifier-is-idempotent ([σ (gen:type 2)] [τ (gen:type 2)])
     (define θ (with-handlers ([exn:fail:unify? (lambda (_) #f)]) (unify σ τ)))
     (or (not θ)
         (let ([t1 (apply-subst θ σ)])
           (equal? (apply-subst θ t1) t1))))))

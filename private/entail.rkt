#lang racket/base

;; Rackton — predicate entailment.
;;
;; Given a class env and a list of hypothesis predicates Γ, decide
;; whether a target predicate p is entailed:
;;
;;   - Γ ⊢ p when p is in Γ (after taking the superclass closure of Γ).
;;   - Γ ⊢ p when there is an instance whose head matches p
;;     under some substitution σ, and Γ ⊢ σ(context_i) for every
;;     instance context predicate.
;;
;; `reduce-context` is the dual operation used during inference: given
;; a collected set of constraints, strip away every predicate that the
;; instance environment fully discharges, keeping only those in head-
;; normal form (a tvar at the head of every argument).

(provide find-matching-instance
         instance-heads-equivalent?
         env-class-has-overlap?
         entail?
         reduce-context
         by-super
         match-pred)

(require racket/match
         racket/list
         "types.rkt"
         "env.rkt"
         "unify.rkt")

;; ----- one-way pattern matching ------------------------------------

;; Match the source predicate against the target by binding only the
;; source's type variables.  Returns a substitution on success, or #f.
(define (match-pred src dst)
  (cond
    [(eq? (pred-class src) (pred-class dst))
     (match-many (pred-args src) (pred-args dst))]
    [else #f]))

(define (match-many srcs dsts)
  (cond
    [(and (null? srcs) (null? dsts)) empty-subst]
    [(or (null? srcs) (null? dsts)) #f]
    [else
     (define σ1 (match-one (car srcs) (car dsts)))
     (cond
       [(not σ1) #f]
       [else
        (define σ2 (match-many (cdr srcs) (cdr dsts)))
        (and σ2 (merge-substs σ1 σ2))])]))

(define (match-one src dst)
  (match* (src dst)
    [((tvar α) t)          (subst-singleton α t)]
    [((tcon c) (tcon c2))  (if (eq? c c2) empty-subst #f)]
    [((tapp h1 args1) (tapp h2 args2))
     (cond
       [(= (length args1) (length args2))
        (define σh (match-one h1 h2))
        (cond
          [(not σh) #f]
          [else
           (define σa (match-many args1 args2))
           (and σa (merge-substs σh σa))])]
       [else #f])]
    [(_ _) #f]))

;; Combine two substitutions, failing if they assign different types to
;; the same variable.
(define (merge-substs σ1 σ2)
  (let/ec return
    (for/fold ([acc σ2]) ([(k v) (in-hash σ1)])
      (cond
        [(hash-has-key? acc k)
         (cond
           [(equal? v (hash-ref acc k)) acc]
           [else (return #f)])]
        [else (hash-set acc k v)]))))

;; ----- superclass closure ------------------------------------------

;; Return the list of all predicates derivable from p by superclass
;; expansion (including p itself).
(define (by-super env p)
  (match-define (pred name args) p)
  (define cinfo (env-ref-class env name))
  (cond
    [(not cinfo) (list p)]
    [else
     (define params (class-info-params cinfo))
     (define σ
       (for/fold ([acc empty-subst]) ([param (in-list params)]
                                      [arg (in-list args)])
         (subst-extend acc param arg)))
     (cons p
           (apply append
                  (for/list ([sp (in-list (class-info-supers cinfo))])
                    (by-super env (apply-subst σ sp)))))]))

(define (super-closure env hypotheses)
  (apply append (for/list ([h (in-list hypotheses)]) (by-super env h))))

;; ----- entailment --------------------------------------------------

(define (entail? env hypotheses target)
  (define hyp-closure (super-closure env hypotheses))
  (cond
    [(for/or ([h (in-list hyp-closure)]) (equal? h target)) #t]
    [else (entail-by-inst? env hypotheses target)]))

(define (entail-by-inst? env hypotheses target)
  (define inst (find-matching-instance env (pred-class target) target))
  (cond
    [(not inst) #f]
    [else
     (define σ (match-pred (instance-info-head inst) target))
     (for/and ([cp (in-list (instance-info-context inst))])
       (entail? env hypotheses (apply-subst σ cp)))]))

;; Phase 37: pick the most-specific matching instance of `class-name`
;; for the given `target` predicate.  Returns #f when nothing matches;
;; raises `exn:fail` with an overlap-explanation when the maximal-
;; specific set has more than one element (the matches are
;; incomparable).
;;
;; Specificity rule: instance A is *strictly more specific* than B if
;; `match-pred B.head A.head` succeeds (B can be specialised to A)
;; but `match-pred A.head B.head` fails (A cannot be specialised
;; back).  The maximal-specific set is the set of matches that no
;; other match strictly generalises.
(define (find-matching-instance env class-name target)
  (define matches
    (for/list ([inst (in-list (env-instances env class-name))]
               #:when (match-pred (instance-info-head inst) target))
      inst))
  (cond
    [(null? matches) #f]
    [else
     (define maximal
       (for/list ([i (in-list matches)]
                  #:unless (for/or ([j (in-list matches)]
                                    #:unless (eq? i j))
                             (strictly-more-specific? j i)))
         i))
     (cond
       [(null? maximal)
        ;; Mathematically impossible — at least one element of a finite
        ;; partially-ordered set is always maximal.  Treat as a logic
        ;; error.
        (error 'find-matching-instance
               "no maximal match (internal invariant violated)")]
       [(> (length maximal) 1)
        (raise
         (exn:fail
          (format "overlapping instances for ~s: ~s"
                  (pred->datum target)
                  (for/list ([m (in-list maximal)])
                    (pred->datum (instance-info-head m))))
          (current-continuation-marks)))]
       [else (car maximal)])]))

(define (strictly-more-specific? a b)
  (and (match-pred (instance-info-head b) (instance-info-head a))
       (not (match-pred (instance-info-head a) (instance-info-head b)))))

;; Two instance heads are equivalent (α-renaming away) when each
;; matches the other.  Used at env-extend-instance to detect duplicate
;; registrations.
(define (instance-heads-equivalent? h1 h2)
  (and (match-pred h1 h2) (match-pred h2 h1)))

;; Phase 37: a class has overlap when two of its instances' heads
;; unify — meaning some concrete target matches both.  This covers
;; both strictly-more-specific pairs (e.g. (Box a) vs (Box Integer))
;; and incomparable pairs (e.g. (P2 Integer b) vs (P2 a Integer)).
;; When overlap exists, all instances of the class compile via the
;; fingerprint-named static-dispatch path; the runtime dispatch table
;; isn't reliable because two instances can share the same outer
;; ctor tag.
(define (env-class-has-overlap? env class-name)
  (define insts (env-instances env class-name))
  (for/or ([i (in-list insts)])
    (for/or ([j (in-list insts)]
             #:unless (eq? i j))
      (heads-unify? (instance-info-head i) (instance-info-head j)))))

(define (heads-unify? h1 h2)
  (with-handlers ([exn:fail:unify? (lambda (_) #f)])
    (define args1 (pred-args h1))
    (define args2 (pred-args h2))
    (cond
      [(not (= (length args1) (length args2))) #f]
      [else
       (for/fold ([s empty-subst]) ([a (in-list args1)] [b (in-list args2)])
         (unify (apply-subst s a) (apply-subst s b)))
       #t])))

;; ----- context reduction ------------------------------------------

;; Drop every predicate that the class env fully discharges.  The
;; remaining predicates are those in head-normal form: they have at
;; least one type variable at the head of their argument list and
;; cannot be dispatched at this point.
(define (reduce-context env hypotheses preds)
  (let loop ([ps preds] [acc '()])
    (cond
      [(null? ps) (reverse (remove-duplicates acc))]
      [else
       (define p (car ps))
       (cond
         [(in-hnf? p)
          ;; Still keep unless redundant against hypotheses.
          (cond
            [(member p (super-closure env hypotheses))
             (loop (cdr ps) acc)]
            [else (loop (cdr ps) (cons p acc))])]
         [(entail? env hypotheses p)
          (loop (cdr ps) acc)]
         [else
          (raise
           (exn:fail
            (format "no instance for ~s" (pred->datum p))
            (current-continuation-marks)))])])))

;; A predicate is in head-normal form when at least one of its
;; arguments has a type-variable head (e.g. `a`, `(Maybe a)` qualifies;
;; `Integer` does not).
(define (in-hnf? p)
  (for/or ([arg (in-list (pred-args p))]) (hnf-type? arg)))

(define (hnf-type? t)
  (match t
    [(tvar _)       #t]
    [(tcon _)       #f]
    [(tapp h _)     (hnf-type? h)]))

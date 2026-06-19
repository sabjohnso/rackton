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
         current-reduce-blame
         by-super
         match-pred
         normalize-type
         normalize-type/guarded
         tyfam-clauses-overlap?)

(require racket/match
         racket/list
         racket/set
         "types.rkt"
         "env.rkt"
         "unify.rkt"
         "nat-solve.rkt"
         "diagnostic.rkt")

;; A syntax object to blame for a constraint error raised inside
;; `reduce-context` (which has no per-pred source).  Callers that know
;; the enclosing form parameterize this; when #f the error is raised
;; without a location, as before.
(define current-reduce-blame (make-parameter #f))

;; Raise a constraint error, attaching the blame location when one is
;; in scope so the message points at the offending form.
(define (raise-constraint-error msg)
  (define stx (current-reduce-blame))
  (if stx
      (raise-syntax-error 'infer msg stx)
      (raise (exn:fail msg (current-continuation-marks)))))

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
  (cond
    ;; A Nat-arithmetic head (e.g. `(+ a 1)` in an `(Array (+ a 1))`
    ;; instance) matches a Nat target by SOLVING the equation rather than
    ;; matching it structurally: `(+ a 1)` against `3` binds `a := 2`.
    ;; This subsumes the literal/literal case and `(* 2 n)`-style heads.
    ;; Handled before the structural `tapp` clause because an arithmetic
    ;; expression is a `tapp` whose operands must not be matched termwise.
    [(and (nat-expr? src) (nat-expr? dst)) (nat-match-one src dst)]
    [else (match-one/structural src dst)]))

;; Solve `src = dst` over the Nats, but keep `match-one`'s one-way
;; discipline: accept the result only if it binds variables of the
;; SOURCE (the instance-head pattern) and never of the target.  A
;; solution that would specialise the target is no match here.
(define (nat-match-one src dst)
  (define σ (solve-nat-equation src dst))
  (cond
    [(not σ) #f]
    [(let ([src-vars (type-vars src)])
       (for/and ([k (in-hash-keys σ)]) (set-member? src-vars k)))
     σ]
    [else #f]))

(define (match-one/structural src dst)
  (match* (src dst)
    [((tvar α) t)          (subst-singleton α t)]
    [((tcon c) (tcon c2))  (if (eq? c c2) empty-subst #f)]
    ;; Type-level Nat literals: a literal-keyed instance head (e.g.
    ;; `(CodeAt 5)`) matches a target only when the literals coincide.
    ;; A symbolic `(CodeAt n)` (dst a tvar) does NOT match a literal head,
    ;; so a ground type-level lookup reduces only on a known literal.
    [((tnat c1) (tnat c2)) (if (= c1 c2) empty-subst #f)]
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
  (apply append (for/list ([h (in-list hypotheses)]) (hyp-closure-1 env h))))

;; The constraints a single hypothesis provides: a constraint synonym
;; provides each of its (recursively expanded) components; an ordinary
;; predicate provides itself plus its superclasses (`by-super`).
(define (hyp-closure-1 env h)
  (cond
    [(expand-constraint-syn env h)
     => (lambda (comps)
          (cons h (apply append
                         (for/list ([c (in-list comps)]) (hyp-closure-1 env c)))))]
    [else (by-super env h)]))

;; ----- entailment --------------------------------------------------

(define (entail? env hypotheses target)
  (cond
    [(equality-pred? target)
     ;; `(~ τ σ)` is a primitive type-equality
     ;; constraint, not a class predicate.  It's discharged iff
     ;; its two arguments unify.  When one side is a tvar or a
     ;; refinable skolem, unify succeeds and binds — but since
     ;; entail? is asked of an already-substituted predicate, by
     ;; the time we get here both sides are in their final form.
     (equality-discharged? target)]
    [else
     (define hyp-closure (super-closure env hypotheses))
     (cond
       [(for/or ([h (in-list hyp-closure)]) (equal? h target)) #t]
       ;; A structural-over-products class on a tuple type entails iff
       ;; the class holds of every element (an exact hypothesis above
       ;; wins first, so a given `(Eq (Tuple a b))` is not decomposed).
       [(tuple-structural-subgoals target)
        => (lambda (subgoals)
             (for/and ([g (in-list subgoals)]) (entail? env hypotheses g)))]
       ;; A constraint synonym holds iff all its components hold.
       [(expand-constraint-syn env target)
        => (lambda (subgoals)
             (for/and ([g (in-list subgoals)]) (entail? env hypotheses g)))]
       [else (entail-by-inst? env hypotheses target)])]))

;; If `p`'s head names a constraint synonym, return the component
;; predicates with the synonym's parameters substituted by `p`'s
;; arguments; otherwise #f.  Used both as a goal (subgoals to discharge)
;; and, via `super-closure`, as a hypothesis (components it provides).
(define (expand-constraint-syn env p)
  (define syn (env-ref-constraint-syn env (pred-class p) #f))
  (and syn
       (let ([params (car syn)] [comps (cdr syn)])
         (and (= (length params) (length (pred-args p)))
              (let ([sub (for/fold ([s empty-subst])
                                   ([param (in-list params)] [arg (in-list (pred-args p))])
                           (subst-extend s param arg))])
                (for/list ([c (in-list comps)])
                  (pred (pred-class c)
                        (map (lambda (a) (apply-subst sub a)) (pred-args c)))))))))

;; The classes whose single method is structural over a product, so a
;; variadic `(Tuple t …)` satisfies them iff each element does.  A
;; fixed-arity instance can't express this (the arity varies), so the
;; reduction is built in here rather than declared in the prelude.
(define structural-tuple-classes (seteq 'Eq 'Ord 'Show))

;; If `p` is `(C (Tuple t1 … tn))` for a structural class C, return the
;; element subgoals `(C t1) … (C tn)`; otherwise #f.
(define (tuple-structural-subgoals p)
  (and (set-member? structural-tuple-classes (pred-class p))
       (match (pred-args p)
         ;; Both tuple heads: `Tuple` (any arity) and `Pair` (arity 2).
         [(list (tapp (tcon 'Tuple) elems))
          (for/list ([e (in-list elems)]) (pred (pred-class p) (list e)))]
         [(list (tapp (tcon 'Pair) (and elems (list _ _))))
          (for/list ([e (in-list elems)]) (pred (pred-class p) (list e)))]
         [_ #f])))

;; A `~` predicate has class-name `'~` and exactly two args.
(define (equality-pred? p)
  (and (eq? (pred-class p) '~)
       (= (length (pred-args p)) 2)))

;; Discharge a `~` equality: returns #t if the two sides unify
;; (which for two fully-substituted concrete types just means
;; they're equal).
(define (equality-discharged? p)
  (with-handlers ([exn:fail:unify? (lambda (_) #f)])
    (define args (pred-args p))
    (unify (car args) (cadr args))
    #t))

(define (entail-by-inst? env hypotheses target)
  (define inst (find-matching-instance env (pred-class target) target))
  (cond
    [(not inst) #f]
    [else
     (define σ (match-pred (instance-info-head inst) target))
     (for/and ([cp (in-list (instance-info-context inst))])
       (entail? env hypotheses (apply-subst σ cp)))]))

;; Pick the most-specific matching instance of `class-name`
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
        (define msg-doc
          (labeled-block
           (format "overlapping instances for ~a:" (format-pred target))
           (for/list ([m (in-list maximal)])
             (format-pred (instance-info-head m)))))
        (raise
         (exn:fail
          (render-doc msg-doc (current-type-columns))
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

;; A class has overlap when two of its instances' heads
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
         [(equality-pred? p)
          ;; A `~` constraint at reduce-context time:
          ;; if the two sides unify it's discharged (drop); if a
          ;; matching hypothesis is in scope it's also discharged;
          ;; otherwise keep it residual so the surrounding scope
          ;; can hand it off as a function-signature constraint.
          (cond
            [(equality-discharged? p) (loop (cdr ps) acc)]
            [(member p hypotheses) (loop (cdr ps) acc)]
            [else
             ;; Both sides concrete and disagree — flag now with a
             ;; clear "type-equality fails" message instead of
             ;; letting it pass through as an unsolved residual.
             (define args (pred-args p))
             (cond
               [(and (not (has-tvar? (car args)))
                     (not (has-tvar? (cadr args))))
                (match-define (list l r)
                  (format-types (list (car args) (cadr args))))
                (raise-constraint-error
                 (format "type-equality fails: ~a ≠ ~a" l r))]
               [else (loop (cdr ps) (cons p acc))])])]
         [(and (not (member p (super-closure env hypotheses)))
               (tuple-structural-subgoals p))
          ;; A tuple's structural constraint reduces to one constraint
          ;; per element; splice those into the worklist so each is then
          ;; discharged (concrete element) or kept residual (tvar
          ;; element) in head-normal form.  Skip when an exact hypothesis
          ;; already covers `p` (handled by the in-hnf?/entail? arms).
          => (lambda (subgoals) (loop (append subgoals (cdr ps)) acc))]
         [(expand-constraint-syn env p)
          ;; A constraint-synonym goal reduces to its component
          ;; constraints; splice them so each is discharged or kept.
          => (lambda (subgoals) (loop (append subgoals (cdr ps)) acc))]
         [(in-hnf? p)
          ;; Still keep unless redundant against hypotheses.
          (cond
            [(member p (super-closure env hypotheses))
             (loop (cdr ps) acc)]
            [else (loop (cdr ps) (cons p acc))])]
         [(entail? env hypotheses p)
          (loop (cdr ps) acc)]
         [else
          ;; Enrich the "no instance" error with the list of
          ;; instances we do have for this class, so the user can see
          ;; whether they hit a typo, a missing instance for their
          ;; specific type, or a class that's missing all instances.
          ;; Also suggest `#:deriving Class` when the class is
          ;; auto-derivable and the missing-instance type is locally
          ;; defined (so the user can add the clause).
          (define cls (pred-class p))
          (define available (env-instances env cls))
          ;; Build the message as a structured doc and render it once to
          ;; the current width, so the instance list reflows to the
          ;; terminal instead of overflowing as a single `~s` line.
          (define avail-doc
            (cond
              [(null? available)
               (doc-text (format " (no instances declared for ~a)" cls))]
              [else
               (doc-nest
                2
                (doc-cat doc-line
                         (labeled-block
                          (format "available ~a instances:" cls)
                          (for/list ([inst (in-list available)])
                            (format-pred (instance-info-head inst))))))]))
          (define derive-hint
            (deriving-suggestion env cls (pred-args p)))
          (define msg-doc
            (doc-cat (doc-text "no instance for ")
                     (doc-text (format-pred p))
                     avail-doc
                     (doc-text derive-hint)))
          (raise-constraint-error
           (render-doc msg-doc (current-type-columns)))])])))

;; Suggest `#:deriving Class` when (a) Class is one of
;; the auto-derivable classes and (b) the missing-instance type's
;; head is a known data type.  Returns a formatted hint or "".
(define derivable-classes
  '(Eq Ord Show Functor Foldable Traversable Bifunctor
    Semigroup Monoid Prism))

(define (deriving-suggestion env cls args)
  (cond
    [(not (memq cls derivable-classes)) ""]
    [else
     (define head-tcon
       (for/or ([a (in-list args)])
         (let loop ([t a])
           (match t
             [(tcon n) n]
             [(tapp h _) (loop h)]
             [_ #f]))))
     (cond
       [(and head-tcon (env-ref-tcon env head-tcon))
        (format "\n  hint: add #:deriving ~s to the data declaration for ~s"
                cls head-tcon)]
       [else ""])]))

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

;; Does the type still contain any tvars?
(define (has-tvar? t)
  (not (set-empty? (type-vars t))))

;; ----- type-family normalization -----------------------

;; Walk a type, eagerly rewriting type-family applications.  A type
;; application `(Foo τ ...)` is normalized when `Foo` is a registered
;; associated-type name for some class C, AND the args match a
;; concrete instance head of C.  Rewrites to the instance's bound rhs
;; (with the instance head's tvars substituted by `τ ...`).
;;
;; Normalization is bottom-up: arguments are normalized first so
;; nested family applications resolve in inner-then-outer order.
;; When no matching instance is found, the application is left
;; symbolic — a later substitution may make it normalizable.
(define (normalize-type env t)
  (match t
    [(tvar _) t]
    [(tcon _) t]
    [(tapp h args)
     (define h*    (normalize-type env h))
     (define args* (for/list ([a (in-list args)]) (normalize-type env a)))
     (define rewritten
       (cond
         [(tcon? h*) (or (try-reduce-tyfam env (tcon-name h*) args*)
                         (try-normalize-family env (tcon-name h*) args*))]
         [else #f]))
     (or rewritten (make-tapp h* args*))]
    [(qual cs body)
     (mqual (for/list ([c (in-list cs)]) (normalize-type env c))
            (normalize-type env body))]
    [(pred c args)
     (pred c (for/list ([a (in-list args)]) (normalize-type env a)))]
    [_ t]))

;; ----- standalone type-family reduction (Feature 1) -----------------
;; A CLOSED family reduces by trying its ordered clauses top-to-bottom:
;; a clause fires when its LHS one-way-matches the arguments AND every
;; earlier clause is APART from the arguments (no unifier) — otherwise the
;; application is stuck, since a later substitution could make an earlier
;; clause apply (soundness).  An OPEN family reduces by coherent single
;; lookup: its equations are non-overlapping, so at most one matches and
;; no apartness check is needed.  The rewritten rhs is itself normalized,
;; under a fuel budget that turns a non-terminating family into a clear
;; compile-time error instead of a hang.

;; Remaining type-level reduction steps along the current chain.  Reset
;; per outer reduction; decremented on each family rewrite.
(define current-tyfam-fuel (make-parameter 10000))

(define (try-reduce-tyfam env name args)
  (define info (env-ref-tyfam env name))
  (cond
    [(not info) #f]
    [(not (= (length args) (tyfam-info-arity info))) #f]   ; partial ⇒ stuck
    [else
     (define rhs
       (case (tyfam-info-openness info)
         [(closed) (closed-family-rhs name (tyfam-info-clauses info) args)]
         [(open)   (open-family-rhs (tyfam-info-clauses info) args)]
         [else     #f]))
     (cond
       [(not rhs) #f]
       [(<= (current-tyfam-fuel) 0)
        (raise-constraint-error
         (format "type-level reduction of ~a exceeded its budget (possible non-terminating type family)"
                 name))]
       [else
        (parameterize ([current-tyfam-fuel (sub1 (current-tyfam-fuel))])
          (normalize-type env rhs))])]))

;; First firing clause's rhs (with pattern vars substituted), or #f if the
;; family is stuck on these arguments.
(define (closed-family-rhs name clauses args)
  (let loop ([cs clauses] [earlier '()])
    (cond
      [(null? cs) #f]
      [else
       (match-define (cons pats0 rhs0) (car cs))
       (define-values (pats rhs) (freshen-clause pats0 rhs0))
       (define σ (match-many pats args))
       (cond
         ;; Fires only if it matches and no earlier clause overlaps `args`.
         [(and σ (all-apart? name earlier args)) (apply-subst σ rhs)]
         ;; Matches but an earlier clause could still apply ⇒ stuck.
         [σ #f]
         [else (loop (cdr cs) (cons pats earlier))])])))

;; Open family: the unique matching equation's rhs, or #f.  Coherence
;; (non-overlapping heads) guarantees at most one match.
(define (open-family-rhs clauses args)
  (for/or ([c (in-list clauses)])
    (match-define (cons pats0 rhs0) c)
    (define-values (pats rhs) (freshen-clause pats0 rhs0))
    (define σ (match-many pats args))
    (and σ (apply-subst σ rhs))))

;; Are all earlier clauses' patterns apart from these arguments — i.e. is
;; there no substitution unifying them?  Apartness lets a later (e.g.
;; variable) clause fire without an earlier clause silently shadowing it.
(define (all-apart? name earlier-pats args)
  (define target (make-tapp (tcon name) args))
  (for/and ([ep (in-list earlier-pats)])
    (apart? (make-tapp (tcon name) ep) target)))

(define (apart? a b)
  (with-handlers ([exn:fail:unify? (lambda (_) #t)])
    (unify a b)
    #f))

;; For an OPEN family: do any two equation heads overlap — i.e. unify, so
;; some argument list could match both?  Returns #t on the first such
;; pair (a coherence violation).  Closed families need no such check.
(define (tyfam-clauses-overlap? name clauses)
  (let loop ([cs clauses])
    (cond
      [(or (null? cs) (null? (cdr cs))) #f]
      [else
       (define-values (pats-i _ri) (freshen-clause (car (car cs)) (cdr (car cs))))
       (define head-i (make-tapp (tcon name) pats-i))
       (or (for/or ([other (in-list (cdr cs))])
             (define-values (pats-j _rj) (freshen-clause (car other) (cdr other)))
             (not (apart? head-i (make-tapp (tcon name) pats-j))))
           (loop (cdr cs)))])))

;; Rename a clause's pattern/rhs type variables to fresh ones so they
;; cannot collide with variables in the arguments being matched.
(define (freshen-clause pats rhs)
  (define vars
    (for/fold ([s (type-vars rhs)]) ([p (in-list pats)])
      (set-union s (type-vars p))))
  (define σ
    (for/fold ([acc empty-subst]) ([v (in-set vars)])
      (subst-extend acc v (tvar (gensym v)))))
  (values (map (lambda (p) (apply-subst σ p)) pats)
          (apply-subst σ rhs)))

;; ----- guarded normalization (hot-path) -----------------------------
;; `normalize-type` walks and rebuilds a type and scans the class env per
;; head — too costly to run at every unify site unconditionally.  This
;; wrapper does it only when (a) the env actually declares some associated
;; type family, and (b) the type mentions a family name.  When no families
;; exist (the common case — the prelude declares none) it is a near-no-op.

;; The set of all associated-type-family names in `env`, memoized on the
;; identity of the (immutable, threaded) classes hash so repeated calls
;; during one inference don't re-scan the class table.
(define family-name-cache (make-weak-hasheq))
(define (env-type-family-names env)
  (hash-ref! family-name-cache (env-classes env)
             (lambda ()
               (for*/seteq ([(_ ci) (in-hash (env-classes env))]
                            [f (in-list (class-info-type-families ci))])
                 f))))

;; Does `t` apply any of `names` at a tcon head?
(define (type-mentions-family? names t)
  (let loop ([t t])
    (match t
      [(tapp h args)
       (or (and (tcon? h) (set-member? names (tcon-name h)))
           (loop h)
           (for/or ([a (in-list args)]) (loop a)))]
      [(qual cs body) (or (for/or ([c (in-list cs)]) (loop c)) (loop body))]
      [(pred _ args)  (for/or ([a (in-list args)]) (loop a))]
      [_ #f])))

(define (normalize-type/guarded env t)
  (define names (set-union (env-type-family-names env) (env-tyfam-names env)))
  (cond
    [(set-empty? names) t]
    [(type-mentions-family? names t) (normalize-type env t)]
    [else t]))

;; If `name` is the associated-type name of some class C, and `args`
;; matches an instance head of C, return that instance's rhs with the
;; instance head's tvars renamed to `args`.  Otherwise #f.
(define (try-normalize-family env name args)
  (define cls (env-class-owning-family env name))
  (cond
    [(not cls) #f]
    [else
     (define target (pred cls args))
     (with-handlers ([exn:fail? (lambda (_) #f)])
       (define inst (find-matching-instance env cls target))
       (cond
         [(not inst) #f]
         [else
          (define σ (match-pred (instance-info-head inst) target))
          (define binding
            (hash-ref (instance-info-type-family-bindings inst) name #f))
          (and binding σ (apply-subst σ binding))]))]))

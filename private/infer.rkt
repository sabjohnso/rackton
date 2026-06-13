#lang racket/base

;; Rackton — Hindley–Milner type inference (Algorithm W) with
;; let-generalization, algebraic data types, pattern matching, and
;; skolemization for declared signatures.
;;
;; Public entry points
;;   (infer-expr/fresh e env)
;;     → (values subst type)
;;     Type-check a single expression in a fresh tvar-supply scope.
;;
;;   (infer-program forms env) → env*
;;     Walk a list of top-level forms, registering data types and
;;     definitions in env, and return the resulting env.
;;
;; The implementation threads substitutions explicitly (functional style)
;; and uses a single piece of mutable state — a counter for fresh type
;; variables — confined to a parameter that is bound at every public
;; entry point.

(provide infer-expr/fresh
         infer-program
         infer-program+forms
         infer-program/phases
         infer-program-step
         require-spec->submod-spec
         def-scc-order
         generalize
         ;; surface ty-AST → scheme, under an env's aliases (REPL ,accepts)
         resolve-scheme
         ;; threaded inference state — the REPL persists one across inputs
         make-infer-state st-table
         current-dict-skolems
         current-prelude-build?
         current-allow-instance-redefinition?)

(require racket/match
         racket/set
         racket/list
         racket/string
         "types.rkt"
         "diagnostic.rkt"
         "env.rkt"
         "unify.rkt"
         "surface.rkt"
         "entail.rkt"
         "impl-symbols.rkt"
         "codegen-plan.rkt"
         "scheme-codec.rkt"
         ;; The Environment monad threads the type-resolution context
         ;; (alias table + expansion guard) — replaces current-aliases /
         ;; current-expanding.
         (only-in "monad/ctx.rkt"
                  ctx-return ctx-sequence asks local run-ctx let/ctx)
         ;; The Infer monad — the inference core is being moved into it,
         ;; one cluster at a time (PLAN.org Phase 3).
         (only-in "monad/infer.rkt"
                  infer-return infer-bind infer-map infer-sequence run-infer
                  let/infer let/infer+ begin/infer
                  ;; the threaded inference state (replaces the boxes)
                  infer-state infer-state? make-infer-state
                  infer-state-fresh-counter infer-state-pending-preds
                  infer-state-tables))

;; ----- fresh type variables -----------------------------------------
;; The fresh-name counter and the pending-pred bag live in the threaded
;; `infer-state` (private/monad/infer.rkt), not in boxes.  st:* are the pure
;; steppers used by the (non-monadic) driver; the m:* ops below are their
;; Infer-monad forms used by the expression core.

;; Return-typed-method use sites accumulated during inference of one
;; top-level definition.  A hashtable from the e:var's syntax object →
;; (list class-name method-name body-type).  After constraint
;; reduction completes, each entry's body-type is run through the
;; final substitution to determine the concrete instance and the
;; entry is graduated into `current-method-resolutions`.  (The accumulator
;; itself is now the 'method-uses channel in the threaded infer-state.)
;; The resolution tables — method-resolutions, method-dict-resolutions,
;; return-typed-methods, needs-dict-defs, instance-default-bodies — are now the
;; threaded infer-state's codegen-plan channels: inference accumulates them in
;; `st`, infer-program+forms reads them into the codegen-plan, and codegen
;; consumes them through its cg-ctx.  No parameters here.
;;
;; current-dict-skolems stays a config parameter: a hasheq from
;; skolem-tcon-name → local dict-arg-name, set around body inference of a
;; needs-dict-body def so the body's polymorphic class-method references
;; resolve to the locally-bound dict args.
(define current-dict-skolems (make-parameter (hasheq)))

;; The monomorphization log is the 'monomorphized-sites channel in the threaded
;; infer-state (a newest-first list); resolve-method-uses accumulates it and
;; infer-program+forms reads it out for the elaborator.

;; (The impl-name symbols of needs-dict return-typed instance methods that
;; must be force-exported are collected in codegen's cg-st now, and handed to
;; elaborate-finish, rather than through a parameter here.)

;; When checking a function body against a declared
;; signature, the body's expected return type is known up front.
;; The top:def declared-signature branch sets this parameter so
;; the body's `match` expression seeds its `result-tv` with the
;; declared codomain — which may be a refinable skolem — instead
;; of a fresh tvar.  Without this, each match arm's result tvar
;; would be a free tvar that the first arm pins to its concrete
;; type, leaking that refinement to later arms.
;; The parameter is one-shot: e:match consumes it and resets to
;; #f before recursing into the arm bodies, so nested `match`
;; expressions don't reuse the outer scope's expected type.
(define current-expected-type   (make-parameter #f))

;; ----- fresh-name supply + pending-pred accumulator (pure on `st`) ---
;; st:* step the immutable infer-state; they are the (non-monadic) driver's
;; interface to the threaded state.
(define (st:fresh st [prefix 'a])
  (define n (infer-state-fresh-counter st))
  (values (tvar (string->symbol (format "~a~a" prefix n)))
          (struct-copy infer-state st [fresh-counter (add1 n)])))

(define (st:add-preds st ps)
  (struct-copy infer-state st
               [pending-preds (append ps (infer-state-pending-preds st))]))

(define (st:apply-subst-to-preds st s)
  (struct-copy infer-state st
               [pending-preds (for/list ([p (in-list (infer-state-pending-preds st))])
                                (apply-subst s p))]))

(define (st:preds st) (infer-state-pending-preds st))

(define (st:set-preds st ps)
  (struct-copy infer-state st [pending-preds ps]))

;; Pull every pred whose free type vars share any var with `quantified-set`,
;; AND every fully-concrete pred (no free tvars at all).  Fully-concrete
;; preds are unaffected by any further outer-scope substitutions, so the
;; current generalization is our last chance to discharge them; deferring
;; would just leak them forever.  GADT equality-constraint surfacing relies
;; on this so a concrete `(~ Integer String)` from `(pair-eq 7 "hi")` surfaces
;; as an error.  Returns (values taken st').
(define (st:take-relevant-preds st quantified-set)
  (define-values (taken kept)
    (partition (lambda (p)
                 (define vs (type-vars p))
                 (or (set-empty? vs)
                     (not (set-empty? (set-intersect vs quantified-set)))))
               (infer-state-pending-preds st)))
  (values taken (struct-copy infer-state st [pending-preds kept])))

;; ----- Infer-monad state ops (the expression core's interface) ------
;; Each is the monadic form of the matching st:* stepper, reading/writing the
;; threaded `st`.  ctx is unused: the engine's Reader-style config still rides
;; the current-* parameters, which the driver wraps around its *synchronous*
;; run-infer calls, so they remain in scope during execution.
(define ((m:fresh-tvar [prefix 'a]) _ctx st) (st:fresh st prefix))
(define ((m:add-preds ps) _ctx st) (values (void) (st:add-preds st ps)))
(define ((m:snapshot-preds) _ctx st) (values (st:preds st) st))
(define ((m:set-preds ps) _ctx st) (values (void) (st:set-preds st ps)))
(define ((m:apply-subst-to-preds s) _ctx st) (values (void) (st:apply-subst-to-preds st s)))
(define ((m:take-relevant-preds q) _ctx st) (st:take-relevant-preds st q))

;; ----- codegen-plan tables (in st's opaque `tables` field) ----------
;; Each channel is an immutable hash, lazily defaulting to empty.  The
;; stx-keyed tables (method uses/resolutions) use eq?; the symbol/list-keyed
;; ones (needs-dict-defs, instance-default-bodies) use equal?.
(define (table-empty key)
  (case key
    [(needs-dict-defs instance-default-bodies) (hash)]
    [(monomorphized-sites) '()]   ; a newest-first list, not a hash
    [else (hasheq)]))
(define (st-table st key) (hash-ref (infer-state-tables st) key (table-empty key)))
(define (st-table-set st key tbl)
  (struct-copy infer-state st [tables (hash-set (infer-state-tables st) key tbl)]))
(define (st-table-put st key k v)
  (st-table-set st key (hash-set (st-table st key) k v)))
;; Freeze a mutable eq-hash into an immutable one (for storing a locally-built
;; resolution table back into st).
(define (freeze-eq h) (for/hasheq ([(k v) (in-hash h)]) (values k v)))

;; record-* now write the 'method-uses channel as state transitions; the
;; expression core binds these m: ops, the driver uses the st: form directly.
(define (st:record-method-use st stx method-name class-param-tvars method-dict-entries)
  (st-table-put st 'method-uses stx
                (list 'return method-name class-param-tvars method-dict-entries)))
(define (st:record-inst-dispatch-use st stx method-name class-param-tvars)
  (st-table-put st 'method-uses stx (list 'inst-dispatch method-name class-param-tvars)))
(define (st:record-dict-use st stx method-name reqs sub)
  (define dict-entries
    (for/list ([req (in-list reqs)])
      (cons (car req) (for/list ([a (in-list (cdr req))]) (apply-subst sub a)))))
  (st-table-put st 'method-uses stx (list 'dict method-name dict-entries)))
(define ((m:record-method-use stx method-name class-param-tvars method-dict-entries) _ctx st)
  (values (void) (st:record-method-use st stx method-name class-param-tvars method-dict-entries)))
(define ((m:record-inst-dispatch-use stx method-name class-param-tvars) _ctx st)
  (values (void) (st:record-inst-dispatch-use st stx method-name class-param-tvars)))
(define ((m:record-dict-use stx method-name reqs sub) _ctx st)
  (values (void) (st:record-dict-use st stx method-name reqs sub)))

;; ----- generalize / instantiate -------------------------------------

;; Instantiate a scheme.  If the body is a qualified type, the constraints
;; are added to the running pred bag and the bare type is returned.  Also
;; strips a top-level `tforall` from the body — the quantified vars become
;; fresh tvars, the same way scheme-bound vars do.  This lets `(f 7)`
;; typecheck when `f` was bound at a tforall type by some outer rank-N
;; declaration.  Monadic core; `instantiate` is the bridged entry point.
(define (instantiate/m sch)
  (let/infer ([raw (match sch
                     [(scheme '() body) (infer-return body)]
                     [(scheme vs body)
                      (let/infer ([s (fresh-subst/m vs)])
                        (infer-return (apply-subst s body)))])]
              [unforalled (instantiate-tforall/m raw)])
    (cond
      [(qual? unforalled)
       (let/infer ([_ (m:add-preds (qual-constraints unforalled))])
         (instantiate-tforall/m (qual-body unforalled)))]
      [else (infer-return unforalled)])))

;; Strip any leading `tforall`, replacing each bound var with a fresh tvar.
;; Only the OUTERMOST tforall is unwrapped — nested tforalls (e.g. the
;; argument of a rank-2 function) stay intact so they can carry their
;; polymorphism into argument positions.
(define (instantiate-tforall/m t)
  (match t
    [(tforall vs body)
     (let/infer ([s (fresh-subst/m vs)])
       (instantiate-tforall/m (apply-subst s body)))]
    [_ (infer-return t)]))

;; Build a substitution sending each of `vs` to a distinct fresh tvar named
;; after it (the prefix is the variable, matching st:fresh's naming).
(define (fresh-subst/m vs)
  (let loop ([vs vs] [s empty-subst])
    (cond
      [(null? vs) (infer-return s)]
      [else
       (let/infer ([t (m:fresh-tvar (car vs))])
         (loop (cdr vs) (subst-extend s (car vs) t)))])))

;; Bidirectional check companion to `infer-expr`.
;; Checks that `expr` has type `expected-ty` under `env`, returning
;; the same shape `(values subst type)` that `infer-expr` does.
;; Three cases steer the work:
;;   - expected is `(tforall vs body)`: skolemize the bound vars to
;;     fresh refinable-skolem tcons, then check `expr` against the
;;     skolemized body.  This is what unblocks rank-N: a caller can
;;     pass `(lambda (x) x)` where `(All (a) (-> a a))` is expected
;;     and the lambda's parameter gets the skolem type as its env
;;     binding, so a downstream `(f 7)` is REJECTED (good — the
;;     polymorphic function isn't allowed to specialize to Integer
;;     at the wrong site).
;;   - expected is an arrow AND expr is a lambda: push the declared
;;     argument types into env and check the body against the
;;     codomain.  This generalizes the bidirectional-push used at
;;     top-level def to refinable lambdas inside any expression.
;;   - otherwise: fall back to `infer-expr` and unify the result
;;     with `expected-ty`.
(define (check-expr/m expr env expected-ty)
  (cond
    [(tforall? expected-ty)
     (match-define (tforall vs body) expected-ty)
     (define s-skol
       (for/fold ([s empty-subst]) ([v (in-list vs)])
         (subst-extend s v (tcon (gensym (format "$skolem.~a." v))))))
     (check-expr/m expr env (apply-subst s-skol body))]
    [(and (arrow? expected-ty) (e:lam? expr))
     (match-define (e:lam params body _) expr)
     (define-values (arg-tys cod)
       (unfold-arrow-checked expected-ty (length params) expr))
     (define env*
       (for/fold ([e env]) ([p (in-list params)] [t (in-list arg-tys)])
         (env-extend-var e p (scheme '() t))))
     (let/infer ([rb (check-expr/m body env* cod)])
       (let* ([s-body (car rb)] [t-body (cdr rb)])
         (infer-return
          (cons s-body
                (foldr make-arrow t-body
                       (for/list ([t (in-list arg-tys)]) (apply-subst s-body t)))))))]
    [else
     (let/infer ([r (infer-expr/m expr env)])
       (let* ([s (car r)] [t (cdr r)]
              [s-u (with-handlers
                    ([exn:fail:unify?
                      (lambda (_)
                        (raise-type-mismatch! (expr-stx expr) expected-ty (apply-subst s t)))])
                    (unify (apply-subst s t) expected-ty))])
         (infer-return (cons (subst-compose s-u s) (apply-subst s-u t)))))]))

;; Like `unfold-arrow` but raises a friendly typecheck error rather
;; than an internal exception when the expected arrow has fewer
;; arrows than the lambda has params.
(define (unfold-arrow-checked t n stx)
  (cond
    [(decl-arrow-depth-ge? t n) (unfold-arrow t n)]
    [else
     (raise-syntax-error 'infer
       (format "expected type ~a has fewer arrows than the lambda has params"
               (pretty-type t))
       (expr-stx stx))]))

;; Like `instantiate` but also returns the substitution it built so
;; callers can recover the fresh tvar that replaced any specific
;; scheme-bound variable.  Used to record both return-typed-method
;; sites and dict-requiring-method sites.  The
;; scheme body may carry nested quals when a method declared its own
;; qualifying context on top of the class head — both layers of
;; constraints are pulled out into the pending-preds box.
(define (instantiate/subst/m sch)
  (match sch
    [(scheme vs body)
     (let/infer ([s (fresh-subst/m vs)])
       (let* ([raw (apply-subst s body)])
         (let/infer ([_ (m:add-preds (qual-constraints-of raw))])
           (infer-return (cons (qual-body-deep raw) s)))))]))

(define (qual-body-deep t)
  (cond [(qual? t) (qual-body-deep (qual-body t))]
        [else t]))

;; Helper: given a list of `(class-name . param-name-list)`
;; dict requirements and the substitution produced by
;; `skolemize/tracked`, produce (values skolem-map arg-names) where
;;   skolem-map : hasheq from skolem-tcon-name → arg-name
;;   arg-names  : (Listof symbol) in declaration order
(define (build-dict-skolems reqs subst [env #f])
  ;; Returns:
  ;;   sk-map    : equal?-hash from (cons skolem-name method-name) →
  ;;               local dict-arg name
  ;;   arg-names : (Listof symbol) in declaration order — what the
  ;;               compiled lambda will accept as leading params.
  ;;
  ;; `reqs` may be either shape:
  ;;   (cons class-name (list tvar-name ...))     — legacy single-param shape
  ;;   (cons class-name (list pred-arg-type ...)) — multi-param shape
  ;; Walks superclass closures so inherited return-typed methods (e.g.
  ;; `pure` via Monad super of MonadState) get their own dict slot.
  (define (resolve-arg p)
    (cond
      [(symbol? p) (hash-ref subst p)]
      [else        (apply-subst subst p)]))
  (define (filter-skolems cls arg-types)
    (define cinfo (and env (env-ref-class env cls)))
    (cond
      [(and cinfo (= (length (class-info-params cinfo)) (length arg-types)))
       (define determined
         (for/fold ([acc (seteq)])
                   ([fd (in-list (class-info-fundeps cinfo))])
           (set-union acc (list->seteq (cdr fd)))))
       (for/list ([p (in-list arg-types)]
                  [cp (in-list (class-info-params cinfo))]
                  #:when (or (symbol? p) (tvar? p))
                  #:unless (set-member? determined cp))
         (resolve-arg p))]
      [else
       (for/list ([p (in-list arg-types)]
                  #:when (or (symbol? p) (tvar? p)))
         (resolve-arg p))]))
  (define-values (sk-map arg-names-rev)
    (for/fold ([sk (hash)] [args '()])
              ([req (in-list reqs)])
      (define cls (car req))
      (define arg-types (cdr req))
      (for/fold ([sk sk] [args args])
                ([pair (in-list (collect-dict-method-args cls arg-types env))])
        (define dm           (car pair))
        (define method-args  (cdr pair))
        (define dm-cls
          (or (and env (env-ref-method-class env dm))
              (case dm
                [(pure)   'Applicative]
                [(mempty) 'Monoid]
                [else cls])))
        (define skolems (filter-skolems dm-cls method-args))
        (for/fold ([sk sk] [args args]) ([sk-ty (in-list skolems)])
          (define skolem-name (tcon-name sk-ty))
          (define arg-name
            (string->symbol (format "$dict-~a-~a" dm skolem-name)))
          ;; Two constraints sharing a superclass (e.g. MonadState +
          ;; MonadEnv both reach Monad) would produce the same
          ;; (skolem . method) entry twice; keep the first occurrence
          ;; so the compiled lambda's params remain unique.
          (cond
            [(hash-has-key? sk (cons skolem-name dm))
             (values sk args)]
            [else
             (values (hash-set sk (cons skolem-name dm) arg-name)
                     (cons arg-name args))])))))
  (values sk-map (reverse arg-names-rev)))

;; Skolemize: replace each bound type variable with a fresh tcon, so the
;; declared signature acts rigidly — the body can't sneak a more specific
;; type past a polymorphic declaration.  Returns (values body extra-preds);
;; `extra-preds` are the skolem-instantiated constraints that callers must
;; treat as hypotheses while checking the body.
;; A tcon name represents a function-scheme skolem
;; (refinable at GADT pattern matches) iff it begins with
;; `$skolem.` — see `skolemize` below for the gensym format.
;; Other skolem flavours (`$ex-skolem.`, `$inst-skolem.`,
;; `$method-skolem.`) are NOT refinable: they're bound by separate
;; rules (existentials, instance qual, method qual) and refining
;; them would unsoundly leak existential or class-scope identity.
(define (refinable-skolem-tcon? t)
  (and (tcon? t)
       (let ([n (symbol->string (tcon-name t))])
         (and (>= (string-length n) 8)
              (equal? (substring n 0 8) "$skolem.")))))

;; A GADT-aware soft unification.  Like standard unify, but when
;; comparing two tcons (or arms of tapps) where one side is a
;; refinable function-scheme skolem and the other is a concrete
;; type, bind the skolem.  Returns (values tvar-subst skolem-subst)
;; where the tvar-subst goes into the global running substitution
;; and the skolem-subst is applied only to the local arm's expected
;; result type.  Raises exn:fail:unify on hard mismatch (two
;; distinct concrete tcons with no skolem to bind).
(define (gadt-unify σ τ)
  (let loop ([σ σ] [τ τ] [tvar-s empty-subst] [skol-s empty-subst])
    (define σ′ (apply-subst tvar-s (apply-skolem-subst skol-s σ)))
    (define τ′ (apply-subst tvar-s (apply-skolem-subst skol-s τ)))
    (cond
      [(equal? σ′ τ′) (values tvar-s skol-s)]
      [else
       (match* (σ′ τ′)
         [((tvar α) _)
          (when (set-member? (type-vars τ′) α)
            (raise-unify! 'occurs σ′ τ′))
          (values (subst-extend tvar-s α τ′) skol-s)]
         [(_ (tvar α))
          (when (set-member? (type-vars σ′) α)
            (raise-unify! 'occurs σ′ τ′))
          (values (subst-extend tvar-s α σ′) skol-s)]
         [((? refinable-skolem-tcon?) _)
          (values tvar-s (hash-set skol-s (tcon-name σ′) τ′))]
         [(_ (? refinable-skolem-tcon?))
          (values tvar-s (hash-set skol-s (tcon-name τ′) σ′))]
         [((tcon c1) (tcon c2)) #:when (eq? c1 c2)
          (values tvar-s skol-s)]
         [((tapp h1 args1) (tapp h2 args2))
          (cond
            [(= (length args1) (length args2))
             (define-values (tvar-s1 skol-s1)
               (loop h1 h2 tvar-s skol-s))
             (for/fold ([ts tvar-s1] [ss skol-s1])
                       ([a (in-list args1)] [b (in-list args2)])
               (loop a b ts ss))]
            [else (raise-unify! 'arity σ′ τ′)])]
         [(_ _) (raise-unify! 'mismatch σ′ τ′)])])))

;; Apply a skolem-subst (hash from skolem tcon-name → type) to a
;; type.  Walks through tvars / tcons / tapps and replaces any tcon
;; whose name appears in the skolem-subst.
(define (apply-skolem-subst skol-s t)
  (cond
    [(hash-empty? skol-s) t]
    [else
     (match t
       [(tvar _) t]
       [(tcon n) (hash-ref skol-s n t)]
       [(tapp h args) (make-tapp (apply-skolem-subst skol-s h)
                                 (for/list ([a (in-list args)])
                                   (apply-skolem-subst skol-s a)))]
       [(pred c args)
        (pred c (for/list ([a (in-list args)])
                  (apply-skolem-subst skol-s a)))]
       [(qual cs body)
        (mqual (for/list ([c (in-list cs)])
                 (apply-skolem-subst skol-s c))
               (apply-skolem-subst skol-s body))]
       [else t])]))

(define (skolemize sch)
  (match sch
    [(scheme '() body)
     (cond
       [(qual? body) (values (qual-body body) (qual-constraints body))]
       [else         (values body '())])]
    [(scheme vs body)
     (define s
       (for/fold ([s empty-subst]) ([v (in-list vs)])
         (subst-extend s v (tcon (gensym (format "$skolem.~a." v))))))
     (define skol (apply-subst s body))
     (cond
       [(qual? skol) (values (qual-body skol) (qual-constraints skol))]
       [else         (values skol '())])]))

;; Like `skolemize`, but also returns the substitution it used.  The
;; caller can read out which skolem-tcon replaced which scheme-bound
;; var — needed to map return-typed-method references in
;; a needs-dict-body back to local dict-arg names instead of looking
;; up the (nonexistent) per-skolem-tcon impl.
(define (skolemize/tracked sch)
  (match sch
    [(scheme vs body)
     (define s
       (for/fold ([s empty-subst]) ([v (in-list vs)])
         (subst-extend s v (tcon (gensym (format "$skolem.~a." v))))))
     (define skol (apply-subst s body))
     (define-values (b preds)
       (cond
         [(qual? skol) (values (qual-body skol) (qual-constraints skol))]
         [else         (values skol '())]))
     (values b preds s)]))

;; ---------- FD improvement -----------------------------------------
;;
;; Functional dependencies let an instance's "determined" args be
;; resolved from its "determining" args.  Given a pred `(C ts…)` with
;; some tvars unknown, we find each instance of C whose determining
;; args match `ts` (under a one-way substitution σ) and unify the
;; pred's determined positions with σ-applied instance determined
;; args.  This may close out unknowns that ordinary unification can't
;; reach on its own.
;;
;; Runs once before reducing the context.  Returns the composed
;; substitution it produced (callers compose it into their running
;; subst); the pending-preds box is updated in place.

(define (improve-by-fds env st)
  (let loop ([s empty-subst] [st st])
    (define preds (st:preds st))
    (define s′ s)
    (for ([p (in-list preds)])
      (define cinfo (env-ref-class env (pred-class p)))
      (when (and cinfo (not (null? (class-info-fundeps cinfo))))
        (define class-params (class-info-params cinfo))
        (define param-index
          (for/hasheq ([param (in-list class-params)] [i (in-naturals)])
            (values param i)))
        (for ([fd (in-list (class-info-fundeps cinfo))])
          (define lhs-pos
            (for/list ([d (in-list (car fd))]) (hash-ref param-index d)))
          (define rhs-pos
            (for/list ([d (in-list (cdr fd))]) (hash-ref param-index d)))
          (define pred-args-now
            (for/list ([a (in-list (pred-args p))]) (apply-subst s′ a)))
          (define pred-lhs (for/list ([i (in-list lhs-pos)])
                             (list-ref pred-args-now i)))
          (for ([inst (in-list (env-instances env (pred-class p)))])
            (define inst-args (pred-args (instance-info-head inst)))
            (define inst-lhs (for/list ([i (in-list lhs-pos)])
                               (list-ref inst-args i)))
            (define match-σ (match-many inst-lhs pred-lhs))
            (when match-σ
              (for ([ri (in-list rhs-pos)])
                (define pr (apply-subst s′ (list-ref pred-args-now ri)))
                (define ir (apply-subst match-σ (list-ref inst-args ri)))
                (with-handlers ([exn:fail:unify? (lambda (_) (void))])
                  (define u (unify pr ir))
                  (set! s′ (subst-compose u s′)))))))))
    (cond
      [(equal? s′ s) (values s st)]      ; fixpoint
      [else
       (loop s′ (st:apply-subst-to-preds st s′))])))

;; FD improvement against a set of HYPOTHESES (rather than concrete
;; instances).  For a polymorphic definition whose context carries a
;; fundep-bearing constraint — e.g. `(ArrowLoop cat p) => …`, with
;; `cat -> p` — the hypothesis pins the determined param for that scope:
;; if a pending pred and a hypothesis are the same class and their
;; determining (lhs) args unify, the fundep forces their determined (rhs)
;; args to unify too.  This is what lets a `proc rec` over an abstract
;; arrow discharge its `(Arrow cat a)` / `(Prod a)` residuals against the
;; declared `(ArrowLoop cat p)` (so `a := p`).  Without it the body's
;; fresh product var never connects to the signature's `p`.
(define (improve-by-hyp-fds env hypotheses st)
  (let loop ([s empty-subst] [st st])
    (define preds (st:preds st))
    (define s′ s)
    (for ([p (in-list preds)])
      (define cinfo (env-ref-class env (pred-class p)))
      (when (and cinfo (not (null? (class-info-fundeps cinfo))))
        (define class-params (class-info-params cinfo))
        (define param-index
          (for/hasheq ([param (in-list class-params)] [i (in-naturals)])
            (values param i)))
        (for ([fd (in-list (class-info-fundeps cinfo))])
          (define lhs-pos (for/list ([d (in-list (car fd))]) (hash-ref param-index d)))
          (define rhs-pos (for/list ([d (in-list (cdr fd))]) (hash-ref param-index d)))
          (define p-args (for/list ([a (in-list (pred-args p))]) (apply-subst s′ a)))
          (for ([h (in-list hypotheses)]
                #:when (eq? (pred-class h) (pred-class p)))
            (define h-args (for/list ([a (in-list (pred-args h))]) (apply-subst s′ a)))
            (when (= (length h-args) (length p-args))
              ;; unify the determining args; on success the fundep forces
              ;; the determined args to agree too.
              (with-handlers ([exn:fail:unify? (lambda (_) (void))])
                (define σl
                  (for/fold ([σ empty-subst]) ([i (in-list lhs-pos)])
                    (subst-compose (unify (apply-subst σ (list-ref h-args i))
                                          (apply-subst σ (list-ref p-args i)))
                                   σ)))
                (define σ
                  (for/fold ([σ σl]) ([i (in-list rhs-pos)])
                    (subst-compose (unify (apply-subst σ (list-ref h-args i))
                                          (apply-subst σ (list-ref p-args i)))
                                   σ)))
                (set! s′ (subst-compose σ s′))))))))
    (cond
      [(equal? s′ s) (values s st)]
      [else
       (loop s′ (st:apply-subst-to-preds st s′))])))

(define (match-many srcs dsts)
  ;; Borrowed from entail.rkt: one-way match returning a substitution.
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

(define (merge-substs σ1 σ2)
  (let/ec return
    (for/fold ([acc σ2]) ([(k v) (in-hash σ1)])
      (cond
        [(hash-has-key? acc k)
         (cond
           [(equal? v (hash-ref acc k)) acc]
           [else (return #f)])]
        [else (hash-set acc k v)]))))

;; Generalize: take the type's quantifiable tvars, pull the constraints
;; that mention them out of the pred-box, reduce them against the env,
;; and wrap into a `(scheme vs (qual cs ty))`.  Bound tvars are renamed
;; to nice sequential names (a, b, c, …) for readability.  Runs FD
;; improvement first, so an instance whose determining args match the
;; pred's can pin the determined args before generalisation.
(define (generalize env ty st [hypotheses '()])
  (define-values (sch _fd-sub st′) (generalize* env ty st hypotheses))
  (values sch st′))

;; Infer-monad form, for the expression core (infer-let/m, infer-letrec/m).
(define (generalize/m env ty [hypotheses '()])
  (lambda (_ctx st) (generalize env ty st hypotheses)))

;; Like `generalize`, but also returns the functional-dependency
;; improvement substitution it computed.  Callers that go on to resolve
;; return-typed method uses (in the undeclared-def path) need that exact
;; substitution so a method whose target type is fundep-determined (e.g.
;; `mk-prod` over an arrow whose product is fixed by `cat -> p`) resolves
;; against the improved type rather than an ambiguous tvar.  Returning the
;; subst — instead of re-running `improve-by-fds`, which mutates the shared
;; pred box again — keeps generalization behavior identical.
(define (generalize* env ty st [hypotheses '()])
  (define-values (fd-sub st1) (improve-by-fds env st))
  (define ty* (apply-subst fd-sub ty))
  (define env-fv (env-vars-free-vars env))
  (define ty-fv  (type-vars ty*))
  (define q-set  (set-subtract ty-fv env-fv))
  (define-values (preds st2) (st:take-relevant-preds st1 q-set))
  (define reduced (reduce-context env hypotheses preds))
  (define final-q
    (for/fold ([acc q-set]) ([p (in-list reduced)])
      (set-union acc (type-vars p))))
  (define quantified-raw
    (sort (set->list (set-subtract final-q env-fv)) symbol<?))
  (define nice (nice-tvar-names (length quantified-raw) env-fv))
  (define σ
    (for/fold ([s empty-subst]) ([old (in-list quantified-raw)]
                                 [new (in-list nice)])
      (subst-extend s old (tvar new))))
  (values (scheme nice (mqual (for/list ([p (in-list reduced)]) (apply-subst σ p))
                              (apply-subst σ ty*)))
          fd-sub st2))

;; For user-facing diagnostic output: rename a type's free tvars to
;; nice sequential names (a, b, c, …), flatten curried arrows back to
;; the n-ary surface form, and wrap wide types across lines.  The pure
;; layout lives in types.rkt (`format-type` / `format-pred`); this just
;; hands the type over.  Without it, error messages show internal fresh
;; names like `a12` and deeply nested binary arrows.
(define (pretty-type t) (format-type t))
(define (pretty-pred p) (format-pred p))

;; Indent every line after the first by `n` spaces, so a multi-line
;; pretty-printed type lines up under its label column.
(define (indent-continuation s n)
  (define lines (string-split s "\n" #:trim? #f))
  (cond
    [(or (null? lines) (null? (cdr lines))) s]
    [else
     (define pad (make-string n #\space))
     (string-join
      (cons (car lines)
            (map (lambda (l) (string-append pad l)) (cdr lines)))
      "\n")]))

;; -------- expected/got block helper --------------------------------

;; The aligned two-line block shared by every "wrong type" diagnostic:
;;
;;     expected: <type>
;;     got:      <type>
;;
;; The value column starts at 12 (`"  expected: "` / `"  got:      "`),
;; so continuation lines of a wrapped type are indented to match.
(define (expected/got-lines expected-str got-str)
  (format "  expected: ~a\n  got:      ~a"
          (indent-continuation expected-str 12)
          (indent-continuation got-str 12)))

;; Same block for two types, renamed with ONE shared substitution so a
;; variable common to both reads as the same letter on each side.
(define (expected/got-block expected got)
  (match-define (list e g) (format-types (list expected got)))
  (expected/got-lines e g))

;; -------- type-mismatch error helper -------------------------------

;; Build and raise a structured type-mismatch error.  Output looks like:
;;
;;   infer: type mismatch
;;     expected: Integer
;;     got:      String
;;
;; `blame-stx` should be the most specific syntax object whose source
;; location is the cause — e.g. the offending argument's stx, not the
;; whole application form.
(define (raise-type-mismatch! blame-stx expected got)
  (raise-syntax-error 'infer
    (format "type mismatch\n~a" (expected/got-block expected got))
    blame-stx))

;; The trailing slot of every e:* / p:* / ty:* AST struct is the
;; originating syntax object.  This helper pulls it back out for use
;; as the blame target on a type-mismatch report.
(define (expr-stx node)
  (match node
    [(e:literal _ s)    s]
    [(e:var _ s)        s]
    [(e:lam _ _ s)      s]
    [(e:app _ _ s)      s]
    [(e:let _ _ s)      s]
    [(e:letrec _ _ s)   s]
    [(e:if _ _ _ s)     s]
    [(e:ann _ _ s)      s]
    [(e:match _ _ _ s)  s]
    [(e:match* _ _ _ s) s]
    [(e:escape _ _ _ s) s]))

;; -------- "did you mean?" suggestions ------------------------------

;; Standard iterative Levenshtein distance.
(define (edit-distance s1 s2)
  (define m (string-length s1))
  (define n (string-length s2))
  (define prev (make-vector (add1 n)))
  (define curr (make-vector (add1 n)))
  (for ([j (in-range (add1 n))]) (vector-set! prev j j))
  (for ([i (in-range 1 (add1 m))])
    (vector-set! curr 0 i)
    (for ([j (in-range 1 (add1 n))])
      (define cost
        (if (char=? (string-ref s1 (sub1 i))
                    (string-ref s2 (sub1 j)))
            0 1))
      (vector-set! curr j
                   (min (add1 (vector-ref prev j))
                        (add1 (vector-ref curr (sub1 j)))
                        (+ cost (vector-ref prev (sub1 j))))))
    (for ([k (in-range (add1 n))])
      (vector-set! prev k (vector-ref curr k))))
  (vector-ref prev n))

;; Search env for an identifier whose name is within edit distance ≤ 2
;; of `wanted`.  Return a parenthesised suggestion string ("" if none).
;; `flavour` selects which namespaces to scan: by default we look at
;; value and data-ctor names; `'class` consults env-classes; `'type`
;; consults tcons.
(define (suggest-similar wanted env [flavour 'value])
  (define wanted-str (symbol->string wanted))
  (define candidates
    (case flavour
      [(value) (append (hash-keys (env-vars env))
                       (hash-keys (env-data-ctors env)))]
      [(class) (hash-keys (env-classes env))]
      [(type)  (hash-keys (env-tcons env))]
      [else    '()]))
  (define best
    (for/fold ([acc #f]) ([cand (in-list candidates)])
      (define d (edit-distance wanted-str (symbol->string cand)))
      (cond
        [(> d 2) acc]
        [(or (not acc) (< d (cdr acc))) (cons cand d)]
        [else acc])))
  (cond
    [best (format " (did you mean `~s`?)" (car best))]
    [else ""]))

(define (nice-tvar-names n avoid)
  (define (letter-name i)
    (cond
      [(< i 26)
       (string->symbol
        (string (integer->char (+ (char->integer #\a) i))))]
      [else
       (string->symbol
        (format "~a~a"
                (integer->char (+ (char->integer #\a) (modulo i 26)))
                (quotient i 26)))]))
  (let loop ([taken 0] [i 0] [acc '()])
    (cond
      [(>= taken n) (reverse acc)]
      [else
       (define name (letter-name i))
       (cond
         [(set-member? avoid name) (loop taken (add1 i) acc)]
         [else (loop (add1 taken) (add1 i) (cons name acc))])])))

;; ----- type expression → type ---------------------------------------

;; Resolve a parsed type AST to a core type or qualified type.
;; `(All ...)` wrappers are stripped here; the explicit quantifier is
;; preserved only by `resolve-scheme`.  References to type aliases are
;; expanded inline by substituting the alias's parameters with the
;; supplied arguments and recursing on the alias target.
;; The read-only context the type-resolution cluster threads through the
;; Environment monad: the alias table and the set of aliases currently being
;; expanded (the recursion guard).  Replaces the current-aliases /
;; current-expanding parameters with explicit, monad-threaded context —
;; `asks` reads the alias table, `local` extends the guard for a sub-resolve.
(struct resolve-ctx (aliases expanding) #:transparent)

;; resolve-type/m : ty-ast -> Ctx type
(define (resolve-type/m ty-ast)
  (match ty-ast
    [(ty:var n _) (ctx-return (tvar n))]
    [(ty:con n stx)
     (let/ctx ([aliases (asks resolve-ctx-aliases)])
       (cond
         [(hash-ref aliases n #f) => (lambda (info) (expand-alias/m n info '() stx))]
         [else (ctx-return (tcon n))]))]
    [(ty:app (and h (ty:con n _)) args stx)
     (let/ctx ([aliases (asks resolve-ctx-aliases)])
       (cond
         [(hash-ref aliases n #f) => (lambda (info) (expand-alias/m n info args stx))]
         [else
          (let/ctx ([rargs (ctx-sequence (map resolve-type/m args))])
            (ctx-return (make-tapp (tcon n) rargs)))]))]
    [(ty:app h args _)
     (let/ctx ([rh    (resolve-type/m h)]
               [rargs (ctx-sequence (map resolve-type/m args))])
       (ctx-return (make-tapp rh rargs)))]
    [(ty:forall vs body _)
     ;; Preserve nested `(All ...)` quantifiers as `tforall` so the type can
     ;; carry embedded polymorphism into argument positions for rank-N.
     ;; Top-level schemes come through `resolve-scheme` instead.
     (let/ctx ([rb (resolve-type/m body)])
       (ctx-return (tforall vs rb)))]
    [(ty:qual cs body _)
     (let/ctx ([rcs (ctx-sequence (map resolve-constraint/m cs))]
               [rb  (resolve-type/m body)])
       (ctx-return (mqual rcs rb)))]))

;; expand-alias/m : name info args stx -> Ctx type
(define (expand-alias/m name info args stx)
  (let/ctx ([expanding (asks resolve-ctx-expanding)])
    (expand-alias-checked name info args stx expanding)))

;; Plain checks (raise on recursion / arity mismatch), then recurse with the
;; expansion guard extended via `local`.  Returns a Ctx computation.
(define (expand-alias-checked name info args stx expanding)
  (when (set-member? expanding name)
    (raise-syntax-error 'infer
      (format "recursive type alias: ~s" name) stx))
  (define params (car info))
  (define target (cdr info))
  (unless (= (length params) (length args))
    (raise-syntax-error 'infer
      (format "type alias ~s expects ~a arg(s), got ~a"
              name (length params) (length args))
      stx))
  (define sub (for/hasheq ([p (in-list params)] [a (in-list args)])
                (values p a)))
  (local (lambda (c) (struct-copy resolve-ctx c
                                  [expanding (set-add expanding name)]))
         (resolve-type/m (substitute-tyvars sub target))))

;; Walk a surface type AST and substitute ty:var occurrences whose name
;; appears in `sub` with the corresponding replacement AST.
(define (substitute-tyvars sub ty-ast)
  (match ty-ast
    [(ty:var n _)
     (hash-ref sub n ty-ast)]
    [(ty:con _ _) ty-ast]
    [(ty:app h args stx)
     (ty:app (substitute-tyvars sub h)
             (for/list ([a (in-list args)]) (substitute-tyvars sub a))
             stx)]
    [(ty:forall vs body stx)
     (define sub*
       (for/fold ([s sub]) ([v (in-list vs)]) (hash-remove s v)))
     (ty:forall vs (substitute-tyvars sub* body) stx)]
    [(ty:qual cs body stx)
     (ty:qual (for/list ([c (in-list cs)]) (substitute-constraint-tyvars sub c))
              (substitute-tyvars sub body)
              stx)]))

(define (substitute-constraint-tyvars sub c)
  (match c
    [(constraint cls args stx)
     (constraint cls
                 (for/list ([a (in-list args)]) (substitute-tyvars sub a))
                 stx)]))

;; resolve-constraint/m : constraint -> Ctx pred
(define (resolve-constraint/m c)
  (match c
    [(constraint class args _)
     (let/ctx ([rargs (ctx-sequence (map resolve-type/m args))])
       (ctx-return (pred class rargs)))]))

;; Resolve a parsed type AST as a scheme (for declarations).  A bare
;; `(All ...)` wraps the explicit quantifier; otherwise we generalize
;; over every type variable that appears.
;;   resolve-scheme/m : ty-ast -> Ctx scheme
(define (resolve-scheme/m ty-ast)
  (match ty-ast
    [(ty:forall vs body _)
     (let/ctx ([rb (resolve-type/m body)])
       (ctx-return (scheme vs rb)))]
    [_
     (let/ctx ([t (resolve-type/m ty-ast)])
       (ctx-return
        (let ([vs (sort (set->list (type-vars t)) symbol<?)])
          (scheme vs t))))]))

;; ----- runner wrappers: the Environment-monad boundary --------------
;; Build the resolution context from `env` and run.  As the form-handlers
;; that call these are themselves converted to the monad, these run-points
;; become ordinary `let/ctx` binds and the wrappers dissolve.
(define (resolve-type ty-ast env)
  (run-ctx (resolve-type/m ty-ast) (resolve-ctx (env-aliases env) (seteq))))
(define (resolve-constraint c env)
  (run-ctx (resolve-constraint/m c) (resolve-ctx (env-aliases env) (seteq))))
(define (resolve-scheme ty-ast env)
  (run-ctx (resolve-scheme/m ty-ast) (resolve-ctx (env-aliases env) (seteq))))

;; ----- literals -----------------------------------------------------

(define (literal-type v)
  (cond
    [(exact-integer? v) t-int]
    ;; Exact non-integer rationals (e.g. 3/4) are Rational literals.  The
    ;; predicate mirrors dict.rkt's dispatch-tag so the static type and the
    ;; runtime tag agree.
    [(and (rational? v) (exact? v) (not (exact-integer? v))) t-rational]
    [(inexact-real? v)  t-float]
    ;; Non-real numbers (a nonzero imaginary part).  An exact complex —
    ;; both parts exact, e.g. 3+4i — is the exact-complex type; anything
    ;; else with an imaginary part (3.0+4.0i) is the inexact Complex.
    ;; Exactness splits them, mirroring dict.rkt's dispatch-tag.
    [(and (number? v) (not (real? v)) (exact? v)) t-complex-exact]
    [(and (number? v) (not (real? v)))            t-complex]
    [(boolean? v)       t-bool]
    [(string? v)        t-string]
    [(char? v)          t-char]
    [(bytes? v)         t-bytes]
    [(symbol? v)        t-symbol]
    [else (error 'literal-type "unsupported literal: ~e" v)]))

;; ----- core inference ----------------------------------------------

(define (infer-expr/fresh e [env initial-env])
  (define-values (s t _st) (infer-expr e env (make-infer-state)))
  (values s t))

;; infer-expr dispatches on the expression form; each non-trivial arm is
;; its own `infer-<form>` helper below, mutually recursive with this
;; dispatcher.  Keeping the dispatch flat and the arms named makes each
;; form's inference rule readable (and testable) in isolation.
;;
;; The dispatcher is the Infer-monad `infer-expr/m`, returning
;; `Infer (subst . type)`; every arm is its own `infer-<form>/m`.  The fresh
;; counter and pending-pred bag live in the threaded `st` (no boxes).
;; `infer-expr` is the driver-facing bridge that runs it on a given st.
(define (infer-expr/m e env)
  (match e
    [(e:literal v _)                 (infer-return (cons empty-subst (literal-type v)))]
    [(e:var x stx)                   (infer-var/m x stx env)]
    [(e:lam params body _)           (infer-lam/m params body env)]
    [(e:app head args stx)           (infer-app/m head args stx env)]
    [(e:let bindings body _)         (infer-let/m bindings body env)]
    [(e:letrec bindings body _)      (infer-letrec/m bindings body env)]
    [(e:if c t els stx)              (infer-if/m c t els stx env)]
    [(e:ann expr ty-ast stx)         (infer-ann/m expr ty-ast stx env)]
    [(e:escape ty-ast vars _ stx)    (infer-escape/m ty-ast vars stx env)]
    [(e:update record updates stx)   (infer-update/m record updates stx env)]
    [(e:handle expr clauses ret stx) (infer-handle/m expr clauses ret stx env)]
    [(e:match scrut clauses irrefutable? stx)
     (infer-match/m scrut clauses irrefutable? stx env)]
    [(e:match* scrutinees clauses _irrefutable? stx)
     (infer-match*/m scrutinees clauses stx env)]))

;; Driver↔monad bridge: runs the monadic dispatcher on the threaded state and
;; returns (values subst type st').  The current-* config parameters that the
;; driver wraps around its calls stay in scope because run-infer is synchronous.
(define (infer-expr e env st)
  (define-values (r st′) (run-infer (infer-expr/m e env) #f st))
  (values (car r) (cdr r) st′))

;; ----- per-form inference (the arms of infer-expr) ------------------

;; No recursive infer-expr and no direct fresh/preds: a straight conversion
;; that wraps each (values empty-subst type) result in infer-return.  The
;; side-effecting calls (instantiate/subst, record-*) stay direct — during
;; the transition they act on the boxes/tables, monadified at the flip.
(define (infer-var/m x stx env)
     (define sch
       (or (env-ref-var env x)
           (let ([info (env-ref-data env x)])
             (and info (data-info-scheme info)))))
     (cond
       [sch
        (define owner-class (env-ref-method-class env x))
        (define cinfo (and owner-class (env-ref-class env owner-class)))
        (cond
          [cinfo
           (let/infer ([r (instantiate/subst/m sch)])
            (let* ([t (car r)] [sub (cdr r)]
                   ;; `x` is genuinely this class's method only if its scheme
                   ;; quantifies over the class parameters — instantiating it
                   ;; must bind each of them in `sub`.  A LOCAL binding that
                   ;; merely shares a method's name (e.g. an effect operation
                   ;; named `peek`, colliding with the prelude Storable class's
                   ;; `peek`) has its own scheme, so some class param is absent
                   ;; from `sub`.  Such an `x` is an ordinary variable that
                   ;; shadows the method, not the method itself — it must not
                   ;; drive dispatch.
                   [class-param-tvars
                    (for/list ([p (in-list (class-info-params cinfo))])
                      (hash-ref sub p #f))])
              (cond
                [(memv #f class-param-tvars)
                 ;; Shadowed method name — treat as an ordinary variable.
                 (infer-return (cons empty-subst t))]
                [else
                 (define dispatchpos (hash-ref (class-info-dispatchpos cinfo) x #f))
                 (define reqs (hash-ref (class-info-dictreqs cinfo) x '()))
                 (let/infer
                  ([_ (cond
                        [(eq? dispatchpos 'return)
                         ;; A return-typed method may ALSO carry a method-level
                         ;; dict requirement (e.g. MonadTrans.lift's `(Monad m)
                         ;; =>`).  Fold those dict-entries into the single
                         ;; 'return entry so the separate dict record below
                         ;; doesn't clobber the dispatch resolution on the same
                         ;; stx.
                         (define method-dict-entries
                           (for/list ([req (in-list reqs)])
                             (cons (car req)
                                   (for/list ([a (in-list (cdr req))]) (apply-subst sub a)))))
                         (m:record-method-use stx x class-param-tvars method-dict-entries)]
                        [(integer? dispatchpos)
                         ;; Positional class-method call.  The resolver routes
                         ;; to a per-instance impl after the dispatch arg's type
                         ;; settles.  Recording fires for ALL positional method
                         ;; calls so any concrete-type call site can be
                         ;; monomorphized (direct impl call instead of dispatch).
                         (begin/infer
                           (m:record-inst-dispatch-use stx x class-param-tvars)
                           (if (null? reqs) (infer-return (void))
                               (m:record-dict-use stx x reqs sub)))]
                        [else
                         (if (null? reqs) (infer-return (void))
                             (m:record-dict-use stx x reqs sub))])])
                  (infer-return (cons empty-subst t)))])))]
          [else
           ;; A free function may itself be needs-dict: if its scheme's
           ;; qual context includes a constraint over a class with
           ;; return-typed methods (e.g. `mconcat :: (Monoid a) => …`),
           ;; the elaborator inserts the resolved impls at the call
           ;; site, mirroring the path for class methods.
           (define free-reqs (var-dict-requirements env sch))
           (cond
             [(null? free-reqs)
              (let/infer ([t (instantiate/m sch)])
                (infer-return (cons empty-subst t)))]
             [else
              (let/infer ([r (instantiate/subst/m sch)])
                (let* ([t (car r)] [sub (cdr r)])
                  (let/infer ([_ (m:record-dict-use stx x free-reqs sub)])
                    (infer-return (cons empty-subst t)))))])])]
       [else
        (raise-syntax-error 'infer
                            (format "unbound identifier: ~s~a"
                                    x (suggest-similar x env))
                            stx)]))

(define (infer-lam/m params body env)
  (let/infer ([param-tvars (infer-sequence (for/list ([p (in-list params)]) (m:fresh-tvar)))])
    (let ([env* (for/fold ([e env]) ([p (in-list params)] [t (in-list param-tvars)])
                  (env-extend-var e p (scheme '() t)))])
      (let/infer ([rb (infer-expr/m body env*)])
        (let* ([s (car rb)] [body-type (cdr rb)])
          (infer-return
           (cons s (foldr make-arrow body-type
                          (for/list ([t (in-list param-tvars)]) (apply-subst s t))))))))))

;; Sequentially walk args.  After inferring each arg's type, unify the
;; current head-type's domain with the arg's type; on failure, blame the
;; SPECIFIC arg.  If the head's arrow domain is a `tforall`, switch to
;; bidirectional check-mode (rank-N) — the arg is checked against the
;; polymorphic type, not inferred and unified, via `check-expr/m`.
(define (infer-app/m head args stx env)
  (let/infer ([rh (infer-expr/m head env)])
    (let* ([s-head (car rh)] [t-head (cdr rh)])
      (let loop ([args args] [s s-head] [head-ty t-head] [env (apply-subst/env s-head env)])
        (cond
          [(null? args) (infer-return (cons s head-ty))]
          [else
           (define this-arg (car args))
           (define head-ty-pre (apply-subst s head-ty))
           (cond
             [(and (arrow? head-ty-pre) (tforall? (arrow-dom head-ty-pre)))
              (define dom (arrow-dom head-ty-pre))
              (define cod (arrow-cod head-ty-pre))
              (let/infer ([rc (check-expr/m this-arg env dom)])
                (let* ([s-arg (car rc)] [s-now (subst-compose s-arg s)])
                  (loop (cdr args) s-now (apply-subst s-now cod) (apply-subst/env s-arg env))))]
             [else
              (let/infer ([ra (infer-expr/m this-arg env)])
                (let* ([s-arg (car ra)] [t-arg (cdr ra)]
                       [s-now (subst-compose s-arg s)]
                       [head-ty-now (apply-subst s-now head-ty)])
                  (let/infer ([β (m:fresh-tvar)])
                    (let* ([expected-arrow (make-arrow t-arg β)]
                           [s-u (with-handlers
                                 ([exn:fail:unify?
                                   (lambda (_)
                                     (cond
                                       [(arrow? head-ty-now)
                                        (raise-type-mismatch! (expr-stx this-arg)
                                          (apply-subst s-now (arrow-dom head-ty-now))
                                          (apply-subst s-now t-arg))]
                                       [else
                                        (raise-type-mismatch! (expr-stx head)
                                          expected-arrow head-ty-now)]))])
                                 (unify head-ty-now expected-arrow))])
                      (loop (cdr args)
                            (subst-compose s-u s-now)
                            (apply-subst s-u β)
                            (apply-subst/env s-arg env))))))])])))))

;; Parallel let: each rhs is typed in the env at let-entry (with
;; substitutions threaded), generalized, then made available.  The
;; binding-threading for/fold becomes a monadic named-let.
(define (infer-let/m bindings body env)
  (let/infer ([acc (let loop ([bs bindings] [s empty-subst] [env-after env])
                     (cond
                       [(null? bs) (infer-return (cons s env-after))]
                       [else
                        (define b (car bs))
                        (let/infer ([r (infer-expr/m (cdr b) (apply-subst/env s env))])
                          (let* ([s′ (car r)] [t (cdr r)]
                                 [s-combined (subst-compose s′ s)])
                            (let/infer ([_ (m:apply-subst-to-preds s-combined)]
                                        [sch (generalize/m (apply-subst/env s-combined env)
                                                           (apply-subst s-combined t))])
                              (let* ([env-after* (env-extend-var
                                                  (apply-subst/env s-combined env-after) (car b) sch)])
                                (loop (cdr bs) s-combined env-after*)))))]))])
    (let* ([s-acc (car acc)] [env-after (cdr acc)])
      (let/infer ([rb (infer-expr/m body env-after)])
        (infer-return (cons (subst-compose (car rb) s-acc) (cdr rb)))))))

;; Mutual recursion: pre-bind each name with a fresh monomorphic tvar so each
;; rhs can reference every other binding (and itself).  After inferring all
;; rhs's, unify each tvar with the inferred type and generalize against the
;; OUTER env's free-var set.
(define (infer-letrec/m bindings body env)
  (let/infer ([pre-tvars (infer-sequence (for/list ([b (in-list bindings)]) (m:fresh-tvar)))])
    (let* ([pre-bindings (map (lambda (b t) (cons (car b) t)) bindings pre-tvars)]
           [env-with-pre (for/fold ([e env]) ([pb (in-list pre-bindings)])
                           (env-extend-var e (car pb) (scheme '() (cdr pb))))])
      (let/infer ([acc (let loop ([bs bindings] [pbs pre-bindings] [s empty-subst] [ts '()])
                         (cond
                           [(null? bs) (infer-return (cons s ts))]
                           [else
                            (let/infer ([r (infer-expr/m (cdr (car bs))
                                                         (apply-subst/env s env-with-pre))])
                              (let* ([s′ (car r)] [t (cdr r)]
                                     [s-combined (subst-compose s′ s)]
                                     [s-u (unify (apply-subst s-combined (cdr (car pbs)))
                                                 (apply-subst s-combined t))]
                                     [s-after (subst-compose s-u s-combined)])
                                (loop (cdr bs) (cdr pbs) s-after
                                      (cons (apply-subst s-after (cdr (car pbs))) ts))))]))])
        (let* ([s-final (car acc)] [ts (cdr acc)])
          (let/infer ([_ (m:apply-subst-to-preds s-final)]
                      [env-after (let loop ([bs bindings] [tts (reverse ts)]
                                            [e (apply-subst/env s-final env)])
                                   (cond
                                     [(null? bs) (infer-return e)]
                                     [else
                                      (let/infer ([sch (generalize/m (apply-subst/env s-final env)
                                                                     (car tts))])
                                        (loop (cdr bs) (cdr tts)
                                              (env-extend-var e (car (car bs)) sch)))]))])
            (let/infer ([rb (infer-expr/m body env-after)])
              (infer-return (cons (subst-compose (car rb) s-final) (cdr rb))))))))))

;; Infer-monad arm.  Pattern for the conversion: `let/infer` binds each
;; recursive `infer-expr/m` result (a `subst . type` pair); a `let*` does the
;; pure work between binds (unify, subst-compose) and ends in the next monadic
;; computation.
(define (infer-if/m c t e stx env)
  (let/infer ([rc (infer-expr/m c env)])
    (let* ([s-c (car rc)] [t-c (cdr rc)]
           [s-cb
            (with-handlers
             ([exn:fail:unify?
               (lambda (_)
                 (raise-type-mismatch! (expr-stx c) t-bool (apply-subst s-c t-c)))])
             (unify (apply-subst s-c t-c) t-bool))]
           [s1 (subst-compose s-cb s-c)])
      (let/infer ([rt (infer-expr/m t (apply-subst/env s1 env))])
        (let* ([s-then (car rt)] [t-then (cdr rt)]
               [s2 (subst-compose s-then s1)])
          (let/infer ([re (infer-expr/m e (apply-subst/env s2 env))])
            (let* ([s-else (car re)] [t-else (cdr re)]
                   [s3 (subst-compose s-else s2)]
                   [s-branches
                    (with-handlers
                     ([exn:fail:unify?
                       (lambda (_)
                         (raise-type-mismatch! (expr-stx e)
                           (apply-subst s3 t-then) (apply-subst s3 t-else)))])
                     (unify (apply-subst s3 t-then) (apply-subst s3 t-else)))]
                   [s-final (subst-compose s-branches s3)])
              (infer-return (cons s-final (apply-subst s-final t-then))))))))))

(define (infer-ann/m expr ty-ast stx env)
  (let/infer ([re (infer-expr/m expr env)])
    (let* ([s-e (car re)] [t-e (cdr re)]
           [declared (qual-body-type (resolve-type ty-ast env))]
           [s-u (with-handlers
                 ([exn:fail:unify?
                   (lambda (_)
                     (raise-type-mismatch! (expr-stx expr) declared (apply-subst s-e t-e)))])
                 (unify (apply-subst s-e t-e) declared))])
      (infer-return (cons (subst-compose s-u s-e) (apply-subst s-u declared))))))

(define (infer-escape/m ty-ast vars stx env)
  (define expected (qual-body-type (resolve-type ty-ast env)))
  (for ([v (in-list vars)])
    (unless (env-ref-var env v)
      (raise-syntax-error 'infer
        (format "(racket …) escape references unbound name: ~s" v)
        stx)))
  (infer-return (cons empty-subst expected)))

;; current-expected-type is read/reset directly (still a parameter during the
;; transition); infer-clause is not yet converted, so it is bridged.
(define (infer-match/m scrut clauses irrefutable? stx env)
  (let/infer ([rs (infer-expr/m scrut env)])
    (let ()
      (define s-scrut (car rs))
      (define t-scrut (cdr rs))
      ;; Seed result-tv with the surrounding expected type (if any) so per-arm
      ;; GADT skolem refinement resolves each body against the concrete type.
      (define expected (current-expected-type))
      (current-expected-type #f)
      (let/infer ([result-tv (if expected (infer-return expected) (m:fresh-tvar))])
        (let/infer ([s-final
                     (let loop ([clauses clauses] [i 0] [s s-scrut])
                       (cond
                         [(null? clauses) (infer-return s)]
                         [else
                          (let/infer ([rc (infer-clause/m (car clauses)
                                                          (apply-subst s t-scrut)
                                                          (apply-subst s result-tv)
                                                          (apply-subst/env s env)
                                                          (> i 0))])
                            (loop (cdr clauses) (add1 i) (subst-compose (car rc) s)))]))])
          ;; Irrefutable destructure (a pattern let/where) skips exhaustiveness.
          (let/infer ([_ (if irrefutable?
                             (infer-return (void))
                             (check-exhaustive!/m (apply-subst s-final t-scrut) clauses stx env))])
            (infer-return (cons s-final (apply-subst s-final result-tv)))))))))

(define (infer-match*/m scrutinees clauses stx env)
     ;; Multi-scrutinee match (emitted by the multi-clause `define`
     ;; combiner in private/surface.rkt).  Infer each scrutinee's
     ;; type, then for each clause walk its pattern list against the
     ;; scrutinee types, accumulate bindings into a single env
     ;; extension, and infer the body.  Result type unifies across
     ;; all clauses.  Exhaustiveness over the cartesian product of
     ;; scrutinee types is not (yet) checked — the multi-clause
     ;; combiner trusts the user to cover the cases they care about,
     ;; matching Haskell's behaviour.
     (let/infer ([scrut-acc
                  (let loop ([scs scrutinees] [s empty-subst] [acc '()])
                    (cond
                      [(null? scs) (infer-return (cons s acc))]
                      [else
                       (let/infer ([ri (infer-expr/m (car scs) (apply-subst/env s env))])
                         (loop (cdr scs) (subst-compose (car ri) s) (cons (cdr ri) acc)))]))])
       (let ()
         (define s-scruts (car scrut-acc))
         (define scrut-types (reverse (cdr scrut-acc)))
         (define expected (current-expected-type))
         (current-expected-type #f)
         (let/infer ([result-tv (if expected (infer-return expected) (m:fresh-tvar))])
           (let/infer ([s-final
                        (let loop ([clauses clauses] [i 0] [s s-scruts])
                          (cond
                            [(null? clauses) (infer-return s)]
                            [else
                             (let/infer ([rc (infer-clause*/m (car clauses)
                                                              (map (lambda (t) (apply-subst s t)) scrut-types)
                                                              (apply-subst s result-tv)
                                                              (apply-subst/env s env)
                                                              (> i 0))])
                               (loop (cdr clauses) (add1 i) (subst-compose (car rc) s)))]))])
             (infer-return (cons s-final (apply-subst s-final result-tv))))))))

;; Type a single match clause.  Pattern bindings extend the env for the
;; clause body.  The body type is unified with the running result type
;; so every arm yields the same type.
;; Infer a functional record update.  The record's type
;; must be a known struct type (or applied struct type); each named
;; field must exist; each value-expression must match the field's
;; declared type after substituting the record's tparam args.
(define (infer-update/m record updates stx env)
  (let/infer ([rr (infer-expr/m record env)])
    (let ()  ; plain setup, then the monadic update-walk
      (define s-rec (car rr))
      (define t-rec (cdr rr))
      (define t-rec*    (apply-subst s-rec t-rec))
      (define type-head (update-type-head t-rec*))
      (define type-args (update-type-args t-rec*))
      (unless type-head
        (raise-syntax-error 'infer
          (format "update target must have a concrete record type, got ~a"
                  (pretty-type t-rec*))
          stx))
      (define field-names (env-ref-struct-fields env type-head))
      (unless field-names
        (raise-syntax-error 'infer
          (format "update target type ~s is not a record" type-head)
          stx))
      ;; The data ctor's scheme tells us each field's type in terms of the
      ;; struct's tparams; instantiate with the args observed on `record`.
      (define di (env-ref-data env type-head))
      (define-values (field-types _result-type)
        (instantiate-struct-fields di type-args))
      (let/infer ([s-acc
                   (let loop ([updates updates] [s s-rec])
                     (cond
                       [(null? updates) (infer-return s)]
                       [else
                        (define upd (car updates))
                        (define field-name (car upd))
                        (define value-expr (cdr upd))
                        (define idx
                          (or (index-of field-names field-name)
                              (raise-syntax-error 'infer
                                (format "record type ~s has no field named ~s; available: ~s"
                                        type-head field-name field-names)
                                stx)))
                        (define expected (list-ref field-types idx))
                        (let/infer ([rv (infer-expr/m value-expr (apply-subst/env s env))])
                          (let* ([s-v (car rv)] [t-v (cdr rv)]
                                 [s-now (subst-compose s-v s)]
                                 [s-u (with-handlers
                                       ([exn:fail:unify?
                                         (lambda (_)
                                           (raise-type-mismatch! (expr-stx value-expr)
                                             (apply-subst s-now expected)
                                             (apply-subst s-now t-v)))])
                                       (unify (apply-subst s-now t-v)
                                              (apply-subst s-now expected)))])
                            (loop (cdr updates) (subst-compose s-u s-now))))]))])
        (infer-return (cons s-acc (apply-subst s-acc t-rec*)))))))

;; Extract the head tcon name from a record type like
;; `(tcon Point)` or `(tapp (tcon Box) [args])`.
(define (update-type-head t)
  (match t
    [(tcon n) n]
    [(tapp (tcon n) _) n]
    [_ #f]))

(define (update-type-args t)
  (match t
    [(tapp _ args) args]
    [_ '()]))

;; Instantiate a struct's data-info: substitute its scheme's
;; quantified vars with the type args observed at the use site.
;; Returns (values field-types result-type).
(define (instantiate-struct-fields di type-args)
  (define sch (data-info-scheme di))
  (match sch
    [(scheme vs body)
     (define qual-body (qual-body-deep body))
     (define s
       (for/fold ([s empty-subst])
                 ([v (in-list vs)] [a (in-list type-args)])
         (subst-extend s v a)))
     (define applied (apply-subst s qual-body))
     (let loop ([t applied] [acc '()])
       (cond
         [(arrow? t) (loop (arrow-cod t) (cons (arrow-dom t) acc))]
         [else (values (reverse acc) t)]))]))

;; Infer a (handle EXPR clauses... return) expression.
;; The return clause's body type becomes the overall type of the
;; handle.  Each operation clause's body must match that type.
;; In an op clause, `k` is the resumption — typed as `(-> op-result
;; handle-type)`.
(define (infer-handle/m expr clauses ret stx env)
  ;; Infer the body's type first; it must equal the type taken by the return
  ;; clause's bound variable, since the body's normal return value flows into
  ;; the return clause.
  (let/infer ([re (infer-expr/m expr env)])
    (let* ([s-expr (car re)] [t-expr (cdr re)])
      (let/infer ([result-tv (m:fresh-tvar)])
        (let ([env-ret (env-extend-var (apply-subst/env s-expr env)
                                       (handle-return-var ret)
                                       (scheme '() (apply-subst s-expr t-expr)))])
          (let/infer ([rr (infer-expr/m (handle-return-body ret) env-ret)])
            (let* ([s-ret (car rr)] [t-ret (cdr rr)]
                   [s-after-ret (subst-compose s-ret s-expr)]
                   [s-u-ret
                    (with-handlers
                     ([exn:fail:unify?
                       (lambda (_)
                         (raise-syntax-error 'infer
                           (format "handle return clause has type ~a; expected handle's result type"
                                   (pretty-type (apply-subst s-after-ret t-ret)))
                           (handle-return-stx ret)))])
                     (unify (apply-subst s-after-ret t-ret) (apply-subst s-after-ret result-tv)))]
                   [s-acc (subst-compose s-u-ret s-after-ret)])
              ;; Each op clause: look up the op's type, bind params + k, infer
              ;; the body, unify its type with result-tv.
              (let/infer ([s-final
                           (let loop ([clauses clauses] [s s-acc])
                             (cond
                               [(null? clauses) (infer-return s)]
                               [else
                                (define cl (car clauses))
                                (define op-name (handle-clause-op cl))
                                (define eff (env-effect-of-op env op-name))
                                (unless eff
                                  (raise-syntax-error 'infer
                                    (format "handle clause references unknown effect operation: ~s" op-name)
                                    (handle-clause-stx cl)))
                                (define op-sch (env-ref-var env op-name))
                                (let/infer ([op-type (instantiate/m op-sch)])
                                (let ()
                                (define-values (raw-arg-types op-result) (split-arrows op-type))
                                ;; A 0-arg op was internally promoted to (-> Unit T);
                                ;; peel the Unit when the clause has no params.
                                (define arg-types
                                  (cond
                                    [(and (null? (handle-clause-params cl))
                                          (= (length raw-arg-types) 1)
                                          (equal? (car raw-arg-types) (tcon 'Unit)))
                                     '()]
                                    [else raw-arg-types]))
                                (unless (= (length arg-types) (length (handle-clause-params cl)))
                                  (raise-syntax-error 'infer
                                    (format "handle clause for ~s expects ~a parameters, got ~a"
                                            op-name (length arg-types) (length (handle-clause-params cl)))
                                    (handle-clause-stx cl)))
                                (define k-type (make-arrow op-result (apply-subst s result-tv)))
                                (define env-cl
                                  (env-extend-var
                                   (for/fold ([e (apply-subst/env s env)])
                                             ([p (in-list (handle-clause-params cl))]
                                              [ty (in-list arg-types)])
                                     (env-extend-var e p (scheme '() ty)))
                                   (handle-clause-k-name cl)
                                   (scheme '() k-type)))
                                (let/infer ([rb (infer-expr/m (handle-clause-body cl) env-cl)])
                                  (let* ([s-body (car rb)] [t-body (cdr rb)]
                                         [s-now (subst-compose s-body s)]
                                         [s-u (with-handlers
                                               ([exn:fail:unify?
                                                 (lambda (_)
                                                   (raise-syntax-error 'infer
                                                     (format "handle clause for ~s has body type ~a; expected ~a"
                                                             op-name
                                                             (pretty-type (apply-subst s-now t-body))
                                                             (pretty-type (apply-subst s-now result-tv)))
                                                     (handle-clause-stx cl)))])
                                               (unify (apply-subst s-now t-body)
                                                      (apply-subst s-now result-tv)))])
                                    (loop (cdr clauses) (subst-compose s-u s-now))))))]))])
                (infer-return (cons s-final (apply-subst s-final result-tv)))))))))))

;; Split an arrow chain into (list-of-arg-types, final-codomain).
(define (split-arrows t)
  (let loop ([t t] [acc '()])
    (cond
      [(arrow? t) (loop (arrow-cod t) (cons (arrow-dom t) acc))]
      [else (values (reverse acc) t)])))

;; `earlier-arms?` is #t once a previous arm has already constrained
;; the running result type; #f on the first arm, where any mismatch can
;; only be against the result type seeded from the declared signature
;; (a fresh result tvar would unify rather than fail).
;; infer-pattern, the preds-box discharge, and the unify/gadt-unify work stay
;; direct (box/table-backed during the transition); the guard and body
;; recursions go through infer-expr/m.
(define (infer-clause/m cl scrut-type result-type env [earlier-arms? #t])
  (let/infer ([rp (infer-pattern/m (clause-pattern cl) env)])
   (let ()
   (define-values (bindings pat-type ex-hyps)
    (values (car rp) (cadr rp) (caddr rp)))
  ;; Try standard unify first; on a hard mismatch (a
  ;; refinable function-scheme skolem on one side and a concrete
  ;; type on the other), fall back to gadt-unify which returns
  ;; (tvar-subst, skolem-subst).  The skolem-subst applies only
  ;; LOCALLY to this arm's expected result type — never to the
  ;; outer env, which would unsoundly leak refinement to later
  ;; arms or to outer bindings.
  (define-values (s-pat arm-skolem-subst)
    (with-handlers
     ([exn:fail:unify?
       (lambda (e1)
         (with-handlers
          ([exn:fail:unify?
            (lambda (e2)
              (raise-syntax-error 'infer
                (format "pattern type ~a does not match scrutinee type ~a"
                        (pretty-type pat-type) (pretty-type scrut-type))
                (clause-stx cl)))])
          (gadt-unify pat-type scrut-type)))])
     (values (unify pat-type scrut-type) (hash))))
  (define env*
    (for/fold ([e (apply-subst/env s-pat env)])
              ([b (in-list bindings)])
      (env-extend-var e (car b)
                      (scheme '() (apply-subst s-pat (cdr b))))))
  ;; A pattern guard, when present, is typechecked under the pattern bindings
  ;; and must produce a Boolean; thread its substitution into the running
  ;; chain so any tvars it pins are visible to the body and reduction.
  (let/infer ([guard-acc
               (cond
                 [(clause-guard cl)
                  (let/infer ([rg (infer-expr/m (clause-guard cl) env*)])
                    (let* ([s-g (car rg)] [t-g (cdr rg)]
                           [s-u (with-handlers
                                 ([exn:fail:unify?
                                   (lambda (_)
                                     (raise-syntax-error 'infer
                                       (format "pattern guard must be Boolean, got ~a"
                                               (pretty-type (apply-subst s-g t-g)))
                                       (clause-stx cl)))])
                                 (unify (apply-subst s-g t-g) t-bool))]
                           [s* (subst-compose s-u (subst-compose s-g s-pat))])
                      (infer-return (cons s* (apply-subst/env s* env*)))))]
                 [else (infer-return (cons s-pat env*))])])
    (let* ([s-pre-body (car guard-acc)] [env-pre-body (cdr guard-acc)])
      (let/infer ([rb (infer-expr/m (clause-body cl) env-pre-body)])
        (let ()
          (define s-body (car rb))
          (define t-body (cdr rb))
          (define s-acc (subst-compose s-body s-pre-body))
          ;; Discharge pending preds the existential hypotheses prove (the
          ;; pattern is the proof) so they don't bubble to the outer reduce.
          (let/infer ([_ (if (null? ex-hyps)
                             (infer-return (void))
                             (let/infer ([_ (m:apply-subst-to-preds s-acc)]
                                         [current (m:snapshot-preds)])
                               (m:set-preds
                                (reduce-context env
                                                (map (lambda (p) (apply-subst s-acc p)) ex-hyps)
                                                current))))])
           (let ()
          ;; Apply the arm's local skolem refinement before checking body type.
          (define refined-result-type
            (apply-skolem-subst arm-skolem-subst (apply-subst s-acc result-type)))
          (define s-u
            (with-handlers
             ([exn:fail:unify?
               (lambda (_)
                 (match-define (list got exp)
                   (format-types (list (apply-subst s-acc t-body) refined-result-type)))
                 (raise-syntax-error 'infer
                   (format (if earlier-arms?
                               "match clause body has type ~a but earlier arms have ~a"
                               "match clause body has type ~a but the expected result type is ~a")
                           got exp)
                   (clause-stx cl)))])
             (unify (apply-skolem-subst arm-skolem-subst (apply-subst s-acc t-body))
                    refined-result-type)))
          (infer-return (cons (subst-compose s-u s-acc) (apply-subst s-u t-body))))))))))))

;; Multi-pattern variant of `infer-clause` for `e:match*`.  Walks
;; the clause's parameter-pattern list, unifying each with its
;; scrutinee's type and accumulating bindings into one env
;; extension that the body is typed against.  No GADT
;; skolem-refinement (multi-clause defines aren't a GADT-elim site
;; in practice); no existential support; no per-pattern guard
;; (guard is one expression at the clause level if present).
;; `earlier-clauses?` mirrors `infer-clause`'s `earlier-arms?`: #f on
;; the first clause, where a result mismatch can only be against the
;; type seeded from the declared signature.
(define (infer-clause*/m cl* scrut-types result-type env [earlier-clauses? #t])
  (let ()
   (unless (= (length (clause*-patterns cl*)) (length scrut-types))
     (raise-syntax-error 'infer
       (format "match* clause has ~a patterns but ~a scrutinees"
               (length (clause*-patterns cl*)) (length scrut-types))
       (clause*-stx cl*)))
   ;; Pattern walk: thread st through infer-pattern/m via a monadic loop.
   (let/infer ([sp (let loop ([pats (clause*-patterns cl*)]
                              [scrut-ts scrut-types]
                              [s empty-subst] [penv env])
                     (cond
                       [(null? pats) (infer-return (cons s penv))]
                       [else
                        (let/infer ([rp (infer-pattern/m (car pats) (apply-subst/env s penv))])
                          (let* ([bindings (car rp)] [pat-type (cadr rp)]
                                 [s-u (with-handlers
                                       ([exn:fail:unify?
                                         (lambda (_)
                                           (raise-syntax-error 'infer
                                             (format "pattern type ~a does not match scrutinee type ~a"
                                                     (pretty-type pat-type)
                                                     (pretty-type (apply-subst s (car scrut-ts))))
                                             (clause*-stx cl*)))])
                                       (unify pat-type (apply-subst s (car scrut-ts))))]
                                 [s-now (subst-compose s-u s)]
                                 [env-now (for/fold ([e (apply-subst/env s-now penv)]) ([b (in-list bindings)])
                                            (env-extend-var e (car b)
                                                            (scheme '() (apply-subst s-now (cdr b)))))])
                            (loop (cdr pats) (cdr scrut-ts) s-now env-now)))]))])
     (let* ([s-pats (car sp)] [env* (cdr sp)])
   (let/infer ([guard-acc
                (cond
                  [(clause*-guard cl*)
                   (let/infer ([rg (infer-expr/m (clause*-guard cl*) env*)])
                     (let* ([s-g (car rg)] [t-g (cdr rg)]
                            [s-u (with-handlers
                                  ([exn:fail:unify?
                                    (lambda (_)
                                      (raise-syntax-error 'infer
                                        (format "match* clause guard must be Boolean, got ~a"
                                                (pretty-type (apply-subst s-g t-g)))
                                        (clause*-stx cl*)))])
                                  (unify (apply-subst s-g t-g) t-bool))]
                            [s* (subst-compose s-u (subst-compose s-g s-pats))])
                       (infer-return (cons s* (apply-subst/env s* env*)))))]
                  [else (infer-return (cons s-pats env*))])])
     (let* ([s-pre-body (car guard-acc)] [env-pre-body (cdr guard-acc)])
       (let/infer ([rb (infer-expr/m (clause*-body cl*) env-pre-body)])
         (let* ([s-body (car rb)] [t-body (cdr rb)]
                [s-acc (subst-compose s-body s-pre-body)]
                [s-u (with-handlers
                      ([exn:fail:unify?
                        (lambda (_)
                          (match-define (list got exp)
                            (format-types (list (apply-subst s-acc t-body)
                                                (apply-subst s-acc result-type))))
                          (raise-syntax-error 'infer
                            (format (if earlier-clauses?
                                        "match* clause body has type ~a but earlier clauses have ~a"
                                        "match* clause body has type ~a but the expected result type is ~a")
                                    got exp)
                            (clause*-stx cl*)))])
                      (unify (apply-subst s-acc t-body) (apply-subst s-acc result-type)))])
           (infer-return (cons (subst-compose s-u s-acc) (apply-subst s-u t-body)))))))))))

;; Type a pattern.  Returns (bindings, pattern-type).
;; infer-pattern returns a third value — the list of
;; existential hypotheses produced by an existential-ctor pattern.
;; For non-existential patterns this is '(); the caller threads them
;; into the clause body's constraint-reduction so the body's class-
;; method calls on ex-skolems can be discharged.
;; Leaf (no infer-expr): fresh tvars via m:fresh-tvar, the arg walk becomes a
;; monadic named-let.  instantiate-ctor-scheme stays direct (box-backed for
;; now).  `infer-pattern` is the bridged 3-value entry the clause helpers call.
(define (infer-pattern/m pat env)
  (match pat
    [(p:wild _)  (let/infer ([α (m:fresh-tvar)]) (infer-return (list '() α '())))]
    [(p:lit v _) (infer-return (list '() (literal-type v) '()))]
    [(p:var x _)
     (let/infer ([α (m:fresh-tvar)]) (infer-return (list (list (cons x α)) α '())))]
    [(p:ctor name args stx)
     (define info (env-ref-data env name))
     (cond
       [(not info)
        (raise-syntax-error 'infer
          (format "unknown data constructor: ~s~a"
                  name (suggest-similar name env))
          stx)]
       [(not (= (length args) (data-info-arity info)))
        (define fields (env-ref-struct-fields env name))
        (define field-hint
          (cond
            [(and fields (not (null? fields)))
             (format " (fields: ~a)"
                     (string-join (map symbol->string fields) ", "))]
            [else ""]))
        (raise-syntax-error 'infer
          (format "constructor ~s expects ~a arg(s), pattern has ~a~a"
                  name (data-info-arity info) (length args) field-hint)
          stx)]
       [else
        ;; Universal data tparams → fresh tvars (unify with scrutinee);
        ;; existential ex-tvars → fresh SKOLEMS; the ctor's qual context
        ;; becomes hypotheses the pattern proves.
        (let/infer ([rc (instantiate-ctor-scheme/m (data-info-scheme info)
                                                   (data-info-ex-tvars info))])
        (let* ([ctor-type (car rc)] [ex-hyps (cdr rc)])
        (define-values (arg-tys result-ty)
          (unfold-arrow ctor-type (length args)))
        (let/infer ([acc (let loop ([args args] [arg-tys arg-tys]
                                    [bindings '()] [s empty-subst] [hyps ex-hyps])
                           (cond
                             [(null? args) (infer-return (list bindings s hyps))]
                             [else
                              (let/infer ([rp (infer-pattern/m (car args) env)])
                                (let* ([bs (car rp)] [t (cadr rp)] [inner-hyps (caddr rp)]
                                       [s-u (unify (apply-subst s t) (apply-subst s (car arg-tys)))])
                                  (loop (cdr args) (cdr arg-tys)
                                        (append bindings bs)
                                        (subst-compose s-u s)
                                        (append hyps inner-hyps))))]))])
          (let* ([all-bindings (car acc)] [s-acc (cadr acc)] [all-ex-hyps (caddr acc)])
            (infer-return
             (list (for/list ([b (in-list all-bindings)])
                     (cons (car b) (apply-subst s-acc (cdr b))))
                   (apply-subst s-acc result-ty)
                   all-ex-hyps))))))])]))

;; Instantiate a data ctor's scheme for use in a pattern.
;; Universally-quantified data-tparams become fresh tvars (will
;; unify with the scrutinee's type).  Existentially-quantified
;; ex-tvars become fresh SKOLEMS (rigid tcons).  Any qual context
;; constraints (with the skolem substitution applied) are added to
;; pending preds, making them hypotheses available to the clause
;; body — the pattern itself supplies the proof.
;; Returns two values: the instantiated bare ctor type, and the
;; list of existential hypotheses (constraints in terms of fresh
;; ex-skolems) that should be available to the surrounding match
;; arm as already-proven.
(define (instantiate-ctor-scheme/m sch ex-tvars)
  (cond
    [(null? ex-tvars)
     (let/infer ([t (instantiate/m sch)]) (infer-return (cons t '())))]
    [else
     (match sch
       [(scheme vs body)
        (define ex-set (list->seteq ex-tvars))
        ;; ex-tvars → gensym SKOLEMS (pure); others → fresh tvars (monadic).
        (let/infer ([s (let loop ([vs vs] [s empty-subst])
                         (cond
                           [(null? vs) (infer-return s)]
                           [(set-member? ex-set (car vs))
                            (loop (cdr vs)
                                  (subst-extend s (car vs)
                                                (tcon (gensym (format "$ex-skolem.~a." (car vs))))))]
                           [else
                            (let/infer ([t (m:fresh-tvar (car vs))])
                              (loop (cdr vs) (subst-extend s (car vs) t)))]))])
          (let ([raw (apply-subst s body)])
            (cond
              [(qual? raw) (infer-return (cons (qual-body raw) (qual-constraints raw)))]
              [else (infer-return (cons raw '()))])))])]))
;; Compile-time exhaustiveness check for `match`.  Tabular cases:
;;   - any wildcard or variable pattern is a universal catchall.
;;   - on a known ADT, every declared constructor must appear (unless
;;     a catchall is present).
;;   - on Boolean, both #t and #f literals must appear.
;;   - on other primitive scrutinee types (Integer, String, …) and on
;;     scrutinees whose type is still polymorphic, a catchall is required.
;; Returns Infer void: the reachability probes draw fresh tvars, so it threads
;; `st` through ctor-reachable-at?/m.  Every non-probing branch is infer-return.
(define (check-exhaustive!/m scrut-type clauses stx env)
  (define head
    (let loop ([t scrut-type])
      (match t
        [(tcon n)        n]
        [(tapp (tcon n) _) n]
        [_              #f])))
  (define (catchall? c)
    ;; A guarded clause cannot satisfy exhaustiveness because the
    ;; guard may fail — even a wildcard pattern under #:when is not a
    ;; catch-all.
    (and (not (clause-guard c))
         (or (p:wild? (clause-pattern c))
             (p:var?  (clause-pattern c)))))
  (define has-catchall? (for/or ([c (in-list clauses)]) (catchall? c)))
  (cond
    [has-catchall? (infer-return (void))]
    [(eq? head 'Boolean)
     (define hits
       (for/fold ([acc '()]) ([c (in-list clauses)] #:unless (clause-guard c))
         (match (clause-pattern c)
           [(p:lit v _) (cons v acc)]
           [_ acc])))
     (unless (and (member #t hits) (member #f hits))
       (raise-syntax-error 'infer
         "non-exhaustive match on Boolean — both #t and #f must be covered"
         stx))
     (infer-return (void))]
    [head
     (define ti (env-ref-tcon env head))
     (cond
       [(not ti)
        (raise-syntax-error 'infer
          "non-exhaustive match: needs a wildcard or variable pattern"
          stx)]
       [else
        ;; For GADT scrutinees, only ctors whose declared result type can
        ;; unify with the actual scrutinee type are *reachable* here; drop
        ;; the unreachable ones from the must-cover set (probes thread st).
        (let/infer ([needed (let loop ([cs (tcon-info-ctors ti)] [acc '()])
                              (cond
                                [(null? cs) (infer-return (reverse acc))]
                                [else
                                 (let/infer ([reach? (ctor-reachable-at?/m (car cs) scrut-type env)])
                                   (loop (cdr cs) (if reach? (cons (car cs) acc) acc)))]))])
          (let ()
            (define hit
              (for/fold ([acc '()]) ([c (in-list clauses)] #:unless (clause-guard c))
                (match (clause-pattern c)
                  [(p:ctor name _ _) (cons name acc)]
                  [_ acc])))
            (define missing (filter (lambda (c) (not (member c hit))) needed))
            (unless (null? missing)
              (raise-syntax-error 'infer
                (format "non-exhaustive match: missing constructor(s) ~s"
                        missing)
                stx))
            (infer-return (void))))])]
    [else
     (raise-syntax-error 'infer
       "non-exhaustive match: needs a wildcard or variable pattern"
       stx)]))

;; A GADT constructor `c` is reachable at a scrutinee of
;; type `scrut-type` only if the ctor's declared result type can
;; unify with the scrutinee type.  For non-GADT ctors (default
;; uniform result), reachability is always true.
(define (ctor-reachable-at?/m ctor-name scrut-type env)
  (define info (env-ref-data env ctor-name))
  (cond
    [(not info) (infer-return #t)]
    [else
     (match (data-info-scheme info)
       [(scheme vs body)
        (let/infer ([s (fresh-subst/m vs)])
          (let* ([raw (apply-subst s body)]
                 [stripped (qual-body-deep raw)]
                 ;; Walk arrows to the result type.
                 [bare-result (let loop ([t stripped])
                                (cond [(arrow? t) (loop (arrow-cod t))] [else t]))])
            (infer-return
             (with-handlers ([exn:fail:unify? (lambda (_) #f)])
               (unify bare-result scrut-type)
               #t))))])]))

;; Does `t` have AT LEAST `n` leading arrow constructors?
;; Used by the top:def declared-signature branch to decide when we
;; can push skolemized parameter types into a lambda body's env.
(define (decl-arrow-depth-ge? t n)
  (cond
    [(zero? n) #t]
    [(arrow? t) (decl-arrow-depth-ge? (arrow-cod t) (sub1 n))]
    [else #f]))

(define (unfold-arrow t n)
  (let loop ([t t] [n n] [acc '()])
    (cond
      [(zero? n) (values (reverse acc) t)]
      [(arrow? t) (loop (arrow-cod t) (sub1 n) (cons (arrow-dom t) acc))]
      [else (error 'unfold-arrow
                   "expected ~a more arrow(s) in ~v but ran out" n t)])))

;; ----- top-level forms ----------------------------------------------

;; Four-phase order-invariant pipeline.  Within a single rackton
;; module, top-level forms may reference each other in any order:
;; mutually recursive value definitions, mutually recursive data
;; types, classes used before they're declared, instances that
;; mention later types.
;;
;;   Phase A — type infrastructure: process requires; pre-register
;;     tcon shells (name + arity, empty ctor list) so mutually
;;     recursive data types resolve; register aliases lazily (just
;;     stores the AST target); register struct-fields; resolve effect
;;     op signatures; process every class form fully (resolves method
;;     schemes); resolve every data type's ctor schemes (now tcons
;;     and classes are visible); resolve every top:dec into a shared
;;     `declared` table.  Order within Phase A is fixed so that each
;;     sub-step's inputs are already in env.
;;
;;   Phase B — def pre-registration: for each top:def, install the
;;     name in env with its declared scheme if present, else a fresh
;;     tvar.  This lets later phases see every top-level name.
;;
;;   Phase C — instance registration: run handle-instance-form on
;;     each top:instance in source order.  Method bodies are inferred
;;     here; def names from Phase B are visible.  Instance dispatch
;;     tables register at codegen time (source order) so runtime
;;     resolution still sees everything before user code runs.
;;
;;   Phase D — body inference: build a dependency graph over the
;;     top:def list (edges: f → g iff f's body free-references g),
;;     compute SCCs in topological order via Tarjan, infer each SCC
;;     monomorphically together, then generalize every binding in the
;;     SCC against the env enriched with the SCC's bindings.  Forward
;;     non-recursive references see the polymorphic scheme of the
;;     target; mutually recursive bindings share a monomorphic shape
;;     within their SCC.
(define (infer-program forms [env initial-env])
  (define-values (env* _ _forms _st) (infer-program/phases forms env (hasheq)))
  env*)

;; Like `infer-program`, but also returns the post-expansion form list
;; (with every `#:derive-superclasses` instance replaced by the plain
;; instances it synthesized).  The elaborator drives codegen off THIS
;; list so the synthesized superclass instances are lowered too.
;; Run inference and also produce the codegen-plan: the tables codegen
;; consumes.  The inference-output parameters are owned here (created fresh,
;; bound around the phases, then read out into the plan) rather than set up
;; by the elaborator, so the infer→codegen handoff is an explicit returned
;; value.  `current-method-uses` is inference-internal scratch (settled into
;; the resolutions) and stays out of the plan.
;; Returns (values env forms* codegen-plan monomorphized-sites).  The
;; monomorphization log (a newest-first list) is read out of the final st,
;; alongside the plan; the elaborator publishes it to the runtime.
(define (infer-program+forms forms [env initial-env])
  (define-values (env* _ forms* final-st) (infer-program/phases forms env (hasheq)))
  (values env* forms*
          (codegen-plan (st-table final-st 'method-resolutions)
                        (st-table final-st 'method-dict-resolutions)
                        (st-table final-st 'needs-dict-defs)
                        (st-table final-st 'instance-default-bodies)
                        (env-return-typed-methods env*))
          (st-table final-st 'monomorphized-sites)))

;; `prior-declared` carries forward `top:dec` schemes registered in
;; previous REPL inputs.  A fresh `(infer-program)` call always
;; supplies an empty map; the REPL's `elaborate-form` passes its
;; persisted declared so a `(: foo …)` declared in one input still
;; applies to a `(define foo …)` in a later one.
;; The optional `st0` carries inference state in (fresh counter, pending preds,
;; and the codegen-plan tables).  Batch callers start fresh; the REPL passes
;; the st it persists so its resolution tables accumulate across inputs.  The
;; final st is returned so the caller can read the plan tables out of it.
(define (infer-program/phases forms env prior-declared [st0 (make-infer-state)])
  ;; ---- Phase A: type infrastructure ----
  (define-values (env-after-A declared)
    (run-phase-A env forms prior-declared))
  ;; ---- Cross-class derivation expansion ----
  ;; Now that every class (prelude, local, imported) is in env, rewrite
  ;; each `#:derive-superclasses` instance into the plain instances it
  ;; synthesizes.  Every later phase — and codegen — runs over `forms*`.
  (define forms* (expand-derive-instances forms env-after-A))
  ;; ---- Superclass existence ----
  ;; Every superclass a protocol names must be a class that actually
  ;; exists.  Checked here, after Phase A, so a forward reference (a
  ;; subclass declared before its superclass) and an imported
  ;; superclass both resolve; only a genuinely undefined name — a typo,
  ;; or a non-class identifier — is flagged.
  (check-superclass-existence env-after-A forms*)
  ;; ---- Phase B: pre-register def names ----
  (define-values (env-after-B def-tvars st1)
    (run-phase-B env-after-A declared forms* st0))
  ;; ---- Phase C: instance registration (+ method body inference) ----
  (define-values (env-after-C st2) (run-phase-C env-after-B declared forms* st1))
  ;; ---- Superclass obligations ----
  ;; Now that every instance — local, later-declared, and imported — is
  ;; in env, require each declared instance to satisfy its class's
  ;; superclasses.  Done here (not per-instance in Phase C) so the check
  ;; is order-independent: a superclass instance may be declared after
  ;; the subclass instance that needs it.
  (check-superclass-obligations env-after-C forms*)
  ;; ---- Phase D: SCC-based body inference for top:defs ----
  (define-values (env-after-D st3) (run-phase-D env-after-C declared def-tvars forms* st2))
  ;; Second value: the declared map to carry into the next REPL input —
  ;; this input's `(: foo …)` decs merged in (Phase A), minus every name
  ;; whose define landed in this input.  A signature is consumed by its
  ;; define, so a later REPL redefinition is inferred fresh rather than
  ;; checked against a spent declaration.
  (values env-after-D
          (for/fold ([d declared]) ([f (in-list forms*)] #:when (top:def? f))
            (hash-remove d (top:def-name f)))
          forms*
          st3))

;; Verify that every superclass named in a protocol declaration refers
;; to a class that exists in `env` (locally defined or imported).  This
;; is the definition-time complement to check-superclass-obligations
;; (which checks instances): it catches a superclass that is an
;; uppercase but undefined name — a typo like `Functr` for `Functor` —
;; which the syntactic class-name check cannot see.  The `~` equality
;; predicate is a constraint head but not a class (it is discharged by
;; unification, never by an instance), so it is skipped, exactly as the
;; entailment checker special-cases it.
(define (check-superclass-existence env forms)
  (for* ([f (in-list forms)] #:when (top:class? f)
         [s (in-list (top:class-supers f))]
         #:unless (eq? (constraint-class s) '~))
    (unless (env-ref-class env (constraint-class s) #f)
      (raise-syntax-error 'infer
        (format "class ~a: superclass ~a is not a defined class"
                (constraint-class (top:class-head f))
                (constraint-class s))
        (or (constraint-stx s) (top:class-stx f))))))

;; Verify that every instance declared in `forms` satisfies the
;; superclass constraints of its class.  For an instance `C T₁…Tₙ` with
;; context `ctx`, substitute the instance args for `C`'s parameters in
;; each superclass predicate and require the result to be entailed by
;; the full instance set (in `env`) together with `ctx` as hypotheses.
;; The instance's own type variables are rigid: `match-pred` binds only
;; a candidate instance's head variables, treating the target as ground,
;; so the obligation reads "for all the instance's variables, the
;; superclass holds" — exactly the coherence requirement.
(define (check-superclass-obligations env forms)
  (for ([f (in-list forms)] #:when (top:instance? f))
    (define head  (resolve-constraint (top:instance-head f) env))
    (define cinfo (env-ref-class env (pred-class head) #f))
    (when cinfo
      (define ctx
        (for/list ([c (in-list (top:instance-context f))])
          (resolve-constraint c env)))
      (define σ
        (for/fold ([s empty-subst])
                  ([p (in-list (class-info-params cinfo))]
                   [a (in-list (pred-args head))])
          (subst-extend s p a)))
      (for ([sp (in-list (class-info-supers cinfo))])
        (define target (apply-subst σ sp))
        (unless (entail? env ctx target)
          (raise-syntax-error 'infer
            (format "instance ~a requires ~a, which has no instance"
                    (pred->datum head) (pred->datum target))
            (top:instance-stx f)))))))

;; ----- cross-class derivation expansion ----------------------------
;;
;; A `#:derive-superclasses` instance bundles only the irreducible
;; primitives (e.g. `pure` + `flatmap`).  Rewrite it into plain
;; `top:instance` forms: one synthesized instance per MISSING superclass
;; (filling its methods from the deriving class's `#:derive` table and
;; the bundled primitives), plus the base instance carrying only the
;; deriving class's own methods.  Superclasses that already have an
;; instance — hand-written or imported — are left untouched, so the two
;; never collide.
(define (expand-derive-instances forms env)
  (cond
    [(not (for/or ([f (in-list forms)]) (top:derive-instance? f))) forms]
    [else
     (define existing (make-hash))
     (for ([f (in-list forms)] #:when (top:instance? f))
       (define h (resolve-constraint (top:instance-head f) env))
       (hash-set! existing (cons (pred-class h) (head-spine-tcon h)) #t))
     (append*
      (for/list ([f (in-list forms)])
        (if (top:derive-instance? f)
            (synthesize-derived-instances f env existing)
            (list f))))]))

;; The spine type-constructor of a pred's first argument, e.g. `Box` for
;; `(Functor Box)` and `StateT` for `(Monad (StateT s m))`; #f if none.
(define (head-spine-tcon p)
  (and (pair? (pred-args p)) (type-head-tcon (car (pred-args p)))))

;; Transitive superclass class-NAMES of `class-name`, nearest first.
(define (superclass-name-closure env class-name)
  (let loop ([pending (list class-name)] [seen '()] [acc '()])
    (cond
      ;; `acc` is already accumulated nearest-first (BFS append order):
      ;; (Applicative Functor) for Monad.  Synthesizing in this order
      ;; registers a nearer superclass before a farther one that a
      ;; derived body may reference.
      [(null? pending) acc]
      [else
       (define cinfo (env-ref-class env (car pending) #f))
       (define sup-names
         (if cinfo (map pred-class (class-info-supers cinfo)) '()))
       (define fresh (filter (lambda (s) (not (member s seen))) sup-names))
       (loop (append (cdr pending) fresh)
             (append seen fresh)
             (append acc fresh))])))

;; Merge the cross-class derivation tables of `C` and its superclass
;; closure into one `superclass → (method → expr)` map, with `C`'s own
;; entries taking precedence over an intermediate superclass's.
(define (merged-derive-table env C closure)
  (define (hash-merge2 a b) ; entries of b win
    (for/fold ([h a]) ([(k v) (in-hash b)]) (hash-set h k v)))
  (for/fold ([acc (hasheq)])
            ([k (in-list (reverse (cons C closure)))]) ; farthest … nearest (C last)
    (define ci (env-ref-class env k #f))
    (cond
      [ci (for/fold ([a acc]) ([(S tbl) (in-hash (class-info-super-derives ci))])
            (hash-set a S (hash-merge2 (hash-ref a S (hasheq)) tbl)))]
      [else acc])))

(define (synthesize-derived-instances di env existing)
  (define surface-head (top:derive-instance-head di))      ; surface constraint
  (define head-pred    (resolve-constraint surface-head env))  ; core pred
  (define C            (pred-class head-pred))
  (define spine        (head-spine-tcon head-pred))
  (define ctx          (top:derive-instance-context di))
  (define stx          (top:derive-instance-stx di))
  (define cinfo (env-ref-class env C #f))
  (unless cinfo
    (raise-syntax-error 'derive-superclasses
      (format "#:derive-superclasses on an instance of unknown class ~a" C)
      stx))
  (define closure (superclass-name-closure env C))
  (define merged  (merged-derive-table env C closure))
  ;; primitives the user bundled in the instance body (e.g. `pure`).
  (define bundled
    (for/fold ([acc (hasheq)]) ([m (in-list (top:derive-instance-methods di))]
                                #:when (top:def? m))
      (hash-set acc (top:def-name m) m)))
  ;; the base instance keeps only the deriving class's OWN methods.
  (define C-methods (class-info-methods cinfo))
  (define base-methods
    (for/list ([m (in-list (top:derive-instance-methods di))]
               #:when (or (inst-type-fam? m)
                          (and (top:def? m)
                               (hash-has-key? C-methods (top:def-name m)))))
      m))
  (define base-inst (top:instance ctx surface-head base-methods stx))
  ;; synthesize each missing superclass, reusing the head's surface type
  ;; arguments so the head re-resolves normally.  Emit the BASE instance
  ;; first, then superclasses nearest-first (Applicative before Functor):
  ;; Phase C infers method bodies in this order, and a derived body may
  ;; reference a nearer class's method (e.g. Functor's `fmap` calls the
  ;; deriving Monad's `flatmap` and Applicative's `pure`), which must
  ;; already be registered for its concrete-type constraint to discharge.
  (define surface-args (constraint-args surface-head))
  ;; the superclasses we will actually synthesize here (the rest already
  ;; have an instance for this carrier and are left untouched).
  (define synth-supers
    (for/list ([S (in-list closure)]
               #:unless (or (hash-ref existing (cons S spine) #f)
                            (instance-already-in-env? env S spine)))
      S))
  ;; Reject a cross-class default/derived cycle among the methods this
  ;; instance leaves to be auto-filled, before emitting instances that
  ;; would only loop at runtime.
  (check-derived-instance-cycle C synth-supers merged bundled env head-pred stx)
  (define synth
    (for/list ([S (in-list synth-supers)])
      (synthesize-one-superclass S surface-args ctx merged bundled env stx)))
  (cons base-inst synth))

;; Is there already an instance of class `S` for the type whose spine is
;; `spine`, among prelude/imported instances in env?  (Local instances
;; aren't registered yet at expansion time — those are covered by the
;; `existing` set built from the form list.)
(define (instance-already-in-env? env S spine)
  (for/or ([inst (in-list (env-instances env S))])
    (equal? (head-spine-tcon (instance-info-head inst)) spine)))

(define (synthesize-one-superclass S surface-args ctx merged bundled env stx)
  (define S-cinfo (env-ref-class env S))
  (define S-derive (hash-ref merged S (hasheq)))
  (define methods
    (for/fold ([acc '()]) ([(mname _sch) (in-hash (class-info-methods S-cinfo))])
      (cond
        ;; (1) a primitive the user bundled (e.g. `pure`) — reuse verbatim.
        [(hash-ref bundled mname #f)
         => (lambda (def) (cons def acc))]
        ;; (2) a derivation body from the table — freshen its syntax to
        ;;     the instance site.  Each node gets a DISTINCT handle so the
        ;;     method-resolution map (keyed by syntax identity) doesn't
        ;;     collapse this body's uses with another deriving instance's.
        [(hash-ref S-derive mname #f)
         => (lambda (expr)
              (cons (top:def mname (freshen-ast expr stx) stx) acc))]
        ;; (3) omit, so S's own intra-class default fills it — or, if
        ;;     there is no default (the `pure` floor), handle-instance-form
        ;;     later raises "missing method ~s with no default".
        [else acc])))
  (top:instance ctx (constraint S surface-args stx) methods stx))

;; Phase A — process forms that build the type-level env, in the
;; order described above.  Returns the post-A env and the `declared`
;; table mapping each top:dec'd name to its resolved scheme,
;; combined with any declarations carried over from prior REPL inputs.
(define (run-phase-A env forms prior-declared)
  ;; A1: requires bring tcons/classes/instances from other modules
  ;; into env; must run before any local type resolution.
  (define env-A1
    (for/fold ([e env]) ([f (in-list forms)] #:when (top:require? f))
      ;; Phase-A forms (require/effect/class) are pure: a throwaway st suffices.
      (let-values ([(e* _ _st) (handle-top-form f e (hasheq) (make-infer-state))])
        e*)))
  ;; A2-A4: pre-register tcon shells, aliases (lazy), struct-fields.
  ;; None of these resolve types, so order among them is irrelevant —
  ;; just hash inserts.
  (define env-A2 (pre-register-tcon-shells env-A1 forms))
  (define env-A3
    (for/fold ([e env-A2]) ([f (in-list forms)] #:when (top:alias? f))
      (env-extend-alias e (top:alias-name f) (top:alias-params f)
                        (top:alias-target f))))
  (define env-A4
    (for/fold ([e env-A3]) ([f (in-list forms)] #:when (top:struct-fields? f))
      (env-extend-struct-fields e (top:struct-fields-struct-name f)
                                (top:struct-fields-field-names f))))
  ;; A4.5: infer each data type's kind from its constructor field types
  ;; (replacing the arity-placeholder kinds on the shells).  Runs after
  ;; aliases (A3) — field types may use them — and before any kind-
  ;; checked resolution of a type that mentions these constructors.
  (define env-A4.5 (infer-data-kinds env-A4 forms))
  ;; A5: effects (resolve op types against the tcon-complete env).
  (define env-A5
    (for/fold ([e env-A4.5]) ([f (in-list forms)] #:when (top:effect? f))
      ;; Phase-A forms (require/effect/class) are pure: a throwaway st suffices.
      (let-values ([(e* _ _st) (handle-top-form f e (hasheq) (make-infer-state))])
        e*)))
  ;; A6: classes (handle-class-form resolves method schemes and
  ;; registers each method as a polymorphic var).
  (define env-A6
    (for/fold ([e env-A5]) ([f (in-list forms)] #:when (top:class? f))
      ;; Phase-A forms (require/effect/class) are pure: a throwaway st suffices.
      (let-values ([(e* _ _st) (handle-top-form f e (hasheq) (make-infer-state))])
        e*)))
  ;; A7: data ctors (now classes are in env, so ctor types with
  ;; class constraints in extra-context resolve correctly).
  (define env-A7
    (resolve-data-ctors env-A6 forms))
  ;; A8: resolve every top:dec into the shared declared table; mirror
  ;; the entry into env so the rest of the pipeline can env-ref-var
  ;; before the def's body has been inferred.
  (define declared
    (for/fold ([d prior-declared]) ([f (in-list forms)] #:when (top:dec? f))
      ;; env-A6 carries the imported (A1) and local (A3) aliases that the
      ;; declared types may reference; the bare `env` does not.
      (hash-set d (top:dec-name f) (resolve-scheme (top:dec-type f) env-A6))))
  (define env-A8
    (for/fold ([e env-A7]) ([(name sch) (in-hash declared)])
      (env-extend-var e name sch)))
  ;; A9: foreign (host) imports — register each as a typed var, like a
  ;; bare dec, but NOT in `declared` (there is no Rackton def body; the
  ;; binding comes from the Racket require codegen emits).
  (define env-A9
    (for/fold ([e env-A8]) ([f (in-list forms)])
      (cond
        [(top:foreign? f)
         (env-extend-var e (top:foreign-name f)
                         (resolve-scheme (top:foreign-type f) env-A7))]
        [(top:foreign-c? f)
         (env-extend-var e (top:foreign-c-name f)
                         (resolve-scheme (top:foreign-c-type f) env-A8))]
        [else e])))
  (values env-A9 declared))

;; Pre-register every top:data's tcon header in env: name + arity +
;; full ctor-name list + abstract flag.  Ctor schemes are resolved
;; separately in `resolve-data-ctors` after every tcon (and class) is
;; in env.  Pre-registering the ctor name list (not just the count)
;; matches what `env-extend-tcon`'s final state would look like, so
;; `env-ref-tcon` answers correctly during type resolution of other
;; forms' bodies.
(define (pre-register-tcon-shells env forms)
  (for/fold ([e env]) ([f (in-list forms)] #:when (top:data? f))
    (define tname    (top:data-name f))
    (define tparams  (top:data-params f))
    (define ctors    (top:data-ctors f))
    (define abstract? (top:data-abstract? f))
    (env-extend-tcon e tname
                     (tcon-info tname (length tparams)
                                ;; Placeholder kind; Phase A2.5
                                ;; (infer-data-kinds) replaces it with the
                                ;; inferred kind before any type is checked.
                                (arity->star-kind (length tparams))
                                (for/list ([c (in-list ctors)])
                                  (data-ctor-name c))
                                abstract?
                                (top:data-runtime-tag f)))))

;; ----- kinds: the elaboration walk -----------------------------------

;; The kinds of the primitive type constructors that are never
;; registered as data: the scalar types and the function arrow.  All
;; other constructors carry their kind in tcon-info.
(define primitive-kind-table
  (hasheq 'Integer kstar 'Boolean kstar 'String kstar 'Float kstar
          '-> (kind-arrow* (list kstar kstar) kstar)))

;; The kind of type constructor `name`: a batch seed (during
;; data-kind inference) wins, then the env's stored kind, then the
;; primitive table; #f when unknown (a resolved type should never
;; mention an unknown tcon, so callers may treat #f leniently).
(define (tcon-kind-of env batch-kinds name)
  (or (hash-ref batch-kinds name #f)
      (let ([ti (env-ref-tcon env name #f)]) (and ti (tcon-info-kind ti)))
      (hash-ref primitive-kind-table name #f)))

;; Infer the kind of a resolved core type.  `tvar-kinds` is a mutable
;; hasheq name→kind, pre-seeded with the in-scope type variables and
;; auto-extended (fresh kvar) for any not seeded.  `batch-kinds` holds
;; the seed kinds of data types being inferred together (for self/
;; mutual recursion).  Returns (values kind ksubst); raises
;; exn:fail:kind-unify on an ill-kinded application.
(define (elab-kind t env batch-kinds tvar-kinds s)
  (match t
    [(tvar n)
     (values (hash-ref! tvar-kinds n (lambda () (kvar (gensym 'k)))) s)]
    [(tcon n)
     (define k (tcon-kind-of env batch-kinds n))
     (values (or k (kvar (gensym 'k))) s)]
    [(tapp h args)
     (define-values (kh s1) (elab-kind h env batch-kinds tvar-kinds s))
     (define-values (kargs s2)
       (for/fold ([acc '()] [s s1] #:result (values (reverse acc) s))
                 ([a (in-list args)])
         (define-values (ka s*) (elab-kind a env batch-kinds tvar-kinds s))
         (values (cons ka acc) s*)))
     (define result (kvar (gensym 'k)))
     (define expected (kind-arrow* kargs result))
     (define s3 (ksubst-compose
                 (unify-kind (apply-ksubst s2 kh) (apply-ksubst s2 expected))
                 s2))
     (values (apply-ksubst s3 result) s3)]
    [(tforall vs body)
     (for ([v (in-list vs)]) (hash-set! tvar-kinds v (kvar (gensym 'k))))
     (elab-kind body env batch-kinds tvar-kinds s)]
    [(qual _cs body)
     ;; The predicates' own kinds are checked at their resolution sites;
     ;; the qualified type's kind is its body's.
     (elab-kind body env batch-kinds tvar-kinds s)]
    [_ (values (kvar (gensym 'k)) s)]))

;; ----- kinds: data-type kind inference (Phase A2.5) ------------------

;; Infer and record the kind of every `top:data` constructor in this
;; batch.  Seed each `T(p1..pn)` with `κp1 -> … -> κpn -> *` (a data
;; type's result is always `*`), seeding ALL batch types before
;; constraining so self- and mutual recursion resolve against the
;; shared seeds; constrain every constructor field (and GADT result)
;; to kind `*`; then default residual param kvars to `*` and write the
;; concrete kind into the tcon shell.  `env` must already carry the
;; tcon shells (Phase A2) and aliases (A3) — field types may use both.
(define (infer-data-kinds env forms)
  (define datas (filter top:data? forms))
  (cond
    [(null? datas) env]
    [else
     ;; 1. Seed.
     (define seeds
       (for/list ([f (in-list datas)])
         (match-define (top:data _ tparams _ _ _ _) f)
         (define pkvars (for/list ([_ (in-list tparams)]) (kvar (gensym 'k))))
         (list f (map cons tparams pkvars) (kind-arrow* pkvars kstar))))
     (define batch-kinds
       (for/fold ([h (hasheq)]) ([sd (in-list seeds)])
         (hash-set h (top:data-name (car sd)) (caddr sd))))
     ;; 2. Constrain: every constructor field and GADT result is kind *.
     (define (demand-star core tvar-kinds s)
       (define-values (k s*) (elab-kind core env batch-kinds tvar-kinds s))
       (ksubst-compose (unify-kind (apply-ksubst s* k) kstar) s*))
     (define s
       (for/fold ([s empty-ksubst]) ([sd (in-list seeds)])
         (match-define (list f param-kvars _) sd)
         (for/fold ([s s]) ([c (in-list (top:data-ctors f))])
           (define tvar-kinds (make-hasheq))
           (for ([pk (in-list param-kvars)])
             (hash-set! tvar-kinds (car pk) (cdr pk)))
           (for ([ev (in-list (data-ctor-extra-tvars c))])
             (hash-set! tvar-kinds ev (kvar (gensym 'k))))
           (define s-fields
             (for/fold ([s s]) ([ft (in-list (data-ctor-field-types c))])
               (demand-star (resolve-type ft env) tvar-kinds s)))
           (cond
             [(data-ctor-result-type c)
              (demand-star (resolve-type (data-ctor-result-type c) env)
                           tvar-kinds s-fields)]
             [else s-fields]))))
     ;; 3. Solve & default, writing the concrete kind into each shell.
     (for/fold ([env env]) ([sd (in-list seeds)])
       (match-define (list f _ seed) sd)
       (define name (top:data-name f))
       (define ti (env-ref-tcon env name))
       (env-extend-tcon env name
                        (struct-copy tcon-info ti
                                     [kind (default-kind (apply-ksubst s seed))])))]))

;; Resolve every top:data form's ctor field types against the
;; type-level-complete env, registering each ctor as a data binding.
;; Mirrors the existing `handle-top-form` top:data branch's ctor loop.
(define (resolve-data-ctors env forms)
  (for/fold ([env env]) ([f (in-list forms)] #:when (top:data? f))
    (match-define (top:data tname tparams ctors stx _abstract? _runtime-tag) f)
    (define default-result-type
      (make-tapp (tcon tname)
                 (for/list ([p (in-list tparams)]) (tvar p))))
    (for/fold ([e env]) ([c (in-list ctors)])
      (define field-tys
        (for/list ([t (in-list (data-ctor-field-types c))])
          (resolve-type t env)))
      (define ctor-result-type
        (cond
          [(data-ctor-result-type c)
           (resolve-type (data-ctor-result-type c) env)]
          [else default-result-type]))
      (define ctor-fn-type
        (foldr make-arrow ctor-result-type field-tys))
      (define extra-tvars   (data-ctor-extra-tvars c))
      (define extra-context (data-ctor-extra-context c))
      (define qualified-body
        (cond
          [(null? extra-context) ctor-fn-type]
          [else
           (mqual (for/list ([cs (in-list extra-context)])
                    (resolve-constraint cs env))
                  ctor-fn-type)]))
      (define quantifier-vars
        (cond
          [(data-ctor-result-type c)
           (define ft-vars
             (apply set-union
                    (cons (seteq)
                          (for/list ([t (in-list field-tys)])
                            (type-vars t)))))
           (define rt-vars (type-vars ctor-result-type))
           (sort (set->list (set-union ft-vars rt-vars)) symbol<?)]
          [else (append tparams extra-tvars)]))
      (define sch (scheme quantifier-vars qualified-body))
      (env-extend-data e (data-ctor-name c)
                       (data-info tname (data-ctor-name c)
                                  (length field-tys) sch
                                  extra-tvars)))))

;; Phase B — pre-register every top:def's name in env so later
;; phases (instances, def-body inference) see all top-level names.
;; Names with a matching top:dec take that scheme; the rest get a
;; fresh tvar inside a degenerate `(scheme '() α)`.  Returns the
;; post-B env and a hasheq mapping each non-declared def name to its
;; placeholder tvar (used in Phase D to unify the inferred body type
;; with its slot).
(define (run-phase-B env declared forms st)
  (for/fold ([env env] [tvars (hasheq)] [st st])
            ([f (in-list forms)] #:when (top:def? f))
    (define name (top:def-name f))
    (cond
      [(hash-has-key? declared name)
       ;; Already registered in env-A8 with its declared scheme.
       (values env tvars st)]
      [else
       (define-values (α st′) (st:fresh st))
       (values (env-extend-var env name (scheme '() α))
               (hash-set tvars name α)
               st′)])))

;; Phase C — register every top:instance.  Reuses the existing
;; handle-instance-form which both registers the instance and
;; type-checks its method bodies.  Bodies see the full set of
;; pre-registered def names from Phase B.
(define (run-phase-C env declared forms st)
  (for/fold ([e env] [st st]) ([f (in-list forms)] #:when (top:instance? f))
    (define-values (e* _ st′) (handle-top-form f e declared st))
    (values e* st′)))

;; Phase D — infer top:def bodies in dependency order using SCC
;; analysis.  Each SCC's bindings are added to env with their
;; placeholders, inferred together, then generalized as a group.
(define (run-phase-D env declared def-tvars forms st)
  (define defs (filter top:def? forms))
  (define defs-by-name
    (for/fold ([acc (hasheq)]) ([d (in-list defs)])
      (hash-set acc (top:def-name d) d)))
  (define sccs (def-scc-order forms))
  (for/fold ([env env] [st st]) ([scc (in-list sccs)])
    (infer-def-scc env declared def-tvars defs-by-name scc st)))

;; The dependency-order SCC list for the top:def forms in `forms`.
;; Each SCC is a list of names; the outer list is in topological
;; order (callees before callers).  Exposed so codegen can emit
;; defines in the same order — a non-function-RHS def must come
;; after any def it references in its initializer.
(define (def-scc-order forms)
  (define defs (filter top:def? forms))
  (define name-set
    (for/seteq ([d (in-list defs)]) (top:def-name d)))
  (define edges
    (for/list ([d (in-list defs)])
      (cons (top:def-name d)
            (set->list (def-free-tops (top:def-expr d) name-set)))))
  (tarjan-sccs (map car edges) edges))

;; AST walker producing the set of top-level identifiers referenced
;; in `expr`, restricted to `top-names` (the set of all top:def names
;; in the module).  Shadowing-aware: identifiers bound by a lambda
;; parameter, let/letrec binding, match-pattern variable, or handle
;; clause's op-param / k-name are not counted for that subtree.
(define (def-free-tops expr top-names)
  (define seen (mutable-seteq))
  (let walk ([e expr] [shadowed (seteq)])
    (match e
      [(e:literal _ _) (void)]
      [(e:var n _)
       (when (and (set-member? top-names n)
                  (not (set-member? shadowed n)))
         (set-add! seen n))]
      [(e:lam params body _)
       (walk body (set-union shadowed (list->seteq params)))]
      [(e:app head args _)
       (walk head shadowed)
       (for ([a (in-list args)]) (walk a shadowed))]
      [(e:let bindings body _)
       (for ([b (in-list bindings)]) (walk (cdr b) shadowed))
       (define sh*
         (for/fold ([s shadowed]) ([b (in-list bindings)])
           (set-add s (car b))))
       (walk body sh*)]
      [(e:letrec bindings body _)
       (define sh*
         (for/fold ([s shadowed]) ([b (in-list bindings)])
           (set-add s (car b))))
       (for ([b (in-list bindings)]) (walk (cdr b) sh*))
       (walk body sh*)]
      [(e:if c th el _) (walk c shadowed) (walk th shadowed) (walk el shadowed)]
      [(e:ann body _ _) (walk body shadowed)]
      [(e:match scrut clauses _ _)
       (walk scrut shadowed)
       (for ([cl (in-list clauses)])
         (define sh* (set-union shadowed (pattern-bound-names (clause-pattern cl))))
         (when (clause-guard cl) (walk (clause-guard cl) sh*))
         (walk (clause-body cl) sh*))]
      [(e:match* scrutinees clauses _ _)
       (for ([s (in-list scrutinees)]) (walk s shadowed))
       (for ([cl (in-list clauses)])
         (define sh*
           (for/fold ([acc shadowed]) ([p (in-list (clause*-patterns cl))])
             (set-union acc (pattern-bound-names p))))
         (when (clause*-guard cl) (walk (clause*-guard cl) sh*))
         (walk (clause*-body cl) sh*))]
      [(e:update record updates _)
       (walk record shadowed)
       (for ([u (in-list updates)]) (walk (cdr u) shadowed))]
      [(e:handle expr clauses ret _)
       (walk expr shadowed)
       (when ret (walk ret shadowed))
       (for ([cl (in-list clauses)])
         (define sh* (set-union shadowed
                                (list->seteq (handle-clause-params cl))
                                (let ([k (handle-clause-k-name cl)])
                                  (if k (seteq k) (seteq)))))
         (walk (handle-clause-body cl) sh*))]
      [(e:escape _ _ _ _) (void)]
      [_ (void)]))
  seen)

(define (pattern-bound-names p)
  (match p
    [(p:var n _) (seteq n)]
    [(p:ctor _ args _)
     (for/fold ([s (seteq)]) ([a (in-list args)])
       (set-union s (pattern-bound-names a)))]
    [_ (seteq)]))

;; Tarjan's algorithm.  Input: a node list and an adjacency list
;; `(node . successors)`.  Output: list of SCCs (each an inner list
;; of node names) in *topological order* — earliest predecessors
;; first, so dependencies are inferred before their dependents.
;; Self-loops keep their node in a singleton SCC.  Edges to nodes
;; outside the input set (e.g. uses of class methods, prelude
;; bindings) are silently dropped — only intra-module dependencies
;; partition the def list.
(define (tarjan-sccs nodes edges-alist)
  (define succ (make-hasheq))
  (for ([entry (in-list edges-alist)])
    (hash-set! succ (car entry) (cdr entry)))
  (define index-of (make-hasheq))
  (define lowlink (make-hasheq))
  (define onstack (make-hasheq))
  (define stack '())
  (define sccs '())
  (define counter 0)
  (define (strongconnect v)
    (hash-set! index-of v counter)
    (hash-set! lowlink v counter)
    (set! counter (add1 counter))
    (set! stack (cons v stack))
    (hash-set! onstack v #t)
    (for ([w (in-list (hash-ref succ v '()))])
      (cond
        [(not (hash-has-key? index-of w))
         (strongconnect w)
         (hash-set! lowlink v
                    (min (hash-ref lowlink v) (hash-ref lowlink w)))]
        [(hash-ref onstack w #f)
         (hash-set! lowlink v
                    (min (hash-ref lowlink v) (hash-ref index-of w)))]))
    (when (= (hash-ref lowlink v) (hash-ref index-of v))
      (define scc
        (let loop ([acc '()])
          (define w (car stack))
          (set! stack (cdr stack))
          (hash-remove! onstack w)
          (cond
            [(eq? w v) (cons w acc)]
            [else (loop (cons w acc))])))
      (set! sccs (cons scc sccs))))
  (for ([v (in-list nodes)] #:unless (hash-has-key? index-of v))
    (strongconnect v))
  ;; Tarjan produces SCCs in reverse topological order; reverse to
  ;; deliver them in dependency order (callees before callers).
  (reverse sccs))

;; Infer one SCC's bindings together.  All members of the SCC are
;; visible in env when each body is type-checked; constraints
;; accumulate across the group; generalization runs at the SCC
;; boundary so mutually recursive bindings share a monomorphic shape
;; during inference and land as polymorphic schemes afterward.
;;
;; Each def's processing mirrors `handle-top-form`'s top:def branch
;; one of two ways:
;;   * declared (top:dec'd) — skolemize the scheme, push skolemized
;;     param types, infer body, unify, reduce against decl preds,
;;     resolve methods.
;;   * undeclared — use the placeholder tvar from Phase B; defer
;;     generalization until the SCC closes; detect needs-dict on the
;;     generalized scheme and run the same dict-skolem path the
;;     single-form branch uses.
(define (infer-def-scc env declared def-tvars defs-by-name scc-names st)
  (define-values (decl-names undecl-names)
    (partition (lambda (n) (hash-has-key? declared n)) scc-names))
  ;; Process declared members first: they're fully type-checked
  ;; and generalized immediately, just like the old declared path.
  ;; Their schemes are already in env from Phase A.
  (define-values (env-after-decl st1)
    (for/fold ([env env] [st st]) ([name (in-list decl-names)])
      (define f (hash-ref defs-by-name name))
      (infer-declared-def env declared (top:def-expr f) name (top:def-stx f) st)))
  ;; Process undeclared members of the SCC.  When |undecl-names| > 1
  ;; (genuine mutual recursion) we keep each binding's placeholder
  ;; tvar visible while every body is inferred, unify each body's
  ;; type into the tvar, then generalize all bindings together at
  ;; the end of the SCC.  When |undecl-names| = 1 (a singleton SCC
  ;; with no self-recursion either), single-binding inference
  ;; matches the original handle-top-form behavior exactly.
  (infer-undeclared-scc env-after-decl declared def-tvars defs-by-name
                        undecl-names st1))

;; Mirror of the declared-signature branch of the old
;; `handle-top-form` top:def case.  The scheme was already entered
;; into env during Phase A and into `declared` for this lookup.
(define (infer-declared-def env declared expr name stx st)
  (define decl-scheme (hash-ref declared name))
  (define needs-dict-reqs (var-dict-requirements env decl-scheme))
  (define-values (decl-ty decl-preds dict-skolems dict-arg-names)
    (cond
      [(null? needs-dict-reqs)
       (define-values (t p) (skolemize decl-scheme))
       (values t p (hasheq) '())]
      [else
       (define-values (t p s) (skolemize/tracked decl-scheme))
       (define-values (sk-map args) (build-dict-skolems needs-dict-reqs s env))
       (values t p sk-map args)]))
  (define saved-skolems (current-dict-skolems))
  (current-dict-skolems dict-skolems)
  (define-values (s t st1)
    (cond
      [(and (e:lam? expr)
            (decl-arrow-depth-ge? decl-ty (length (e:lam-params expr))))
       (define-values (arg-tys cod)
         (unfold-arrow decl-ty (length (e:lam-params expr))))
       (define env-lam
         (for/fold ([e env])
                   ([p (in-list (e:lam-params expr))]
                    [ty (in-list arg-tys)])
           (env-extend-var e p (scheme '() ty))))
       (define-values (s-body t-body st-b)
         (cond
           [(or (e:match? (e:lam-body expr))
                (e:match*? (e:lam-body expr)))
            (parameterize ([current-expected-type cod])
              (infer-expr (e:lam-body expr) env-lam st))]
           [else (infer-expr (e:lam-body expr) env-lam st)]))
       (values s-body
               (foldr make-arrow t-body
                      (for/list ([ty (in-list arg-tys)])
                        (apply-subst s-body ty)))
               st-b)]
      [else (infer-expr expr env st)]))
  (define s-u
    (with-handlers
     ([exn:fail:unify?
       (lambda (_)
         (raise-syntax-error 'infer
           (format "definition of ~s has the wrong type\n~a"
                   name
                   (expected/got-lines (format-scheme decl-scheme)
                                       (pretty-type (apply-subst s t))))
           stx))])
     (unify (normalize-type env (apply-subst s t)) decl-ty)))
  (define final-subst0 (subst-compose s-u s))
  (define st2 (st:apply-subst-to-preds st1 final-subst0))
  ;; Run functional-dependency improvement before reducing, exactly as the
  ;; undeclared path does inside `generalize`.  A leftover constraint like
  ;; `(Arrow LFun a)` — where the product `a` appears only in the
  ;; constraint, never in the declared type — is closed by the fundep
  ;; `cat -> p` against the matching instance (`a := LPair`), so it reduces
  ;; instead of being reported as unsolved.
  (define-values (fd-sub st3) (improve-by-fds env st2))
  ;; Also improve against the declared constraints themselves: a
  ;; fundep-bearing hypothesis (e.g. `(ArrowLoop cat p)`) determines the
  ;; product/coproduct for the body, connecting the body's fresh tensor
  ;; vars to the signature's `p` / `s`.
  (define hyp-closure
    (append-map (lambda (p) (by-super env (apply-subst fd-sub p))) decl-preds))
  (define-values (hyp-fd-sub st4) (improve-by-hyp-fds env hyp-closure st3))
  (define final-subst (subst-compose hyp-fd-sub (subst-compose fd-sub final-subst0)))
  (define remaining-preds
    (reduce-context env (map (lambda (p) (apply-subst final-subst p)) decl-preds)
                    (st:preds st4)))
  (when (not (null? remaining-preds))
    (raise-syntax-error 'infer
      (render-doc (labeled-block (format "unsolved constraints in ~a:" name)
                                 (map pretty-pred remaining-preds))
                  (current-type-columns))
      stx))
  (define st5 (st:set-preds st4 '()))
  (define st6 (resolve-method-uses st5 final-subst env))
  (define st7 (if (null? dict-arg-names) st6
                  (st-table-put st6 'needs-dict-defs name dict-arg-names)))
  (current-dict-skolems saved-skolems)
  (values env st7))

;; Infer one SCC of undeclared defs.  Singleton SCC (no self-edge):
;; behaves exactly like the previous single-def undeclared path,
;; including the inferred-needs-dict detection.  Mutual SCC (or
;; singleton with self-edge): each member's placeholder tvar from
;; Phase B stays in env throughout body inference; generalization
;; happens at the end against an env scrubbed of the SCC members so
;; each scheme can quantify the type vars shared across the group.
(define (infer-undeclared-scc env declared def-tvars defs-by-name names st)
  (cond
    [(null? names) (values env st)]
    [else
     (define-values (s* defs-rev st1)
       (for/fold ([s empty-subst] [acc '()] [st st]) ([name (in-list names)])
         (define f (hash-ref defs-by-name name))
         (define expr (top:def-expr f))
         (define α (hash-ref def-tvars name))
         (define env-cur (apply-subst/env s env))
         (define-values (s-body t-body st-b) (infer-expr expr env-cur st))
         (define s-now (subst-compose s-body s))
         (define s-rec (unify (apply-subst s-now α) (apply-subst s-now t-body)))
         (values (subst-compose s-rec s-now)
                 (cons (list name f α) acc)
                 st-b)))
     (define defs (reverse defs-rev))
     (define st2 (st:apply-subst-to-preds st1 s*))
     (define env-after-subst (apply-subst/env s* env))
     (for/fold ([env env-after-subst] [st st2]) ([entry (in-list defs)])
       (match-define (list name f α) entry)
       (define final-ty (apply-subst s* α))
       ;; Strip *every* SCC member's binding from env before
       ;; generalize.  The current member's placeholder
       ;; `(scheme '() final-ty)` would otherwise pin its own type
       ;; vars into env-fv and block quantification.  Already-
       ;; generalized members (from earlier iterations) are safe to
       ;; remove or keep — their scheme-free-vars is empty either
       ;; way — so blanket removal is simpler than tracking which
       ;; have been finished.
       (define gen-env
         (for/fold ([e env]) ([n (in-list names)])
           (env-remove-var e n)))
       (finish-undeclared-def env gen-env (top:def-expr f)
                              name s* final-ty (top:def-stx f) st))]))

;; Shared finalizer for an undeclared def: detect inferred needs-dict,
;; record recursive dict-uses, generalize, resolve method calls.
;; Extracted so both single-form and SCC paths share one body.
;; `env` is where the new binding lands; `env-for-gen` is `env`
;; minus any SCC siblings whose placeholder tvars would block
;; quantification of tvars shared across the group.
(define (finish-undeclared-def env env-for-gen expr name s* final-ty stx st)
  (define env-fv (env-vars-free-vars env-for-gen))
  (define ty-fv  (type-vars final-ty))
  (define quant-set (set-subtract ty-fv env-fv))
  (define dict-bearing-preds
    (cond
      [(or (set-empty? quant-set) (not (e:lam? expr))) '()]
      [else
       (for/list ([p (in-list (st:preds st))]
                  #:when
                  (and (class-has-return-typed-methods? env (pred-class p))
                       (not (set-empty? (set-intersect (type-vars p) quant-set)))
                       (for/or ([a (in-list (pred-args p))]) (tvar? a))))
         p)]))
  (cond
    [(null? dict-bearing-preds)
     ;; Resolve method uses against the SAME functional-dependency
     ;; improvement `generalize` computed.  Otherwise a return-typed
     ;; method whose target type is fundep-determined (e.g. `mk-prod` in a
     ;; `proc` over an arrow whose product `p` is fixed by `cat -> p`) is
     ;; resolved against an unimproved tvar and reported as ambiguous.
     (define-values (generalized fd-sub st1) (generalize* env-for-gen final-ty st))
     (define st2 (resolve-method-uses st1 (subst-compose fd-sub s*) env))
     (values (env-extend-var env name generalized) st2)]
    [else
     (define dict-tvar-names
       (for/fold ([acc (seteq)]) ([p (in-list dict-bearing-preds)])
         (for/fold ([acc acc]) ([a (in-list (pred-args p))]
                                #:when (and (tvar? a)
                                            (set-member? quant-set
                                                         (tvar-name a))))
           (set-add acc (tvar-name a)))))
     (define skolem-subst
       (for/fold ([sub empty-subst]) ([tv (in-set dict-tvar-names)])
         (subst-extend sub tv
                       (tcon (string->symbol
                              (format "$skolem.~a" tv))))))
     (define needs-dict-reqs
       (for/list ([p (in-list dict-bearing-preds)])
         (cons (pred-class p) (pred-args p))))
     (define-values (sk-map arg-names)
       (build-dict-skolems needs-dict-reqs skolem-subst env))
     (define st-rr (record-recursive-dict-uses st expr name needs-dict-reqs s*))
     (define-values (generalized st1) (generalize env-for-gen final-ty st-rr))
     (define saved-skolems (current-dict-skolems))
     (current-dict-skolems sk-map)
     (define st2 (resolve-method-uses st1 (subst-compose skolem-subst s*) env))
     (current-dict-skolems saved-skolems)
     (define st3 (if (null? arg-names) st2
                     (st-table-put st2 'needs-dict-defs name arg-names)))
     (values (env-extend-var env name generalized) st3)]))

;; Single-form step exposed so the REPL kernel can call into handle-top-form
;; directly.  The REPL persists the inference state (fresh counter + pending
;; preds) across inputs by threading the `infer-state` it carries: each step
;; takes the prior st and returns the next.
(define (infer-program-step form env declared st)
  (cond
    ;; A `#:derive-superclasses` instance expands into several plain
    ;; instances; register each so the REPL behaves like batch mode.
    [(top:derive-instance? form)
     (for/fold ([e env] [d declared] [st st] #:result (values e d st))
               ([f (in-list (expand-derive-instances (list form) env))])
       (handle-top-form f e d st))]
    [else (handle-top-form form env declared st)]))

;; handle-top-form dispatches on the top-level form; each non-trivial arm is
;; its own `handle-<form>` helper.  Type resolution takes `env` explicitly
;; (resolve-type &c. run the Environment monad over the alias table), so no
;; ambient `current-aliases` setup is needed here anymore.
;; Returns (values env declared st).  Only the def and instance arms consume
;; the threaded state (fresh tvars + pending preds); the rest are pure and pass
;; st straight through.
(define (handle-top-form form env declared st)
  (define (pass thunk) (let-values ([(e d) (thunk)]) (values e d st)))
  (match form
    [(top:alias name params target-ast stx)
     (pass (lambda () (handle-alias-form name params target-ast env declared)))]
    [(top:dec name ty-ast _)
     (pass (lambda () (handle-dec-form name ty-ast env declared)))]
    [(top:def name expr stx)            (handle-def-form name expr stx env declared st)]
    [(top:class supers head methods stx)
     (pass (lambda () (handle-class-form supers head methods stx env declared)))]
    [(top:instance ctx head methods stx)
     (handle-instance-form ctx head methods stx env declared st)]
    [(top:require specs stx)
     (pass (lambda () (handle-require-form specs stx env declared)))]
    [(top:provide specs stx)            (pass (lambda () (handle-provide-form env declared)))]
    [(top:struct-fields struct-name field-names _)
     (pass (lambda () (handle-struct-fields-form struct-name field-names env declared)))]
    [(top:effect ename ops stx)
     (pass (lambda () (handle-effect-form ename ops stx env declared)))]
    [(top:data tname tparams ctors stx abstract? runtime-tag)
     (pass (lambda () (handle-data-form tname tparams ctors stx abstract? runtime-tag env declared)))]))

;; ----- per-top-form elaboration (the arms of handle-top-form) -------

(define (handle-alias-form name params target-ast env declared)
  (values (env-extend-alias env name params target-ast) declared))

(define (handle-dec-form name ty-ast env declared)
     (define sch (resolve-scheme ty-ast env))
     ;; Pre-register the name in env with its declared scheme so that
     ;; subsequent top-level forms can forward-reference it.  When the
     ;; matching define is processed later, the binding is replaced.
     (values (env-extend-var env name sch)
             (hash-set declared name sch)))

(define (handle-def-form name expr stx env declared st)
     (cond
       [(hash-has-key? declared name)
        (define decl-scheme (hash-ref declared name))
        ;; Detect needs-dict-body — a def whose qual context
        ;; introduces a return-typed-bearing constraint over a tvar.
        ;; Pre-allocate the dict-arg local names and a skolem map so
        ;; the body's polymorphic mempty/pure refs resolve to the
        ;; locals rather than to (nonexistent) per-skolem impls.
        (define needs-dict-reqs (var-dict-requirements env decl-scheme))
        (define-values (decl-ty decl-preds dict-skolems dict-arg-names)
          (cond
            [(null? needs-dict-reqs)
             (define-values (t p) (skolemize decl-scheme))
             (values t p (hasheq) '())]
            [else
             (define-values (t p s) (skolemize/tracked decl-scheme))
             (define-values (sk-map args) (build-dict-skolems needs-dict-reqs s env))
             (values t p sk-map args)]))
        ;; Pre-register with the FULL polymorphic scheme rather than the
        ;; skolemized monomorphic type, enabling polymorphic recursion:
        ;; the body may call itself at different instantiations.
        (define env-rec (env-extend-var env name decl-scheme))
        ;; The dict-skolem map must be visible to BOTH the body
        ;; inference AND the post-reduction `resolve-method-uses!`
        ;; pass that follows below; mutate the parameter directly
        ;; for the whole branch instead of wrapping just infer-expr.
        (define saved-skolems (current-dict-skolems))
        (current-dict-skolems dict-skolems)
        ;; When the body is a lambda and the declared
        ;; type has at least that many leading arrows, push the
        ;; SKOLEMIZED parameter types directly into the env so
        ;; the body inferences against rigid skolems rather than
        ;; fresh tvars.  This is what makes GADT pattern-match
        ;; refinement actually fire — without it, `eval :: (Expr a)
        ;; -> a` would type its parameter as a fresh tvar, and the
        ;; first arm's standard unify would pin `a` to that arm's
        ;; concrete result type, leaking to later arms.
        (define-values (s t st1)
          (cond
            [(and (e:lam? expr)
                  (decl-arrow-depth-ge? decl-ty (length (e:lam-params expr))))
             (define-values (arg-tys cod)
               (unfold-arrow decl-ty (length (e:lam-params expr))))
             (define env-lam
               (for/fold ([e env-rec])
                         ([p (in-list (e:lam-params expr))]
                          [ty (in-list arg-tys)])
                 (env-extend-var e p (scheme '() ty))))
             ;; Only seed the expected-type parameter
             ;; when the lambda body is DIRECTLY a `match` — that's
             ;; the GADT-elimination case where pushing the
             ;; declared codomain into result-tv unlocks local
             ;; skolem refinement.  For any other body shape, the
             ;; expected type would propagate too deep (across
             ;; do-blocks, nested lambdas, etc.) and pin a
             ;; subexpression's result to the wrong type.
             (define-values (s-body t-body st-b)
               (cond
                 [(e:match? (e:lam-body expr))
                  (parameterize ([current-expected-type cod])
                    (infer-expr (e:lam-body expr) env-lam st))]
                 [else (infer-expr (e:lam-body expr) env-lam st)]))
             (values s-body
                     (foldr make-arrow t-body
                            (for/list ([ty (in-list arg-tys)])
                              (apply-subst s-body ty)))
                     st-b)]
            [else (infer-expr expr env-rec st)]))
        ;; Normalize the inferred body type so any
        ;; associated-type applications resolve against the env's
        ;; registered instances before comparing to the declared
        ;; type.
        (define s-u
          (with-handlers
           ([exn:fail:unify?
             (lambda (_)
               (raise-syntax-error 'infer
                 (format "definition of ~s has the wrong type\n~a"
                         name
                         (expected/got-lines (format-scheme decl-scheme)
                                             (pretty-type (apply-subst s t))))
                 stx))])
           (unify (normalize-type env (apply-subst s t)) decl-ty)))
        ;; Discharge any constraints raised inside the body against the
        ;; declaration's preds (hypotheses).
        (define final-subst (subst-compose s-u s))
        (define st2 (st:apply-subst-to-preds st1 final-subst))
        (define remaining-preds
          (reduce-context env decl-preds (st:preds st2)))
        (when (not (null? remaining-preds))
          (raise-syntax-error 'infer
            (render-doc (labeled-block (format "unsolved constraints in ~a:" name)
                                       (map pretty-pred remaining-preds))
                        (current-type-columns))
            stx))
        (define st3 (st:set-preds st2 '()))
        (define st4 (resolve-method-uses st3 final-subst env))
        (define st5 (if (null? dict-arg-names) st4
                        (st-table-put st4 'needs-dict-defs name dict-arg-names)))
        (current-dict-skolems saved-skolems)
        (values (env-extend-var env name decl-scheme)
                (hash-remove declared name)
                st5)]
       [else
        ;; No declaration: pre-register fresh tvar for recursive use,
        ;; infer, unify, generalize.
        (define-values (α st1) (st:fresh st))
        (define env-rec (env-extend-var env name (scheme '() α)))
        (define-values (s t st2) (infer-expr expr env-rec st1))
        (define s-rec (unify (apply-subst s α) (apply-subst s t)))
        (define s* (subst-compose s-rec s))
        (define st3 (st:apply-subst-to-preds st2 s*))
        (define final-ty (apply-subst s* t))
        (define final-env (apply-subst/env s* env))
        ;; Detect inferred needs-dict: pending preds over a
        ;; return-typed-bearing class whose argument is a tvar that
        ;; will be quantified.  Without this, a polymorphic monadic
        ;; helper such as
        ;;   (define (madd mx my) (do [x <- mx] [y <- my] (pure (+ x y))))
        ;; raises "ambiguous use of pure" because `pure`'s class param
        ;; resolves to the to-be-quantified `m`.  Treat such defs the
        ;; same way as a declared (Monad m) => ... signature: skolemize
        ;; the relevant tvars, allocate dict-arg names, and resolve the
        ;; body's `pure`/`mempty` calls through the dict-skolems map.
        (define env-fv (env-vars-free-vars final-env))
        (define ty-fv  (type-vars final-ty))
        (define quant-set (set-subtract ty-fv env-fv))
        ;; Only enable the inferred needs-dict path for function
        ;; definitions (lambda RHS).  A bare value binding such as
        ;; `(define x mempty)` or `(define x (pure 5))` would
        ;; otherwise become a polymorphic dict-taking value — useful
        ;; in principle but surprising in practice, and tests pin the
        ;; current "rejected at compile time" behavior.  Users who
        ;; want a polymorphic value can ascribe a type with `:`.
        (define dict-bearing-preds
          (cond
            [(or (set-empty? quant-set) (not (e:lam? expr))) '()]
            [else
             (for/list ([p (in-list (st:preds st3))]
                        #:when
                        (and (class-has-return-typed-methods?
                              env (pred-class p))
                             (not (set-empty?
                                   (set-intersect (type-vars p) quant-set)))
                             (for/or ([a (in-list (pred-args p))])
                               (tvar? a))))
               p)]))
        (cond
          [(null? dict-bearing-preds)
           (define-values (generalized st4) (generalize final-env final-ty st3))
           (define st5 (resolve-method-uses st4 s* env))
           (values (env-extend-var final-env name generalized) declared st5)]
          [else
           ;; Build skolem subst: each tvar arg of a dict-bearing pred
           ;; that will be quantified gets a fresh skolem tcon.  These
           ;; names mirror `build-dict-skolems`' filter — non-tvar args
           ;; are passed through `apply-subst` to itself, so they stay
           ;; literal in the resulting types.
           (define dict-tvar-names
             (for/fold ([acc (seteq)]) ([p (in-list dict-bearing-preds)])
               (for/fold ([acc acc]) ([a (in-list (pred-args p))]
                                      #:when (and (tvar? a)
                                                  (set-member? quant-set
                                                               (tvar-name a))))
                 (set-add acc (tvar-name a)))))
           (define skolem-subst
             (for/fold ([sub empty-subst]) ([tv (in-set dict-tvar-names)])
               (subst-extend sub tv
                             (tcon (string->symbol
                                    (format "$skolem.~a" tv))))))
           (define needs-dict-reqs
             (for/list ([p (in-list dict-bearing-preds)])
               (cons (pred-class p) (pred-args p))))
           (define-values (sk-map arg-names)
             (build-dict-skolems needs-dict-reqs skolem-subst env))
           ;; Recursive calls inside the body had no dict-use recorded
           ;; (env-rec held `(scheme '() α)`), so retroactively record
           ;; them with the same reqs — they share the lambda's
           ;; locally-bound dict args via the skolem map.
           (define st-rr (record-recursive-dict-uses st3 expr name needs-dict-reqs s*))
           ;; Generalize (consumes preds from the pool).
           (define-values (generalized st4) (generalize final-env final-ty st-rr))
           ;; Resolve methods with the skolem map in scope so
           ;; pure/mempty land on the local dict-arg names rather than
           ;; per-tcon impls.
           (define saved-skolems (current-dict-skolems))
           (current-dict-skolems sk-map)
           (define st5 (resolve-method-uses st4 (subst-compose skolem-subst s*) env))
           (current-dict-skolems saved-skolems)
           (define st6 (if (null? arg-names) st5
                           (st-table-put st5 'needs-dict-defs name arg-names)))
           (values (env-extend-var final-env name generalized) declared st6)])]))

(define (handle-provide-form env declared)
     ;; `provide` is a packaging concern, not a typing concern: it
     ;; doesn't introduce any constraints or change the env.  The
     ;; elaborator resolves the spec list against the final env
     ;; after inference completes.
     (values env declared))

(define (handle-struct-fields-form struct-name field-names env declared)
     ;; Register the struct's ordered field-name list.
     ;; No code is emitted; the entry is consulted by inference
     ;; (and codegen) for `e:update` to resolve named fields to
     ;; the underlying Racket struct slots.
     (values (env-extend-struct-fields env struct-name field-names)
             declared))

(define (handle-effect-form ename ops stx env declared)
     ;; An effect declaration registers each operation
     ;; as a regular value with type `(-> argT ... resultT)`.
     ;; The effect itself is recorded in env so codegen can emit
     ;; a per-effect continuation-prompt-tag and so handle clauses
     ;; can verify the op belongs to a known effect.  A 0-arg op
     ;; is typed `(-> Unit resultT)` so it can be called as a
     ;; function — the surface `(op)` form auto-passes Unit.
     (define op-schemes
       (for/list ([o (in-list ops)])
         (define arg-tys (map (lambda (t) (resolve-type t env))
                              (effect-op-arg-types o)))
         (define res-ty  (resolve-type (effect-op-result-type o) env))
         (define normalized-arg-tys
           (cond
             [(null? arg-tys) (list (tcon 'Unit))]
             [else arg-tys]))
         (define body (foldr make-arrow res-ty normalized-arg-tys))
         (cons (effect-op-name o) (scheme '() body))))
     (define env*
       (for/fold ([e env]) ([os (in-list op-schemes)])
         (env-extend-var e (car os) (cdr os))))
     (values (env-extend-effect env* ename
                                (for/list ([o (in-list ops)])
                                  (effect-op-name o)))
             declared))

(define (handle-data-form tname tparams ctors stx abstract? runtime-tag env declared)
     (define default-result-type
       (make-tapp (tcon tname)
                  (for/list ([p (in-list tparams)]) (tvar p))))
     (define env*
       (env-extend-tcon env tname
                        (tcon-info tname (length tparams)
                                   (arity->star-kind (length tparams))
                                   (for/list ([c (in-list ctors)])
                                     (data-ctor-name c))
                                   abstract?
                                   runtime-tag)))
     (define env**
       (for/fold ([e env*]) ([c (in-list ctors)])
         (define field-tys
           (for/list ([t (in-list (data-ctor-field-types c))])
             (resolve-type t env)))
         ;; GADT ctors can specify their own result type
         ;; via `#:returns RT`.  Default to the data type's uniform
         ;; `(T tparams)` shape.
         (define ctor-result-type
           (cond
             [(data-ctor-result-type c)
              (resolve-type (data-ctor-result-type c) env)]
             [else default-result-type]))
         (define ctor-fn-type
           (foldr make-arrow ctor-result-type field-tys))
         ;; An existential ctor adds its own tvars and
         ;; constraints into the scheme.
         (define extra-tvars   (data-ctor-extra-tvars c))
         (define extra-context (data-ctor-extra-context c))
         (define qualified-body
           (cond
             [(null? extra-context) ctor-fn-type]
             [else
              (mqual (for/list ([cs (in-list extra-context)])
                       (resolve-constraint cs env))
                     ctor-fn-type)]))
         ;; For GADT ctors, the scheme's quantifiers are
         ;; the free type variables appearing in field-tys ++ result
         ;; (rather than always quantifying over the data type's
         ;; tparams).  This lets `Lit :: Integer -> Expr Integer`
         ;; have an empty quantifier list, and `If :: (Expr Bool)
         ;; -> Expr a -> Expr a -> Expr a` only quantify over `a`.
         (define quantifier-vars
           (cond
             [(data-ctor-result-type c)
              ;; GADT: union free vars of fields + result.
              (define ft-vars
                (apply set-union
                       (cons (seteq)
                             (for/list ([t (in-list field-tys)])
                               (type-vars t)))))
              (define rt-vars (type-vars ctor-result-type))
              (sort (set->list (set-union ft-vars rt-vars)) symbol<?)]
             [else (append tparams extra-tvars)]))
         (define sch (scheme quantifier-vars qualified-body))
         (env-extend-data e (data-ctor-name c)
                          (data-info tname (data-ctor-name c)
                                     (length field-tys) sch
                                     extra-tvars))))
     (values env** declared))

;; ----- class / instance elaboration --------------------------------

(define (handle-class-form supers head methods stx env declared)
  (define class-name (constraint-class head))
  ;; The head's args may be plain ty:var nodes or kind-annotated
  ;; ty:vars (parser stashes the kind on the stx as 'rackton:kind).
  (define class-args
    (for/list ([a (in-list (constraint-args head))]) (resolve-type a env)))
  (define class-params
    (for/list ([a (in-list class-args)])
      (match a
        [(tvar n) n]
        [_ (raise-syntax-error 'infer
              "class head arguments must be (kind-annotated) type variables"
              stx)])))
  ;; A parameter with no explicit `::` kind may still be higher-kinded by
  ;; virtue of a superclass: wherever `name` appears as a direct argument
  ;; of a super-constraint `(C … name …)`, it occupies the corresponding
  ;; parameter of `C` and inherits that parameter's (already-core) kind.
  ;; This covers both the bound's subject — `[w => Functor]` ⇒ `(Functor
  ;; w)`, giving `w` the kind `* -> *` (the bound's last position) — and a
  ;; bare parameter mentioned earlier in another bound — `[g => (Pairing
  ;; f)]` ⇒ `(Pairing f g)`, giving the bare `f` the kind of `Pairing`'s
  ;; first parameter.  Superclasses are always declared before their
  ;; subclasses, so the lookup is resolved in `env`.
  (define (kind-from-supers name)
    (for/or ([s (in-list supers)])
      (define cinfo (env-ref-class env (constraint-class s) #f))
      (and cinfo
           (for/or ([a (in-list (constraint-args s))]
                    [p (in-list (class-info-params cinfo))])
             (and (ty:var? a)
                  (eq? (ty:var-name a) name)
                  (hash-ref (class-info-kinds cinfo) p #f))))))
  (define class-kinds
    (for/fold ([acc (hasheq)])
              ([raw (in-list (constraint-args head))]
               [name (in-list class-params)])
      (match raw
        [(ty:var _ var-stx)
         (define surface-kind
           (and (syntax? var-stx)
                (syntax-property var-stx 'rackton:kind)))
         (define kind
           (cond
             [surface-kind (surface-kind->core surface-kind)]
             [(kind-from-supers name) => values]
             [else (surface-kind->core (k:star))]))
         (hash-set acc name kind)]
        [_ (hash-set acc name (kind-star))])))
  (define super-preds
    (for/list ([s (in-list supers)]) (resolve-constraint s env)))
  (define head-pred (pred class-name class-args))
  (define method-schemes
    (for/fold ([acc (hasheq)]) ([m (in-list methods)])
      (cond
        [(method-sig? m)
         (define raw (resolve-type (method-sig-type m) env))
         (define body (mqual (list head-pred) raw))
         ;; Quantify over EVERY type variable that appears in the
         ;; method type (including method-local ones like `a`/`b`
         ;; in `(flatmap : (a -> m b) -> (m a) -> m b)`).  Class params
         ;; come first by convention.
         (define body-vars (type-vars body))
         (define extra-vars
           (sort (set->list
                  (set-subtract body-vars (list->seteq class-params)))
                 symbol<?))
         (define quantified (append class-params extra-vars))
         (define sch (scheme quantified body))
         (hash-set acc (method-sig-name m) sch)]
        [else acc])))
  (define defaults
    (for/fold ([acc (hasheq)]) ([m (in-list methods)])
      (cond
        [(method-default? m)
         (hash-set acc (method-default-name m) (method-default-expr m))]
        [else acc])))
  ;; For each method, find which argument's runtime tag determines the
  ;; instance: the first top-level arg whose type mentions a class
  ;; parameter.  If no argument mentions a class param but the *return*
  ;; type does (e.g. `pure :: a -> f a`), mark the method as
  ;; return-typed — at use sites it is resolved at compile time from the
  ;; expected return type rather than from a runtime value tag.
  ;; Compute fundeps first so dispatchpos can consult them — class
  ;; methods of fundep-bearing classes skip args whose type is wholly
  ;; "determined" by the fundep (no instance-disambiguation value).
  (define fundeps
    (for/list ([m (in-list methods)] #:when (class-fundep? m))
      (cons (class-fundep-lhs m) (class-fundep-rhs m))))
  (define dispatchpos
    (for/fold ([acc (hasheq)])
              ([(method-name sch) (in-hash method-schemes)])
      ;; Peel ALL qual layers from the body type so methods
      ;; with their own qual context (e.g. `traverse :: (Applicative
      ;; f) => ...`) still get their arrow examined for positional
      ;; dispatch.  Previously qual-body-type peeled only one layer
      ;; and `traverse`'s (Applicative f) qual hid the arrow,
      ;; causing find-dispatch-pos to return #f and the method to be
      ;; classified as 'return.
      (define body-type (qual-body-deep (scheme-body sch)))
      (define pos (find-dispatch-pos body-type class-params fundeps))
      (cond
        [pos (hash-set acc method-name pos)]
        [(return-type-mentions-class-param? body-type class-params)
         (hash-set acc method-name 'return)]
        [else
         (raise-syntax-error 'infer
           (format "class method ~s does not have any argument whose type mentions a class parameter — single dispatch cannot resolve it"
                   method-name)
           stx)])))
  ;; Compute per-method dict requirements — for each method, the list
  ;; of (class-name . param-names) entries whose return-typed methods
  ;; must be inserted as extra leading arguments at call sites.  See
  ;; the docstring on class-info in env.rkt.
  (define dictreqs
    (for/fold ([acc (hasheq)])
              ([(method-name sch) (in-hash method-schemes)])
      (define reqs (method-dict-requirements sch class-params))
      (cond
        [(null? reqs) acc]
        [else (hash-set acc method-name reqs)])))
  ;; Collect declared associated-type names so each
  ;; instance can be checked for matching #:type bindings.
  (define type-families
    (for/list ([m (in-list methods)] #:when (class-type-fam? m))
      (class-type-fam-name m)))
  ;; Cross-class derivation table: superclass-name → (method-name → expr),
  ;; built from each `[Super (define …) …]` clause in the body's `#:derive` list.
  (define super-derives
    (for/fold ([acc (hasheq)]) ([m (in-list methods)] #:when (class-super-derive? m))
      (define inner
        (for/fold ([h (hasheq)]) ([d (in-list (class-super-derive-methods m))])
          (hash-set h (method-default-name d) (method-default-expr d))))
      (hash-set acc (class-super-derive-super m) inner)))
  (define info (class-info class-name class-params class-kinds
                           super-preds method-schemes defaults
                           dispatchpos fundeps dictreqs
                           type-families super-derives))
  ;; When a class is redeclared, its previously-registered
  ;; instances belong to a now-superseded class.  Clear them out so
  ;; the new declaration starts fresh — without this the duplicate-
  ;; instance check would fire for `(== Eq Integer)` re-registrations,
  ;; and env-class-has-overlap? would spuriously trigger.
  (define env*
    (cond
      [(env-ref-class env class-name #f)
       (env-clear-instances env class-name)]
      [else env]))
  (values (env-extend-class env* class-name info) declared))

;; Process a (require "file.rkt" …) form inside a rackton block.
;; For each spec, attempt to load the corresponding (submod spec
;; rackton-schemes) module and read the exported `rackton-bindings`
;; association list, decoding it back into schemes and extending env.
;; Specs that don't carry a rackton-schemes submodule (e.g. requires
;; of plain racket libraries) are silently skipped — the user can
;; still bring those in for runtime use, but they won't be type-checked.
(define (handle-require-form specs stx env declared)
  (define new-env
    (for/fold ([e env]) ([spec-stx (in-list specs)])
      (define submod-spec (require-spec->submod-spec spec-stx))
      (cond
        [(not submod-spec) e]
        [else
         ;; The catch-all `with-handlers` around the
         ;; dynamic-requires must NOT swallow coherence errors —
         ;; only the "this isn't a rackton module" case where the
         ;; sidecar's rackton-bindings binding isn't found.  Scope
         ;; the recovery narrowly to the bindings lookup; once we
         ;; know it IS a rackton module, let later errors (like
         ;; coherence violations) propagate.
         (define bindings
           (with-handlers ([exn:fail? (lambda (_) #f)])
             (dynamic-require submod-spec 'rackton-bindings)))
         (cond
          [(not bindings) e]
          [else
           (define data-ctors
             (with-handlers ([exn:fail? (lambda (_) '())])
               (dynamic-require submod-spec 'rackton-data-ctors)))
           (define tcons
             (with-handlers ([exn:fail? (lambda (_) '())])
               (dynamic-require submod-spec 'rackton-tcons)))
           (define classes
             (with-handlers ([exn:fail? (lambda (_) '())])
               (dynamic-require submod-spec 'rackton-classes)))
           (define instances
             (with-handlers ([exn:fail? (lambda (_) '())])
               (dynamic-require submod-spec 'rackton-instances)))
           (define e1
             (for/fold ([acc e]) ([entry (in-list bindings)])
               (env-extend-var acc (car entry)
                               (sexp->scheme (cdr entry)))))
           (define e2
             (for/fold ([acc e1]) ([entry (in-list data-ctors)])
               (env-extend-data acc (car entry)
                                (decode-data-info (cdr entry)))))
           (define e3
             (for/fold ([acc e2]) ([entry (in-list tcons)])
               (env-extend-tcon acc (car entry)
                                (decode-tcon-info (cdr entry)))))
           (define e4
             (for/fold ([acc e3]) ([entry (in-list classes)])
               (env-extend-class acc (car entry)
                                 (decode-class-info (cdr entry)))))
           (for/fold ([acc e4]) ([entry (in-list instances)])
             (define decoded (decode-instance-info entry))
             (define class-name (car decoded))
             (define new-inst (cdr decoded))
             ;; Module-level coherence.  An imported instance whose head
             ;; is alpha-equivalent to one already in scope is either:
             ;;   - the SAME instance reaching us by a second import path
             ;;     (a diamond) — detected by equal, non-#f origins — in
             ;;     which case we dedup (skip the re-add); or
             ;;   - a genuinely DIFFERENT instance with the same head
             ;;     (different or unknown origin) — a real conflict, which
             ;;     stays a hard error.
             (define dup-status
               (for/or ([existing (in-list (env-instances acc class-name))])
                 (and (instance-heads-equivalent?
                       (instance-info-head existing)
                       (instance-info-head new-inst))
                      (let ([eo (instance-info-origin existing)]
                            [no (instance-info-origin new-inst)])
                        (cond
                          [(and eo no (equal? eo no)) 'same]
                          [else
                           (raise-syntax-error 'require
                             (format "instance coherence: ~s would conflict with an instance already in scope"
                                     (pred->datum (instance-info-head new-inst)))
                             stx)])))))
             (cond
               [(eq? dup-status 'same) acc]   ;; benign diamond — dedup
               [else (env-extend-instance acc class-name new-inst)]))])])))
  (values new-env declared))

;; Resolve a require spec syntax to a usable `(submod ... rackton-schemes)`
;; module path.  Relative-path strings are interpreted relative to the
;; source file of the spec itself.
(define (require-spec->submod-spec spec-stx)
  (define spec-datum (syntax->datum spec-stx))
  (define src (syntax-source spec-stx))
  (cond
    [(and (string? spec-datum) (path-string? spec-datum) src)
     (define caller-dir
       (let-values ([(base _name _dir?) (split-path src)])
         base))
     (define full (path->complete-path spec-datum caller-dir))
     `(submod (file ,(path->string full)) rackton-schemes)]
    [(symbol? spec-datum)
     `(submod ,spec-datum rackton-schemes)]
    [else #f]))

(define (surface-kind->core k)
  (match k
    [(k:star)      (kind-star)]
    [(k:arr d c)   (kind-arr (surface-kind->core d) (surface-kind->core c))]
    [_             (kind-star)]))

;; Walk the arrow chain of a method type and return the position of
;; the first argument whose type mentions a class-param other than
;; "determined" ones (params on the RHS of a fundep).  Why this
;; refinement: for a class like `(MonadState s m | m -> s)`, the `s`
;; in `put :: s -> m Unit` is fundep-determined by `m` — dispatching
;; on the runtime value of `s` (e.g. an Integer) doesn't pick the
;; right instance, because multiple `m`s share the same `s`.  Skip
;; those args; the method falls through to return-typed dispatch.
;; For single-param classes (no fundeps), the determined set is
;; empty and the behaviour is unchanged.
;; A class parameter `p` is *dispatchable* in an argument type `dom`
;; only when the runtime value of `dom` carries `p`'s identity as its
;; head constructor — i.e. `dom` is `p` itself (a bare type variable), or
;; `dom` is an application whose spine head is `p` (like `f` in `(f a)`).
;; A `p` that merely appears as a NESTED argument of some other
;; constructor — e.g. `a` in `(Ptr a)` — is a phantom there: the runtime
;; value (a raw pointer) does not tag its element type, so it cannot
;; drive single dispatch.  Such a method falls through to return-typed
;; resolution.  (This is what lets Storable's `peek :: Ptr a -> IO a` be
;; return-typed while `poke :: Ptr a -> a -> IO ()` dispatches on its
;; bare `a` value argument rather than the opaque pointer.)
(define (type-spine-head d)
  (if (tapp? d) (type-spine-head (tapp-head d)) d))

(define (param-dispatchable-in? p d)
  (cond
    [(and (tvar? d) (eq? (tvar-name d) p)) #t]
    [(tapp? d) (let ([h (type-spine-head d)])
                 (and (tvar? h) (eq? (tvar-name h) p)))]
    [else #f]))

(define (find-dispatch-pos t class-params [fundeps '()])
  (define determined
    (for/fold ([acc (seteq)]) ([fd (in-list fundeps)])
      (set-union acc (list->seteq (cdr fd)))))
  (let loop ([t t] [pos 0])
    (cond
      [(arrow? t)
       (define dom (arrow-dom t))
       (define mentions
         (filter (lambda (p) (param-dispatchable-in? p dom)) class-params))
       (cond
         [(and (not (null? mentions))
               (not (andmap (lambda (p) (set-member? determined p))
                            mentions)))
          pos]
         [else (loop (arrow-cod t) (add1 pos))])]
      [else #f])))

;; True when the return position of a (possibly curried) method type
;; mentions any of `class-params`.  Used to flag methods like
;; `pure :: a -> f a` that need return-typed dispatch.
(define (return-type-mentions-class-param? t class-params)
  (define ret (let loop ([t t])
                (cond [(arrow? t) (loop (arrow-cod t))]
                      [else t])))
  (ormap (lambda (p) (set-member? (type-vars ret) p)) class-params))

;; Walk a possibly-qualified method scheme and collect the class
;; constraints whose arguments introduce additional type variables
;; that appear in the method body (and aren't already class parameters
;; of the declaring class).  Each entry is `(cons class-name
;; param-name-list)`; the param names will be looked up at use sites
;; against the freshened scheme substitution to recover the tvars that
;; carry the dict resolution.  Example: `traverse : (Applicative f) =>
;; (-> ...)` returns `((Applicative f))` — the dict requirement is
;; `Applicative` with parameter `f`.
;; Free-function counterpart to `method-dict-requirements`.  Walks the
;; scheme's qualifying context and reports any constraint whose class
;; declares return-typed methods — the function's call sites will need
;; resolved impls (e.g. `$mempty:Sum`) prepended.  Returns the same
;; `(Listof (cons class-name param-name-list))` shape as the method
;; variant so `record-dict-use!` can consume either.
(define (var-dict-requirements env sch)
  (define constraints (qual-constraints-of (scheme-body sch)))
  (for/list ([c (in-list constraints)]
             #:when (class-has-return-typed-methods? env (pred-class c)))
    (cons (pred-class c) (pred-args c))))

(define (class-has-return-typed-methods? env class-name)
  ;; A class is "dict-passable" if it directly owns a return-typed
  ;; method (per its class-info-dispatchpos) OR if its name shows up
  ;; in the hard-coded registry that encodes superclass closures
  ;; for the built-in prelude classes (Applicative/Monad/Monoid).
  (or (not (null? (dict-class-return-methods class-name)))
      (let ([cinfo (env-ref-class env class-name)])
        (and cinfo
             (for/or ([dp (in-hash-values (class-info-dispatchpos cinfo))])
               (eq? dp 'return))))))

(define (method-dict-requirements sch class-params)
  (define body (scheme-body sch))
  (define constraints (qual-constraints-of body))
  (define declaring-set (list->seteq class-params))
  (for/list ([c (in-list constraints)]
             #:unless (subset? (constraint-param-set c) declaring-set))
    (cons (pred-class c) (pred-args c))))

(define (constraint-param-set c)
  (apply set-union/empty (map type-vars (pred-args c))))

(define (constraint-tvar-names c)
  (for/list ([a (in-list (pred-args c))]
             #:when (tvar? a))
    (tvar-name a)))

(define (qual-constraints-of t)
  ;; Unwrap any number of nested `qual` layers, accumulating
  ;; constraints into a single list.  `mqual` doesn't flatten, so
  ;; user-written method types with their own qualifiers produce a
  ;; qual-of-qual that we need to walk.
  (cond
    [(qual? t)
     (append (qual-constraints t)
             (qual-constraints-of (qual-body t)))]
    [else '()]))

(define (set-union/empty . sets)
  (cond
    [(null? sets) (seteq)]
    [else (apply set-union sets)]))

;; True when `name` is a class method whose dispatch position is
;; flagged as 'return — i.e. resolved at the call site from the
;; expected type rather than from a runtime value tag.
(define (return-typed-method? name env)
  (define owner (env-ref-method-class env name))
  (cond
    [(not owner) #f]
    [else
     (define cinfo (env-ref-class env owner))
     (eq? (hash-ref (class-info-dispatchpos cinfo) name #f) 'return)]))

;; Stash a return-typed-method entry under `stx`.  The entry shape is
;; `(list 'return method-name class-param-tvars method-dict-entries)`.
;; method-dict-entries (usually empty) holds the method's OWN qual
;; context, for return-typed methods that also carry a method-level
;; dict requirement (e.g. MonadTrans.lift's `(Monad m) =>`) — so the
;; single entry resolves both the dispatch impl and the dict args
;; without the two recordings clobbering each other on `stx`.
;; (Entry-recording now lives in st:record-* / m:record-* near the foundation;
;; these comments document the entry shapes those produce.)

;; A positional class-method call on a class that has at
;; least one needs-dict instance.  Entry shape:
;;   (list 'inst-dispatch method-name class-param-tvars)
;; After the enclosing def's constraints reduce, the resolver applies
;; the final substitution, looks up the matching instance, and — if
;; that instance has dict-bearing constraints in its qual — writes
;; the per-instance impl name into current-method-resolutions and the
;; dict impls into current-method-dict-resolutions.
;; Does this class have at least one instance whose qual context
;; mentions a return-typed-bearing class?  Used to decide whether to
;; record positional class-method call sites for inst-dispatch.
(define (class-has-needs-dict-instances? env class-name)
  (for/or ([inst (in-list (env-instances env class-name))])
    (instance-needs-dict? env inst)))

(define (instance-needs-dict? env inst)
  ;; An instance is genuinely needs-dict (in the sense
  ;; that compile-instance emits a `(define $method:Tcon impl)`
  ;; with dict-arg lambda params) only when the qual context's
  ;; return-typed-bearing constraints involve tvars.  If the qual
  ;; is fully concrete (e.g. `(Concurrent IO) :- (Monad IO)` —
  ;; concrete IO), the body resolves to global impl names directly
  ;; and runtime-table registration is used.
  (for/or ([c (in-list (instance-info-context inst))])
    (and (class-has-return-typed-methods? env (pred-class c))
         (for/or ([a (in-list (pred-args c))])
           (not (set-empty? (type-vars a)))))))

;; Stash a dict-requiring-method entry under `stx`.  The
;; entry shape is `(list 'dict method-name dict-entries)` where each
;; dict-entry is `(cons class-name tvars)` — the class to look up and
;; the fresh tvars whose final resolution names the impl.

;; Walk an AST expression looking for `e:var` references to `name`
;; and `record-dict-use!` each one with the supplied `reqs` / `sub`.
;; Used when a non-declared def turns out to be needs-dict: recursive
;; references inside the body weren't recorded during inference (the
;; recursive scheme had no qual), and codegen will prepend dict-args
;; to the lambda — so the body's calls to itself must pass those
;; dict-args.  Tracks shadowing of `name` introduced by inner
;; binders so we don't capture references that resolve elsewhere.
(define (record-recursive-dict-uses st expr name reqs sub)
  (let walk ([e expr] [shadowed? #f] [st st])
    (cond
      [shadowed? st]
      [else
       (match e
         [(e:literal _ _) st]
         [(e:var n stx)
          (if (eq? n name) (st:record-dict-use st stx name reqs sub) st)]
         [(e:lam params body _)
          (walk body (or shadowed? (and (memq name params) #t)) st)]
         [(e:app head args _)
          (for/fold ([st (walk head shadowed? st)]) ([a (in-list args)])
            (walk a shadowed? st))]
         [(e:let bindings body _)
          (define st1 (for/fold ([st st]) ([b (in-list bindings)]) (walk (cdr b) shadowed? st)))
          (define new-shadowed?
            (or shadowed? (for/or ([b (in-list bindings)]) (eq? (car b) name))))
          (walk body new-shadowed? st1)]
         [(e:letrec bindings body _)
          (define new-shadowed?
            (or shadowed? (for/or ([b (in-list bindings)]) (eq? (car b) name))))
          (define st1 (for/fold ([st st]) ([b (in-list bindings)]) (walk (cdr b) new-shadowed? st)))
          (walk body new-shadowed? st1)]
         [(e:if c th el _)
          (walk el shadowed? (walk th shadowed? (walk c shadowed? st)))]
         [(e:ann body _ _) (walk body shadowed? st)]
         [(e:match scrut clauses _ _)
          (for/fold ([st (walk scrut shadowed? st)]) ([cl (in-list clauses)])
            (define sh? (or shadowed? (pattern-binds-name? (clause-pattern cl) name)))
            (define st1 (if (clause-guard cl) (walk (clause-guard cl) sh? st) st))
            (walk (clause-body cl) sh? st1))]
         [(e:match* scrutinees clauses _ _)
          (define st-s (for/fold ([st st]) ([sc (in-list scrutinees)]) (walk sc shadowed? st)))
          (for/fold ([st st-s]) ([cl (in-list clauses)])
            (define sh?
              (or shadowed?
                  (for/or ([p (in-list (clause*-patterns cl))]) (pattern-binds-name? p name))))
            (define st1 (if (clause*-guard cl) (walk (clause*-guard cl) sh? st) st))
            (walk (clause*-body cl) sh? st1))]
         [(e:update record updates _)
          (for/fold ([st (walk record shadowed? st)]) ([u (in-list updates)])
            (walk (cdr u) shadowed? st))]
         [(e:handle expr clauses ret _)
          (define st-e (walk expr shadowed? st))
          (define st-r (if ret (walk ret shadowed? st-e) st-e))
          (for/fold ([st st-r]) ([cl (in-list clauses)])
            (define sh? (or shadowed?
                            (and (memq name (handle-clause-params cl)) #t)
                            (eq? (handle-clause-k-name cl) name)))
            (walk (handle-clause-body cl) sh? st))]
         [(e:escape _ _ _ _) st]
         [_ st])])))

(define (pattern-binds-name? p name)
  (match p
    [(p:var n _) (eq? n name)]
    [(p:ctor _ args _)
     (for/or ([a (in-list args)]) (pattern-binds-name? a name))]
    [_ #f]))

;; After a top-level def's constraints have been reduced, walk every
;; recorded method use, apply `final-subst` to each entry's tvars,
;; extract resulting type-constructor names, and graduate the entry
;; into the appropriate resolution table:
;;   * `'return` entries land in `current-method-resolutions` as a
;;     single impl-name symbol the codegen substitutes for the e:var.
;;   * `'dict`   entries land in `current-method-dict-resolutions` as
;;     a list of impl-name symbols the codegen prepends to e:app args.
;; Resolve a list of dict-entries — `(cons class-name arg-types)` — into
;; the impl names to prepend at a call site.  Expands each constraint
;; over its superclass closure, dedups by method, drops fully-concrete
;; pairs (resolved to per-type globals directly), and resolves the rest.
;; Shared by the 'dict and 'return method-use branches.
(define (resolve-dict-entries dict-entries final-subst stx env)
  (define all-pairs
    (apply append
           (for/list ([entry (in-list dict-entries)])
             (collect-dict-method-args (car entry) (cdr entry) env))))
  (define dedup-pairs
    (let loop ([ps all-pairs] [seen (seteq)] [acc '()])
      (cond
        [(null? ps) (reverse acc)]
        [(set-member? seen (car (car ps))) (loop (cdr ps) seen acc)]
        [else (loop (cdr ps) (set-add seen (car (car ps))) (cons (car ps) acc))])))
  (define active-pairs
    (filter (lambda (pair) (pair-has-tvar-at-undetermined-position? pair env))
            dedup-pairs))
  (for/list ([pair (in-list active-pairs)])
    (resolve-impl-with-quals (car pair) (cdr pair) final-subst stx env)))

;; State transition: read the 'method-uses channel from st, resolve each entry
;; into local mutable resolution tables (seeded from st's current ones), then
;; store immutable snapshots back into st and clear 'method-uses.  The local
;; mutation is an isolated implementation detail — the function is pure st->st.
(define (resolve-method-uses st final-subst env)
  (define uses (st-table st 'method-uses))
  (define resolutions (hash-copy (st-table st 'method-resolutions)))
  (define dict-resolutions (hash-copy (st-table st 'method-dict-resolutions)))
  ;; The monomorphization log: a newest-first list accumulated in a local box
  ;; (isolated, frozen back into st below — same shape as the resolution copies).
  (define mono-box (box (st-table st 'monomorphized-sites)))
  (for ([(stx entry) (in-hash uses)])
      (match entry
        [(list 'return method-name class-param-tvars method-dict-entries)
         (define impl (resolve-return-impl method-name class-param-tvars
                                           final-subst stx env))
         (hash-set! resolutions stx impl)
         ;; Dict args the impl needs prepended at the call site, from two
         ;; sources: the matching INSTANCE's qual context (return-typed-
         ;; bearing constraints), and the METHOD's own qual context
         ;; (e.g. MonadTrans.lift's `(Monad m) =>`).  A return-typed
         ;; method can have BOTH a dispatch on its result AND a
         ;; method-level dict requirement (lift is the first such).
         (define inst-dict-impls
           (instance-qual-return-impls env method-name
                                       class-param-tvars final-subst stx))
         (define method-dict-impls
           (resolve-dict-entries method-dict-entries final-subst stx env))
         (define all-dict-impls (append inst-dict-impls method-dict-impls))
         (unless (null? all-dict-impls)
           (hash-set! dict-resolutions stx all-dict-impls))]
        [(list 'dict method-name dict-entries)
         ;; For each constraint, expand into (method . arg-types) pairs
         ;; following the class's superclass hierarchy so inherited
         ;; methods (e.g. `pure` reached from MonadState via Monad) see
         ;; only their own class's arg slots.  Each impl is wrapped with
         ;; its instance-qual dicts so needs-dict transformer
         ;; instances pre-apply their inner-monad dicts at the call site.
         ;; Pairs are deduped across constraints (e.g. MonadState +
         ;; MonadEnv both reaching Monad.pure) — matches
         ;; build-dict-skolems' (skolem.method) dedup used to size the
         ;; needs-dict lambda's params.  Pairs whose class-params are
         ;; fully concrete (after fundep filter) at this call site are
         ;; dropped: the function's body resolves those references to
         ;; per-type globals (e.g. `$mempty:String`) directly, with no
         ;; corresponding dict-arg slot.
         (hash-set! dict-resolutions stx
                    (resolve-dict-entries dict-entries final-subst stx env))]
        [(list 'inst-dispatch method-name class-param-tvars)
         ;; Route a class-method call to a per-instance
         ;; impl if the matching instance is needs-dict; otherwise
         ;; fall through silently to the runtime dispatch wrapper.
         (define resolved-types
           (for/list ([tv (in-list class-param-tvars)])
             (apply-subst final-subst tv)))
         (define tcon-names
           (for/list ([rt (in-list resolved-types)])
             (type-head-tcon rt)))
         (when (andmap values tcon-names)
           (define class-name (env-ref-method-class env method-name))
           (define cinfo (and class-name (env-ref-class env class-name)))
           (define target-pred (pred class-name resolved-types))
           (define matching-inst
             (find-matching-instance env class-name target-pred))
           (define matching
             (and matching-inst
                  (cons matching-inst
                        (match-pred (instance-info-head matching-inst)
                                    target-pred))))
           ;; Filter tcon-names by fundep-determined params
           ;; — matches the impl name compile-instance emits and what
           ;; the 'return-typed resolver uses.
           (define keep-tcon-names
             (cond
               [(or (not cinfo) (null? (class-info-fundeps cinfo)))
                tcon-names]
               [else
                (define determined
                  (for/fold ([acc (seteq)])
                            ([fd (in-list (class-info-fundeps cinfo))])
                    (set-union acc (list->seteq (cdr fd)))))
                (for/list ([p (in-list (class-info-params cinfo))]
                           [tn (in-list tcon-names)]
                           #:unless (set-member? determined p))
                  tn)]))
           (cond
             [(and matching (instance-needs-dict? env (car matching)))
              (define impl
                (return-impl-symbol method-name keep-tcon-names))
              (hash-set! resolutions stx impl)
              (define inst-dict-impls
                (instance-qual-return-impls env method-name
                                            class-param-tvars final-subst stx))
              (unless (null? inst-dict-impls)
                (hash-set! dict-resolutions stx inst-dict-impls))]
             ;; For overlap-group classes, emit a deep-
             ;; fingerprint impl name from the MATCHED instance's
             ;; head (not the call site's type) so a generic
             ;; instance `(Show (Box a))` resolves to `$show:Box_*`
             ;; and the specific `(Show (Box Integer))` resolves to
             ;; `$show:Box_Integer`.
             [(and matching (env-class-has-overlap? env class-name))
              (define inst-head-args
                (pred-args (instance-info-head (car matching))))
              (define impl
                (overlap-impl-symbol method-name inst-head-args))
              (hash-set! resolutions stx impl)]
             ;; Regular positional method call whose
             ;; dispatch type is now concrete — emit a direct call
             ;; to the named per-instance impl that compile-instance
             ;; emits.  Same naming scheme as the needs-dict path so
             ;; (== Integer Integer) resolves to $==:Integer.
             ;; Skip prelude-style instances whose bodies are
             ;; `(racket ...)` escapes — those have no real named
             ;; impl and still rely on the runtime dispatch table.
             [(and matching
                   (instance-has-monomorphizable-impl?
                    (car matching) method-name))
              (define impl
                (return-impl-symbol method-name keep-tcon-names))
              (hash-set! resolutions stx impl)
              (set-box! mono-box (cons (cons method-name impl) (unbox mono-box)))]))]))
  (st-table-set
   (st-table-set (st-table-set (st-table-set st 'method-resolutions (freeze-eq resolutions))
                               'method-dict-resolutions (freeze-eq dict-resolutions))
                 'method-uses (hasheq))
   'monomorphized-sites (unbox mono-box)))

;; Does this instance have a real, named compile-emitted
;; impl for the given method?  Two signals say "no":
;;   - the body is a `(racket τ (vars) ...)` escape with no real
;;     Rackton-level definition (prelude placeholder convention);
;;   - the instance was registered as part of the prelude env build
;;     (compile-instance was never invoked, so no `$method:Tcon`
;;     global got emitted).
;; Both correspond to runtime-only impls that monomorphization must
;; not redirect to.
(define (instance-has-monomorphizable-impl? inst method-name)
  (define body (hash-ref (instance-info-methods inst) method-name #f))
  (cond
    [(not body) #f]
    [(e:escape? body) #f]
    [(and (e:lam? body) (e:escape? (e:lam-body body))) #f]
    [(prelude-instance? inst) #f]
    [else #t]))

;; An instance that was registered during prelude-env
;; construction is "prelude" — no compile-instance ran for it.
;; The runtime-only flag is recorded on the instance-info struct;
;; handle-instance-form sets it based on `current-prelude-build?`.
(define current-prelude-build? (make-parameter #f))

;; When #t, re-declaring an instance whose head is α-equivalent to an
;; existing one REPLACES it instead of raising the "duplicate instance"
;; coherence error.  The REPL sets this so a session can iterate on an
;; instance; module compilation leaves it #f, keeping coherence.
(define current-allow-instance-redefinition? (make-parameter #f))

(define (prelude-instance? inst)
  ;; Intrinsic to the struct, set at construction by handle-instance-form when
  ;; current-prelude-build? was #t (no module-level side table).
  (instance-info-prelude? inst))

;; overlap-impl-symbol and head-fingerprint — the instance impl-name
;; contract shared with codegen — live in "impl-symbols.rkt".

;; Predicate matching build-dict-skolems' filter-skolems —
;; a `(method . arg-types)` pair contributes a dict slot only when
;; at least one arg at a non-fundep-determined position is a tvar.
;; Class-params fully concrete in the scheme's qual (e.g. the `String`
;; in `(MonadWriter String m) =>`) resolve to per-type globals in the
;; body directly, without a dict.
(define (pair-has-tvar-at-undetermined-position? pair env)
  (define method-name (car pair))
  (define arg-types   (cdr pair))
  (define owner-class (and env (env-ref-method-class env method-name)))
  (define cinfo       (and owner-class (env-ref-class env owner-class)))
  (define determined
    (cond
      [(and cinfo (not (null? (class-info-fundeps cinfo))))
       (for/fold ([acc (seteq)])
                 ([fd (in-list (class-info-fundeps cinfo))])
         (set-union acc (list->seteq (cdr fd))))]
      [else (seteq)]))
  (define params
    (cond
      [cinfo (class-info-params cinfo)]
      [else (build-list (length arg-types) (lambda (i) #f))]))
  (cond
    [(or (not cinfo) (not (= (length params) (length arg-types))))
     ;; Conservative: keep the pair if we can't reason about it.
     #t]
    [else
     (for/or ([p (in-list params)] [a (in-list arg-types)]
              #:unless (set-member? determined p))
       (or (tvar? a) (symbol? a)))]))

;; Resolve a method-name + arg-types to either a bare impl
;; symbol or an s-expression `(impl-name dict-args...)` that the
;; codegen splices into the dict-prepend.  Used by the 'dict
;; resolution path, where each impl passed to a needs-dict function
;; may itself reference a needs-dict instance (e.g. $get-st:StateT
;; takes an inner-pure dict from the (Monad m) qual).
(define (resolve-impl-with-quals method-name arg-types final-subst stx env)
  (define base (resolve-return-impl method-name arg-types final-subst stx env))
  (define qual-impls
    (instance-qual-return-impls env method-name arg-types final-subst stx))
  (cond
    [(null? qual-impls) base]
    [else (cons base qual-impls)]))

;; Walk the matching instance for a return-typed method's resolved
;; class param types; emit impl names for return-typed-bearing
;; constraints in the instance's qual context.  Returns a (possibly
;; empty) list of impl-name symbols, suitable for prepending to the
;; call site's argument list.
(define (instance-qual-return-impls env method-name class-param-tvars
                                    final-subst stx)
  (define owner-class (env-ref-method-class env method-name))
  (cond
    [(not owner-class) '()]
    [else
     (define resolved-types
       (for/list ([tv (in-list class-param-tvars)])
         (apply-subst final-subst tv)))
     (define target-pred (pred owner-class resolved-types))
     (define matching-inst
       (find-matching-instance env owner-class target-pred))
     (define matching
       (and matching-inst
            (cons matching-inst
                  (match-pred (instance-info-head matching-inst)
                              target-pred))))
     (cond
       [(not matching) '()]
       [else
        (define inst (car matching))
        (define σ   (cdr matching))
        ;; An instance whose qual context is fully concrete
        ;; (no tvar-bearing constraints) gets compiled WITHOUT a
        ;; dict-arg lambda — its body resolved all class-method refs
        ;; to global impl names directly.  Passing dict-impls to such
        ;; an instance would arity-mismatch.  Detect this by checking
        ;; whether the instance's ORIGINAL qual context has any tvars.
        (define instance-needs-dict-args?
          (for/or ([c (in-list (instance-info-context inst))])
            (and (class-has-return-typed-methods? env (pred-class c))
                 (for/or ([a (in-list (pred-args c))])
                   (not (set-empty? (type-vars a)))))))
        (cond
          [(not instance-needs-dict-args?) '()]
          [else
           (apply append
                  (for/list ([c (in-list (instance-info-context inst))])
                    (define inst-pred (apply-subst σ c))
                    (define cls (pred-class inst-pred))
                    (define arg-types (pred-args inst-pred))
                    (cond
                      [(class-has-return-typed-methods? env cls)
                       (for/list ([pair (in-list
                                         (collect-dict-method-args cls arg-types env))])
                         (resolve-impl-with-quals (car pair) (cdr pair)
                                                  final-subst stx env))]
                      [else '()])))])])]))

(define (resolve-return-impl method-name class-param-tvars final-subst stx
                             [env #f])
  (define tcon-names
    (for/list ([tv (in-list class-param-tvars)])
      (type-head-tcon (apply-subst final-subst tv))))
  ;; For fundep-bearing classes, only the "determining" params (those
  ;; not on the RHS of any fundep) participate in the impl name —
  ;; matches the impl name compile-instance emits, which is keyed by
  ;; the head-tcon of the determining-param position.
  (define keep-tcon-names
    (cond
      [(not env) tcon-names]
      [else
       (define owner (env-ref-method-class env method-name))
       (define cinfo (and owner (env-ref-class env owner)))
       (cond
         [(or (not cinfo) (null? (class-info-fundeps cinfo))) tcon-names]
         [else
          (define determined
            (for/fold ([acc (seteq)])
                      ([fd (in-list (class-info-fundeps cinfo))])
              (set-union acc (list->seteq (cdr fd)))))
          (for/list ([p (in-list (class-info-params cinfo))]
                     [tn (in-list tcon-names)]
                     #:unless (set-member? determined p))
            tn)])]))
  (cond
    [(andmap values keep-tcon-names)
     ;; If a class-param resolves to a tracked skolem,
     ;; the call is inside a needs-dict-body — emit a reference to
     ;; the locally-bound dict-arg instead of the per-tcon impl.
     (define skol-map (current-dict-skolems))
     (define skolem-local
       (and skol-map
            ;; For a multi-param class only one of the resolved tcons
            ;; is the skolem we tracked (the determining param via
            ;; fundep) — scan all of them and pick whichever matches.
            (for/or ([tn (in-list tcon-names)])
              (hash-ref skol-map (cons tn method-name) #f))))
     (cond
       [skolem-local skolem-local]
       [else
        (return-impl-symbol method-name keep-tcon-names)])]
    [else
     (raise-syntax-error 'infer
       (format "ambiguous use of ~s: cannot determine target type at this call site"
               method-name)
       stx)]))

;; Hardcoded knowledge: which return-typed methods does each
;; "dict-providing" class supply?  Today there is exactly one entry
;; — `Applicative` provides `pure`.  Future classes that need to be
;; dict-passable would register here.
;; Includes return-typed methods from superclasses transitively — a
;; constraint `(Monad m) =>` carries `pure` because Applicative is a
;; superclass of Monad and Applicative declares pure as return-typed.
(define (dict-class-return-methods class-name [env #f])
  ;; Return-typed methods reachable from a constraint of class
  ;; `class-name`: the class's own methods *plus* a transitive
  ;; superclass closure (e.g. a MonadEnv constraint implies Monad
  ;; which transitively implies Applicative — whose `pure` shows up).
  ;; The hardcoded registry retains the prelude's well-known closures
  ;; for fast paths and for callers without an env in hand.
  ;; The method list is sorted by symbol name for deterministic
  ;; ordering across producer (call sites) and consumer (instance
  ;; impls) — `in-hash` order is implementation-defined.
  (define from-registry
    (case class-name
      [(Applicative) '(pure)]
      [(Monad)       '(pure)]
      [(Monoid)      '(mempty)]
      [else '()]))
  (cond
    [(not (null? from-registry)) from-registry]
    [env
     (define cinfo (env-ref-class env class-name))
     (cond
       [(not cinfo) '()]
       [else
        (define own
          (sort
           (for/list ([(m dp) (in-hash (class-info-dispatchpos cinfo))]
                      #:when (eq? dp 'return))
             m)
           symbol<?))
        (define super-methods
          (apply append
                 (for/list ([sp (in-list (class-info-supers cinfo))])
                   (dict-class-return-methods (pred-class sp) env))))
        (remove-duplicates (append own super-methods))])]
    [else '()]))

;; Collect (method-name . arg-types) pairs for every return-typed
;; method reachable from a constraint of class `cls` with arg list
;; `arg-types`.  Walks the superclass hierarchy, threading the outer
;; arg-types through each super-pred so methods inherited from a
;; super-class see only THEIR class's arg slots.
(define (collect-dict-method-args cls arg-types env)
  (define cinfo (and env (env-ref-class env cls)))
  (define own-methods
    (cond
      [cinfo
       (sort
        (for/list ([(m dp) (in-hash (class-info-dispatchpos cinfo))]
                   #:when (eq? dp 'return))
          m)
        symbol<?)]
      [else
       (case cls
         [(Applicative) '(pure)]
         [(Monad)       '(pure)]
         [(Monoid)      '(mempty)]
         [else '()])]))
  (define own-pairs
    (for/list ([m (in-list own-methods)]) (cons m arg-types)))
  (define super-pairs
    (cond
      [(not cinfo) '()]
      [else
       (define params (class-info-params cinfo))
       (apply
        append
        (for/list ([sp (in-list (class-info-supers cinfo))])
          (define super-cls (pred-class sp))
          (define mapped-args
            (for/list ([sa (in-list (pred-args sp))])
              (cond
                [(tvar? sa)
                 (define idx
                   (for/or ([p (in-list params)] [i (in-naturals)]
                            #:when (eq? p (tvar-name sa)))
                     i))
                 (cond [idx (list-ref arg-types idx)]
                       [else sa])]
                [else sa])))
          (collect-dict-method-args super-cls mapped-args env)))]))
  (define merged
    (for/fold ([acc own-pairs]) ([sp (in-list super-pairs)])
      (cond
        [(assq (car sp) acc) acc]
        [else (append acc (list sp))])))
  merged)

;; Extract the head type-constructor name from a (possibly applied)
;; concrete type.  Returns #f if the type is still polymorphic.
(define (type-head-tcon t)
  (match t
    [(tcon n) n]
    [(tapp h _) (type-head-tcon h)]
    [_ #f]))

;; A pre-existing instance is a "true duplicate" only if
;; its methods (per the elaboration that recorded it) align with the
;; currently-known class shape.  Conservative impl: compare the keys
;; of `instance-info-methods` against the class's method names — if
;; the existing instance defines methods the current class doesn't
;; know about, it belonged to a previously-declared (and now
;; superseded) class and isn't a real duplicate.
(define (instance-matches-class-shape? inst cinfo)
  (define inst-method-names
    (sort (hash-keys (instance-info-methods inst)) symbol<?))
  (define class-method-names
    (sort (hash-keys (class-info-methods cinfo)) symbol<?))
  (or (null? inst-method-names)
      (for/and ([m (in-list inst-method-names)])
        (member m class-method-names))))

;; Walk a surface-expression AST and collect every e:var whose name
;; is in `class-method-names` (a hasheq used as a set).  Returns the
;; set of class methods referenced anywhere in the body — including
;; in dead branches of `if`/`match`, since for cycle-completeness we
;; care about syntactic references, not reachability.
(define (collect-class-method-refs expr class-method-names)
  (define found (make-hasheq))
  (define (visit e)
    (cond
      [(e:literal? e) (void)]
      [(e:var? e)
       (define n (e:var-name e))
       (when (hash-ref class-method-names n #f)
         (hash-set! found n #t))]
      [(e:lam? e) (visit (e:lam-body e))]
      [(e:app? e) (visit (e:app-head e))
                  (for-each visit (e:app-args e))]
      [(e:let? e) (for ([b (in-list (e:let-bindings e))])
                    (visit (cdr b)))
                  (visit (e:let-body e))]
      [(e:letrec? e) (for ([b (in-list (e:letrec-bindings e))])
                       (visit (cdr b)))
                     (visit (e:letrec-body e))]
      [(e:if? e) (visit (e:if-test e))
                 (visit (e:if-then e))
                 (visit (e:if-else e))]
      [(e:ann? e) (visit (e:ann-expr e))]
      [(e:match? e) (visit (e:match-scrutinee e))
                    (for ([c (in-list (e:match-clauses e))])
                      (when (clause-guard c) (visit (clause-guard c)))
                      (visit (clause-body c)))]
      [(e:update? e) (visit (e:update-record e))
                     (for ([u (in-list (e:update-updates e))])
                       (visit (cdr u)))]
      [(e:escape? e) (visit (e:escape-body e))]
      [else (void)]))
  (visit expr)
  found)

;; Given a class's defaults hash and its set of method names, build a
;; directed graph: method m → list of methods called from m's default
;; body.  Methods with no default appear in the graph as nodes with no
;; outgoing edges (callers can't use them as fallback).
(define (build-default-call-graph defaults method-names)
  (define name-set (for/hasheq ([m (in-list method-names)]) (values m #t)))
  (for/hasheq ([m (in-list method-names)])
    (define default (hash-ref defaults m #f))
    (values m
            (cond
              [(not default) '()]
              [else (hash-keys (collect-class-method-refs default name-set))]))))

;; DFS cycle-detector.  Returns one cycle (a list of node names in
;; traversal order, with the repeated node at both ends) if any cycle
;; exists in the subgraph restricted to `live-nodes`, else #f.
(define (find-method-cycle live-nodes adj)
  (define color (make-hasheq))    ; node → 'white | 'gray | 'black
  (define parent (make-hasheq))   ; node → predecessor on current DFS path
  (define cycle (box #f))
  (define live? (for/hasheq ([n (in-list live-nodes)]) (values n #t)))
  (define (dfs n)
    (unless (unbox cycle)
      (hash-set! color n 'gray)
      (for ([m (in-list (hash-ref adj n '()))]
            #:when (hash-ref live? m #f)
            #:unless (unbox cycle))
        (case (hash-ref color m 'white)
          [(gray)
           ;; Back-edge: cycle from m back to itself through n.
           (define cyc
             (let loop ([k n] [acc (list m)])
               (cond
                 [(equal? k m) (cons m acc)]
                 [else (loop (hash-ref parent k) (cons k acc))])))
           (set-box! cycle cyc)]
          [(white)
           (hash-set! parent m n)
           (dfs m)]))
      (hash-set! color n 'black)))
  (for ([n (in-list live-nodes)]
        #:when (eq? (hash-ref color n 'white) 'white)
        #:unless (unbox cycle))
    (dfs n))
  (unbox cycle))

;; Check that the instance's user-defined methods break every cycle in
;; the class's default-call graph.  If a cycle remains among methods
;; the instance did NOT define, raise a targeted syntax error.
(define (check-instance-default-cycle cinfo user-impls head-pred-raw stx)
  (define method-names (hash-keys (class-info-methods cinfo)))
  (define adj (build-default-call-graph (class-info-defaults cinfo)
                                        method-names))
  (define live (filter (lambda (m) (not (hash-ref user-impls m #f)))
                       method-names))
  (define cyc (find-method-cycle live adj))
  (when cyc
    (raise-syntax-error 'infer
      (format
       (string-append
        "instance ~s is incomplete: methods ~s form a cyclic default chain "
        "(~a); at least one must be defined directly to break the cycle.")
       (pred->datum head-pred-raw)
       cyc
       (string-join (map symbol->string cyc) " → "))
      stx)))

;; Cross-class default/derived cycle check for a `#:derive-superclasses`
;; instance.  `check-instance-default-cycle` above is intra-class: it
;; only sees edges between methods of ONE class, so it cannot detect a
;; loop that runs between a deriving-class method (left to its class
;; default) and a superclass method whose body comes from the deriving
;; class's `#:derive` table.  That is exactly the loop the user can write
;; by leaving both ends to be auto-filled, e.g. Comonad's `extend`
;; (default `(fmap … (duplicate …))`) and a derived `fmap`
;; (`(extend … …)`): each is synthesized, neither is user-written, so a
;; runtime call recurses forever.
;;
;; Build one call graph over the deriving class `C` and the superclasses
;; actually synthesized for this instance (`synth-supers`).  A method is
;; *solid* (it breaks cycles) when the user wrote it directly in the
;; instance body — i.e. it is in `bundled`.  A superclass that was NOT
;; synthesized (a real instance for the carrier already exists) supplies
;; its methods elsewhere; those names stay out of the graph, so edges to
;; them are cut and cannot raise a false positive.  Method names have
;; unique owners across classes, so the union by name never conflates two
;; distinct methods.
(define (check-derived-instance-cycle C synth-supers merged bundled env head-pred stx)
  (define classes (cons C synth-supers))
  (define (class-methods K) (hash-keys (class-info-methods (env-ref-class env K))))
  (define all-methods (append* (map class-methods classes)))
  (define name-set (for/hasheq ([m (in-list all-methods)]) (values m #t)))
  ;; the body that fills method `m` of class `K` when the user did not
  ;; write it: a `#:derive`-table entry (superclass methods only) else the
  ;; owning class's intra-class default; #f when neither exists.
  (define (filled-body K m)
    (or (and (not (eq? K C))
             (hash-ref (hash-ref merged K (hasheq)) m #f))
        (hash-ref (class-info-defaults (env-ref-class env K)) m #f)))
  ;; m → methods its filling body calls (restricted to this method set).
  ;; User-written methods (in `bundled`) are solid: no outgoing edges.
  (define adj
    (for/fold ([h (hasheq)]) ([K (in-list classes)])
      (for/fold ([h h]) ([m (in-list (class-methods K))])
        (define body (and (not (hash-ref bundled m #f)) (filled-body K m)))
        (hash-set h m
                  (if body
                      (hash-keys (collect-class-method-refs body name-set))
                      '())))))
  (define live (filter (lambda (m) (not (hash-ref bundled m #f))) all-methods))
  (define cyc (find-method-cycle live adj))
  (when cyc
    (raise-syntax-error 'infer
      (format
       (string-append
        "instance ~s is incomplete: methods ~s form a cyclic "
        "default/derived chain across classes (~a); define at least one "
        "of them directly in the instance to break the cycle.")
       (pred->datum head-pred)
       cyc
       (string-join (map symbol->string cyc) " → "))
      stx)))

;; Identity of the module a piece of syntax came from, as a string, or
;; #f when unknown (e.g. the synthetic syntax used during prelude
;; construction).  Used to stamp an instance's origin so a diamond
;; import of the SAME instance can be deduped without rejecting two
;; genuinely different instances that share a head.
(define (origin-of stx)
  (define src (and (syntax? stx) (syntax-source stx)))
  (cond
    [(path? src)   (path->string src)]
    [(string? src) src]
    [else          #f]))

(define (handle-instance-form ctx head methods stx env declared st)
  (define head-pred-raw (resolve-constraint head env))
  (define class-name (pred-class head-pred-raw))
  (define inst-origin (origin-of stx))
  (define cinfo (env-ref-class env class-name))
  (unless cinfo
    (raise-syntax-error 'infer
      (format "unknown class: ~s~a"
              class-name (suggest-similar class-name env 'class))
      stx))
  ;; Reject duplicate instance registrations (heads
  ;; α-equivalent to an existing one) at compile time.  A test
  ;; corpus that re-declares a prelude class (e.g. classes-test.rkt
  ;; defining its own `Eq`) re-establishes instances against the
  ;; redeclared class — skip the dup check when the class itself
  ;; was redeclared in this elaboration (a previously-registered
  ;; class-info now overlaps with a fresh one).  Detect this by
  ;; checking if there are instances for the class but the class
  ;; methods are a subset of the redeclared class's methods.
  (for ([existing (in-list (env-instances env class-name))])
    (when (instance-heads-equivalent? (instance-info-head existing)
                                      head-pred-raw)
      ;; The "already-known" instance is a duplicate only if it
      ;; belongs to a class with the same method set.  When the
      ;; user redeclares a class, the old instance still hangs
      ;; around but its method scheme is from the previous
      ;; declaration — silently shadow it (drop the duplicate
      ;; error) and let env-extend-instance append the new one.
      ;; In REPL redefinition mode an α-equivalent re-declaration is
      ;; allowed (it replaces the old one at registration below); only a
      ;; module-level duplicate is an error.
      (when (and (instance-matches-class-shape? existing cinfo)
                 (not (current-allow-instance-redefinition?)))
        (raise-syntax-error 'infer
          (format "duplicate instance: ~s already declared"
                  (pretty-pred head-pred-raw))
          stx))))
  (define inst-args-raw (pred-args head-pred-raw))
  ;; The instance head must supply exactly as many type arguments as the
  ;; class was declared with.  An under-applied head — e.g. `(Arrow
  ;; (Kleisli m))` for the two-parameter `Arrow cat p` — would otherwise
  ;; leave a class parameter as an undetermined skolem and only fail
  ;; later as a confusing method-body mismatch.  Name the arity at the
  ;; head instead.
  (let ([param-count (length (class-info-params cinfo))]
        [arg-count   (length inst-args-raw)])
    (unless (= arg-count param-count)
      (raise-syntax-error 'infer
        (format "instance head for class ~s expects ~a type argument~a ~s, but got ~a: ~s"
                class-name param-count (if (= param-count 1) "" "s")
                (class-info-params cinfo) arg-count
                (pretty-pred head-pred-raw))
        stx)))
  (define ctx-preds-raw (for/list ([c (in-list ctx)]) (resolve-constraint c env)))
  ;; If the qual context introduces tvars that pin a
  ;; return-typed-bearing class (e.g. `(HasUnit m) =>` on a lifted
  ;; instance), skolemize those tvars and build a map from each
  ;; skolem to a local dict-arg name.  The instance body's polymorphic
  ;; class-method references will resolve against this map.
  (define-values (sk-subst dict-skolems dict-arg-names)
    (instance-qual-skolems ctx-preds-raw env))
  ;; Apply the instance-qual skolem substitution into the parts that
  ;; flow into body inference.
  ;; The skolemized versions are used only for body-checking
  ;; hypotheses; the env entry uses the original (un-skolemized) head
  ;; and ctx so other constraints can match against this instance.
  (define inst-args-sk (map (lambda (a) (apply-subst sk-subst a)) inst-args-raw))
  (define head-pred-sk (apply-subst sk-subst head-pred-raw))
  (define ctx-preds-sk (map (lambda (p) (apply-subst sk-subst p)) ctx-preds-raw))
  ;; Dict-arg names are saved per-method (combined
  ;; instance-qual + method-qual), inside the method loop below.
  ;; Key the needs-dict-defs entry by the DISPATCHING head-arg's tcon —
  ;; for a fundep class (e.g. MonadState s m | m -> s) that is the
  ;; determining param (m), not the determined `s` (a bare tvar whose
  ;; type-head-tcon is #f).  This must agree byte-for-byte with the
  ;; `(car head-tcon-names)` lookup compile-instance does in codegen.rkt;
  ;; otherwise a needs-dict instance method in function form (put-st,
  ;; modify-st, …) misses its dict-arg prepend and the inner-`pure` dict
  ;; reference is left unbound.
  (define head-tcon
    (cond
      [(null? (class-info-fundeps cinfo)) (type-head-tcon (car inst-args-raw))]
      [else
       (define determined
         (for/fold ([acc (seteq)])
                   ([fd (in-list (class-info-fundeps cinfo))])
           (set-union acc (list->seteq (cdr fd)))))
       (define kept
         (for/list ([p (in-list (class-info-params cinfo))]
                    [a (in-list inst-args-raw)]
                    #:unless (set-member? determined p))
           (type-head-tcon a)))
       (if (null? kept) (type-head-tcon (car inst-args-raw)) (car kept))]))
  (define user-impls
    (for/fold ([acc (hasheq)]) ([m (in-list methods)])
      (match m
        [(top:def name expr _) (hash-set acc name expr)]
        [(inst-type-fam _ _ _) acc])))
  ;; Require the instance to break every default cycle: if any cycle
  ;; remains among methods the instance did not define, fail at
  ;; compile time with a message naming the cycle.  Without this check
  ;; an "all-default" instance would loop at runtime on first call.
  (check-instance-default-cycle cinfo user-impls head-pred-raw stx)
  ;; Collect this instance's type-family bindings and
  ;; check that every family the class declared is supplied.
  (define type-family-bindings
    (for/fold ([acc (hasheq)]) ([m (in-list methods)]
                                #:when (inst-type-fam? m))
      (hash-set acc
                (inst-type-fam-name m)
                (resolve-type (inst-type-fam-type m) env))))
  (for ([fam (in-list (class-info-type-families cinfo))])
    (unless (hash-has-key? type-family-bindings fam)
      (raise-syntax-error 'infer
        (format "instance ~s missing #:type binding for associated type ~s"
                (pred->datum head-pred-raw) fam)
        stx)))
  ;; Register the instance (with empty method bodies, full
  ;; type-family bindings) into a working env BEFORE checking method
  ;; bodies so normalize-type can resolve `(Family arg)` references
  ;; inside the method types against this instance's bindings.
  (define env-with-inst
    (cond
      [(hash-empty? type-family-bindings) env]
      [else
       (env-extend-instance env class-name
                            (instance-info head-pred-raw ctx-preds-raw
                                           (hasheq)
                                           type-family-bindings
                                           inst-origin
                                           (and (current-prelude-build?) #t)))]))
  (define ictx
    (inst-check-ctx env env-with-inst cinfo class-name
                    head-pred-raw head-pred-sk ctx-preds-sk inst-args-sk
                    head-tcon dict-skolems dict-arg-names user-impls stx))
  (define-values (checked-bodies st-after)
    (for/fold ([acc (hasheq)] [st st])
              ([(method-name method-sch)
                (in-hash (class-info-methods cinfo))])
      (define-values (body st-b) (instance-method-body ictx method-name st))
      (define-values (mc st1) (method-qual-context ictx method-name method-sch st-b))
      (define st2 (elaborate-instance-method-body! ictx method-name body mc st1))
      (values (hash-set acc method-name body) st2)))
  ;; Mark instances built during prelude-env construction (prelude? = #t) so
  ;; the monomorphization resolver knows not to redirect calls to a
  ;; non-existent named impl — intrinsic to the struct, no side table.
  (define info (instance-info head-pred-raw ctx-preds-raw checked-bodies
                              type-family-bindings inst-origin
                              (and (current-prelude-build?) #t)))
  ;; In REPL redefinition mode, drop any α-equivalent existing instance
  ;; for this class so the new one replaces it rather than sitting
  ;; alongside (which would read as an overlap at resolution).
  (define base-env
    (if (current-allow-instance-redefinition?)
        (env-set-instances
         env class-name
         (filter (lambda (i)
                   (not (instance-heads-equivalent? (instance-info-head i)
                                                    head-pred-raw)))
                 (env-instances env class-name)))
        env))
  (values (env-extend-instance base-env class-name info) declared st-after))

;; ----- instance method-body checking ------------------------------------
;;
;; handle-instance-form checks every method body of one instance against the
;; class signature.  The per-method work — body selection, skolemization,
;; and the infer/unify/reduce/resolve pass — is factored into the helpers
;; below; the loop-invariant context they share rides in `inst-check-ctx`.

(struct inst-check-ctx
  (env env-with-inst cinfo class-name
   head-pred-raw head-pred-sk ctx-preds-sk inst-args-sk
   head-tcon dict-skolems dict-arg-names user-impls stx)
  #:transparent)

;; The per-method data the body must satisfy, computed from the class
;; signature: the expected (skolemized, normalized) method type, the
;; method's own qual-context preds (hypotheses), the combined dict-skolem
;; map, and the generality skolemization that makes head/method universals
;; rigid.
(struct method-check (expected-type extra-preds skolems generality-subst)
  #:transparent)

;; Skolemize the instance's qualifying context.  For each ctx pred that
;; pins a return-typed-bearing class (e.g. `(HasUnit m) =>` on a lifted
;; instance), skolemize its tvars and map each skolem to a local dict-arg
;; name; the body's polymorphic class-method references resolve against
;; this map.  Returns (values sk-subst dict-skolems dict-arg-names).
(define (instance-qual-skolems ctx-preds-raw env)
  (define inst-needs-dict-reqs
    (for/list ([c (in-list ctx-preds-raw)]
               #:when (class-has-return-typed-methods? env (pred-class c)))
      (cons (pred-class c) (pred-args c))))
  (cond
    [(null? inst-needs-dict-reqs) (values empty-subst (hash) '())]
    [else
     (define inner-vars
       (for/fold ([acc '()]) ([c (in-list ctx-preds-raw)])
         (for/fold ([acc acc]) ([a (in-list (pred-args c))])
           (set->list (set-union (list->seteq acc) (type-vars a))))))
     (define s
       (for/fold ([s empty-subst]) ([v (in-list inner-vars)])
         (subst-extend s v (tcon (gensym (format "$inst-skolem.~a." v))))))
     (define-values (sk-map args) (build-dict-skolems inst-needs-dict-reqs s env))
     (values s sk-map args)]))

;; Pick the body AST for `method-name`: the user's explicit impl, else the
;; class default freshened to THIS instance's site (distinct handles,
;; anchored at the instance stx, recorded for codegen so its return-typed
;; calls resolve against this instance's carrier rather than the protocol's
;; abstract class parameter), else a compile error.
(define (instance-method-body ctx method-name st)
  (define user-impls    (inst-check-ctx-user-impls ctx))
  (define cinfo         (inst-check-ctx-cinfo ctx))
  (define class-name    (inst-check-ctx-class-name ctx))
  (define head-tcon     (inst-check-ctx-head-tcon ctx))
  (define head-pred-raw (inst-check-ctx-head-pred-raw ctx))
  (define stx           (inst-check-ctx-stx ctx))
  (cond
    [(hash-ref user-impls method-name #f) => (lambda (b) (values b st))]
    [(hash-ref (class-info-defaults cinfo) method-name #f)
     => (lambda (default-expr)
          ;; Inherited class default.  Freshen it to THIS
          ;; instance's site (distinct handles, anchored at the
          ;; instance stx) before inferring, so its return-typed
          ;; method calls resolve against this instance's carrier
          ;; rather than the protocol's abstract class parameter.
          ;; Hand the freshened AST to codegen so the syntax
          ;; handles the method resolutions are keyed by agree.
          (define fresh-body (freshen-ast default-expr stx))
          (values fresh-body
                  (st-table-put st 'instance-default-bodies
                                (list class-name head-tcon method-name)
                                fresh-body)))]
    [else
     (raise-syntax-error 'infer
       (format "instance ~s missing method ~s with no default"
               (pred->datum head-pred-raw) method-name)
       stx)]))

;; Build the type the body must satisfy: substitute class params ->
;; instance args (skolemized), freshen the method scheme's own quantified
;; vars to avoid capture, skolemize method-qual + generality tvars,
;; normalize associated types, record any needs-dict arg names for codegen,
;; and return the resulting method-check.
(define (method-qual-context ctx method-name method-sch st)
  (define cinfo          (inst-check-ctx-cinfo ctx))
  (define env            (inst-check-ctx-env ctx))
  (define env-with-inst  (inst-check-ctx-env-with-inst ctx))
  (define inst-args-sk   (inst-check-ctx-inst-args-sk ctx))
  (define head-pred-sk   (inst-check-ctx-head-pred-sk ctx))
  (define dict-skolems   (inst-check-ctx-dict-skolems ctx))
  (define dict-arg-names (inst-check-ctx-dict-arg-names ctx))
  (define class-name     (inst-check-ctx-class-name ctx))
  (define head-tcon      (inst-check-ctx-head-tcon ctx))
  (define σ
    (for/fold ([s empty-subst])
              ([p (in-list (class-info-params cinfo))]
               [a (in-list inst-args-sk)])
      (subst-extend s p a)))
  ;; Freshen the method scheme's own quantified variables before
  ;; substituting class params.  Without this, a method universal
  ;; that happens to share a name with an instance-head variable is
  ;; captured by σ: for `(Functor (Pair a))` the class param goes
  ;; `f := (Pair a)`, and fmap's signature `(-> (f a) (f b))` —
  ;; whose own `a` is a distinct quantified variable — would collapse
  ;; to `(-> (Pair a a) (Pair a b))`, conflating the fixed first
  ;; component with the mapped one.  Renaming the scheme's bound vars
  ;; to fresh names keeps the head variable and the method universals
  ;; distinct.
  (define-values (method-fresh-subst st′)
    (for/fold ([s empty-subst] [st st])
              ([v (in-list (scheme-vars method-sch))]
               #:unless (memq v (class-info-params cinfo)))
      (define-values (tv st2) (st:fresh st v))
      (values (subst-extend s v tv) st2)))
  (define inst-method-qual
    (apply-subst σ (apply-subst method-fresh-subst (scheme-body method-sch))))
  ;; Skolemize method-local tvars that appear in this method's
  ;; qual context (e.g. `(Applicative f) =>` on traverse).
  ;; Without this, the body's polymorphic class-method references
  ;; (pure, fapply, …) can't resolve — the f tvar isn't a concrete
  ;; tcon nor an instance-qual skolem.  Make it a fresh tcon so
  ;; the same dict-skolem mechanism used for instance-qual tvars
  ;; resolves them to local dict-arg names.
  (define raw-method-qual-preds
    (filter (lambda (p) (not (equal? p head-pred-sk)))
            (qual-constraints-of inst-method-qual)))
  (define method-needs-dict-reqs
    (for/list ([p (in-list raw-method-qual-preds)]
               #:when (class-has-return-typed-methods? env (pred-class p)))
      (cons (pred-class p) (pred-args p))))
  (define method-sk-tvars
    (apply append
           (for/list ([req (in-list method-needs-dict-reqs)])
             (for/list ([a (in-list (cdr req))] #:when (tvar? a))
               (tvar-name a)))))
  (define method-sk-subst
    (for/fold ([s empty-subst]) ([v (in-list (remove-duplicates method-sk-tvars))])
      (subst-extend s v (tcon (gensym (format "$method-skolem.~a." v))))))
  (define-values (method-dict-skolems method-arg-names)
    (cond
      [(null? method-needs-dict-reqs) (values (hash) '())]
      [else
       (build-dict-skolems method-needs-dict-reqs method-sk-subst env)]))
  (define combined-skolems
    (for/fold ([acc dict-skolems])
              ([(k v) (in-hash method-dict-skolems)])
      (hash-set acc k v)))
  (define combined-arg-names
    (append dict-arg-names method-arg-names))
  ;; Store the two arg-name groups separately so
  ;; compile-instance can decide which dispatch path to use.
  ;; Instance-qual dicts require compile-time inst-dispatch
  ;; (the runtime wrapper doesn't insert them).  Method-qual
  ;; dicts flow through the existing runtime wrapper (whose
  ;; arity is bumped by class-info-dictreqs at compile-class
  ;; time), so the impl can be registered in the runtime
  ;; dispatch table with method-qual dicts as leading params.
  ;; Apply method-skolem subst to the expected type and method-
  ;; extra-preds so the tvars become rigid for body inference.
  ;; normalize-type rewrites any associated-type
  ;; references (e.g. `(Index (List a))`) to their concrete rhs
  ;; for this instance before unify.
  (define expected-type/flex
    (normalize-type env-with-inst
      (apply-subst method-sk-subst (qual-body-deep inst-method-qual))))
  ;; Skolemize every type variable still free in the expected method
  ;; type.  These are the class method's own universally-quantified
  ;; variables (e.g. fmap's `a`/`b`) plus the instance head's
  ;; variables (e.g. the `a` in `(Functor (Pair a))`).  Left flexible
  ;; they would let an over-specific body unify them together — an
  ;; `fmap` mapping the fixed field of a pair, or ignoring its
  ;; function argument, would wrongly typecheck.  Made rigid they
  ;; force the body to be as general as the class signature demands.
  ;; The same substitution is applied to the body-checking
  ;; hypotheses below so constraint resolution stays consistent with
  ;; these skolems.
  (define generality-sk-subst
    (for/fold ([s empty-subst])
              ([v (in-set (type-vars expected-type/flex))])
      (subst-extend s v (tcon (gensym (format "$gen-skolem.~a." v))))))
  (define expected-type
    (apply-subst generality-sk-subst expected-type/flex))
  (define method-extra-preds
    (map (lambda (p)
           (apply-subst generality-sk-subst (apply-subst method-sk-subst p)))
         raw-method-qual-preds))
  (define st-nd
    (if (null? combined-arg-names) st′
        (st-table-put st′ 'needs-dict-defs
                      (list class-name head-tcon method-name)
                      (cons dict-arg-names method-arg-names))))
  (values (method-check expected-type method-extra-preds combined-skolems generality-sk-subst)
          st-nd))

;; Infer the body, unify it against the class-signature type, run fundep
;; improvement (so a default body's residual tensor constraints reduce
;; against the head hypothesis), ensure no constraints leak, and resolve
;; the body's return-typed method uses.  Raises on a type/constraint error.
(define (elaborate-instance-method-body! ctx method-name body mc st)
  (define env            (inst-check-ctx-env ctx))
  (define env-with-inst  (inst-check-ctx-env-with-inst ctx))
  (define head-pred-sk   (inst-check-ctx-head-pred-sk ctx))
  (define ctx-preds-sk   (inst-check-ctx-ctx-preds-sk ctx))
  (define head-pred-raw  (inst-check-ctx-head-pred-raw ctx))
  (define stx            (inst-check-ctx-stx ctx))
  (define expected-type       (method-check-expected-type mc))
  (define method-extra-preds  (method-check-extra-preds mc))
  (define combined-skolems    (method-check-skolems mc))
  (define generality-sk-subst (method-check-generality-subst mc))
  ;; Isolate the body's pending preds (the fresh counter still threads
  ;; through): save the outer bag, check the body against an empty one, then
  ;; restore the outer bag — keeping the advanced counter.
  (define outer-preds (st:preds st))
  (let ()
    ;; Make the instance-qual + method-qual skolem map visible
    ;; while inferring the body AND while resolve-method-uses!
    ;; runs afterward.
    (define saved-skolems (current-dict-skolems))
    (current-dict-skolems combined-skolems)
    (define-values (s t st1) (infer-expr body env-with-inst (st:set-preds st '())))
    (define s-u
      (with-handlers
       ([exn:fail:unify?
         (lambda (_)
           (raise-syntax-error 'infer
             (format "method ~s body has the wrong type\n~a"
                     method-name
                     (expected/got-block expected-type (apply-subst s t)))
             stx))])
       (unify (apply-subst s t) expected-type)))
    (define final-subst0 (subst-compose s-u s))
    (define st2 (st:apply-subst-to-preds st1 final-subst0))
    ;; The instance head itself is a hypothesis during method
    ;; checking — plus any constraints from the method's own
    ;; qualifying context (e.g. `Applicative f` for traverse).
    (define hyp-preds
      (append (cons (apply-subst generality-sk-subst head-pred-sk)
                    (map (lambda (p) (apply-subst generality-sk-subst p))
                         ctx-preds-sk))
              method-extra-preds))
    ;; Run fundep improvement before reducing, exactly as the top-def
    ;; path does.  A default method body that builds products through
    ;; the abstract tensor — e.g. on-second's `mk-prod`/`arr`, whose
    ;; class param `p` is phantom in `arr`'s type — leaves residual
    ;; `(Arrow cat a)` / `(Prod a)` constraints.  This instance isn't
    ;; registered yet while its own bodies are checked, so improve
    ;; against the head hypothesis (`cat -> p` forces `a := p`),
    ;; connecting the body's fresh tensor var to the instance product
    ;; so the residuals reduce instead of being reported unsolved.
    ;; The improved subst also feeds `resolve-method-uses!` below, so
    ;; the body's return-typed `mk-prod`/`arr`/`inj-*` resolve against
    ;; the concrete tensor rather than an ambiguous tvar.
    (define-values (m-fd-sub st3) (improve-by-fds env st2))
    (define m-hyp-closure
      (append-map (lambda (p) (by-super env (apply-subst m-fd-sub p)))
                  hyp-preds))
    (define-values (m-hyp-fd-sub st4) (improve-by-hyp-fds env m-hyp-closure st3))
    (define final-subst
      (subst-compose m-hyp-fd-sub (subst-compose m-fd-sub final-subst0)))
    (define leftovers
      (reduce-context env
                      (map (lambda (p) (apply-subst final-subst p)) hyp-preds)
                      (st:preds st4)))
    (unless (null? leftovers)
      (raise-syntax-error 'infer
        (render-doc (labeled-block
                     (format "instance ~a method ~a leaves unsolved constraints:"
                             (pretty-pred head-pred-raw) method-name)
                     (map pretty-pred leftovers))
                    (current-type-columns))
        stx))
    (define st5 (resolve-method-uses st4 final-subst env))
    (current-dict-skolems saved-skolems)
    ;; Restore the outer pending-pred bag; the counter in st5 carries on.
    (st:set-preds st5 outer-preds)))


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
         ;; primitive scalar type recognizer (REPL ,info) — these tcons
         ;; are never registered as data, so consumers need this to know
         ;; Integer / Boolean / String / Float are real types
         primitive-type?
         ;; the aligned "expected: …/got: …" diagnostic block, wrapping
         ;; each type at `current-type-columns` and indenting continuation
         ;; lines under the value column.  Exported so the pretty-printer
         ;; test can pin the width and assert the layout deterministically,
         ;; without depending on the ambient terminal.
         expected/got-block
         ;; threaded inference state — the REPL persists one across inputs
         make-infer-state st-table
         current-dict-skolems
         current-prelude-build?
         current-allow-instance-redefinition?)

(require racket/match
         racket/set
         racket/list
         racket/string
         (only-in syntax/modresolve resolve-module-path)
         "types.rkt"
         "diagnostic.rkt"
         "env.rkt"
         "unify.rkt"
         (only-in "nat-solve.rkt" normalize-nat-type)
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

;; The skolemized constraints of the declared signature whose body is
;; currently being checked — e.g. `(Eq $skolem.a)` for `(: f (All (a)
;; ((Eq a) => …)))`.  These givens are in scope throughout the body, so
;; an inner `let`/`letrec` generalized mid-body may assume them when it
;; discharges its own constraints.  The declared-def paths set this
;; around body inference; `generalize*` folds it into the hypotheses it
;; hands to `reduce-context`.  Empty everywhere else (the undeclared
;; path has no skolem givens, so a residual constraint there is a tvar
;; constraint that stays in head-normal form and propagates up).
(define current-given-preds (make-parameter '()))

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
;; Read the GADT-elimination expected type currently in scope (or #f).
(define ((m:read-expected) _ctx st) (values (current-expected-type) st))
;; Run `comp` with `current-expected-type` bound to `exp` (parameterize
;; wraps the *execution* — the monad is delayed, so binding it at
;; construction time would not reach run-infer).  Used to push a function's
;; declared result type into a GADT `match` that sits in a tail position
;; (an `if` branch, a `let`/`letrec` body) and to CLEAR it everywhere else,
;; so the expected type only ever reaches a tail GADT-elimination site.
(define ((with-expected exp comp) ctx st)
  (parameterize ([current-expected-type exp]) (comp ctx st)))
;; Run a monadic computation with extra skolem givens in scope (see
;; `current-given-preds`).  Appends to the execution-time givens rather
;; than replacing them, so a scope nested inside a declared def — an
;; existential `open` or a `match` arm in the def's body — composes its
;; own packed hypotheses with the def's.  Parameterizes the *execution*
;; (the monad is delayed), mirroring `with-expected`.
(define ((with-given-preds ps comp) ctx st)
  (parameterize ([current-given-preds (append ps (current-given-preds))])
    (comp ctx st)))
;; The expression forms into which an expected result type may flow: a
;; `match` (which consumes it) or an `if`/`let`/`letrec` (which threads it
;; on to its own tail sub-expression).  Anything else is NOT a tail
;; position, so the expected type is cleared before descending into it.
(define (tail-elim-form? e)
  (or (e:match? e) (e:match*? e) (e:if? e) (e:let? e) (e:letrec? e)))
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
    [(texists? expected-ty)
     ;; PACK — the dual of the tforall skolemize branch.  Instantiate the
     ;; hidden vars with fresh UNIFICATION tvars (the witness solves them),
     ;; check `expr` against the body, and emit the `:where` constraints
     ;; as pending preds so they are discharged at this site against the
     ;; concrete witness.  The result type is the existential itself — the
     ;; witness is hidden (it appears nowhere in `expected-ty`).
     (match-define (texists vs body) expected-ty)
     (let/infer ([s-inst (fresh-subst/m vs)])
       (let* ([body*  (apply-subst s-inst body)]
              [preds  (qual-constraints-of body*)]
              [bare   (qual-body-deep body*)])
         (let/infer ([_ (m:add-preds preds)]
                     [r (check-expr/m expr env bare)])
           (let ([s (car r)])
             (infer-return (cons s (apply-subst s expected-ty)))))))]
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

;; Lift a skolem-subst over a scheme's body.  Skolems are tcons, never
;; bound by the scheme's quantifier list, so the body is refined
;; regardless of `vs`.
(define (apply-skolem-subst/scheme skol-s sch)
  (match sch
    [(scheme vs body) (scheme vs (apply-skolem-subst skol-s body))]))

;; Push a GADT pattern match's local refinement into the types of every
;; in-scope binding for the matched arm.  The refinement (skol-s) maps
;; the function-scheme skolem learned from the constructor to a concrete
;; index type; applying it to the arm's environment is what lets a later
;; use of an in-scope binding (e.g. a continuation argument whose type
;; mentions the same index) see the refined type.  Scoped to one arm:
;; callers pass the arm-local env, never the shared outer env, so no
;; refinement leaks to sibling arms.  No-op (and skips the walk) when
;; the refinement is empty, which is the common, non-GADT case.
(define (apply-skolem-subst/env skol-s e)
  (cond
    [(hash-empty? skol-s) e]
    [else
     (struct-copy env e
                  [vars
                   (for/hasheq ([(k sch) (in-hash (env-vars e))])
                     (values k (apply-skolem-subst/scheme skol-s sch)))])]))

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
  ;; The enclosing declared signature's skolem givens (if any) are in
  ;; scope here, so a constraint this binding raised over one of those
  ;; skolems — e.g. an inner `let loop` using `==` under `(Eq a)` —
  ;; is discharged against the hypothesis rather than searched for as
  ;; an instance.  Without it, the ground skolem pred has no proof and
  ;; reduce-context reports a spurious "no instance".
  (define reduced
    (reduce-context env (append hypotheses (current-given-preds)) preds))
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
    [(e:open _ _ _ _ s) s]
    [(e:ann _ _ s)      s]
    [(e:match _ _ _ s)  s]
    [(e:match* _ _ _ s) s]
    [(e:escape _ _ _ s) s]
    [(e:tuple _ s)      s]
    [(e:bits _ s)       s]
    [(e:tref _ _ s)     s]
    [(e:array _ s)      s]
    [(e:build-array _ _ s) s]
    [(e:aref _ _ s)     s]
    [(e:array-slice _ _ _ s) s]))

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
    [(ty:nat v _) (ctx-return (tnat v))]
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
            ;; A 2-element `(Tuple a b)` canonicalizes to `(Pair a b)`:
            ;; `Pair` is the binary tuple's head, so it can be used
            ;; unapplied as a higher-kinded constructor (Bifunctor/Prod)
            ;; while staying interchangeable with the 2-tuple.
            (ctx-return (make-tapp (tcon (tuple-head-name n rargs)) rargs)))]))]
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
    [(ty:exists vs body _)
     ;; A first-class existential becomes a `texists` carrying its hidden
     ;; vars; the body (usually a `qual`) resolves under them.
     (let/ctx ([rb (resolve-type/m body)])
       (ctx-return (texists vs rb)))]
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
    [(ty:nat _ _) ty-ast]
    [(ty:app h args stx)
     (ty:app (substitute-tyvars sub h)
             (for/list ([a (in-list args)]) (substitute-tyvars sub a))
             stx)]
    [(ty:forall vs body stx)
     (define sub*
       (for/fold ([s sub]) ([v (in-list vs)]) (hash-remove s v)))
     (ty:forall vs (substitute-tyvars sub* body) stx)]
    [(ty:exists vs body stx)
     (define sub*
       (for/fold ([s sub]) ([v (in-list vs)]) (hash-remove s v)))
     (ty:exists vs (substitute-tyvars sub* body) stx)]
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
  (when (constraint? c) (kind-check-constraint-surface env c))
  (run-ctx (resolve-constraint/m c) (resolve-ctx (env-aliases env) (seteq))))
(define (resolve-scheme ty-ast env)
  (kind-check-surface env ty-ast)
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
    [(e:open e tvs vv body stx)      (infer-open/m e tvs vv body stx env)]
    [(e:ann expr ty-ast stx)         (infer-ann/m expr ty-ast stx env)]
    [(e:escape ty-ast vars _ stx)    (infer-escape/m ty-ast vars stx env)]
    [(e:update record updates stx)   (infer-update/m record updates stx env)]
    [(e:tuple elems stx)             (infer-tuple/m elems stx env)]
    [(e:bits segs stx)               (infer-bits/m segs stx env)]
    [(e:tref te idx stx)             (infer-tref/m te idx stx env)]
    [(e:array elems stx)            (infer-array/m elems stx env)]
    [(e:build-array n proc stx)     (infer-build-array/m n proc stx env)]
    [(e:aref ae idx stx)            (infer-aref/m ae idx stx env)]
    [(e:array-slice op k ae stx)    (infer-array-slice/m op k ae stx env)]
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
;; When an application carries `'rackton:kw-labels` (it came from keyword
;; construction `(C :f v …)`), verify the head is a constructor with named
;; fields and that the labels match those fields exactly and in declared
;; order.  The arguments are already positional (the values in source
;; order), so a passing check means the ordinary application logic below
;; types it correctly.
(define (validate-kw-labels-for-ctor! ctor-name labels stx env)
  (define info (and ctor-name (env-ref-data env ctor-name)))
  (cond
    [(not info)
     (raise-syntax-error 'rackton
       "keyword fields require a constructor with named fields"
       stx)]
    [(not (data-info-field-names info))
     (raise-syntax-error 'rackton
       (format "constructor ~a has positional fields; keyword fields are not allowed"
               ctor-name)
       stx)]
    [(not (equal? labels (data-info-field-names info)))
     (raise-syntax-error 'rackton
       (format "keyword fields for ~a must be ~a in declared order, got ~a"
               ctor-name
               (map (lambda (f) (string->symbol (format ":~a" f)))
                    (data-info-field-names info))
               (map (lambda (f) (string->symbol (format ":~a" f))) labels))
       stx)]))

;; Verify the labels recorded on an application's stx (from keyword
;; construction) against the head constructor's declared fields.
(define (validate-kw-labels! head labels stx env)
  (validate-kw-labels-for-ctor!
   (and (e:var? head) (e:var-name head)) labels stx env))

(define (infer-app/m head args stx env)
  (let ([kw-labels (and (syntax? stx) (syntax-property stx 'rackton:kw-labels))])
    (when kw-labels (validate-kw-labels! head kw-labels stx env)))
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
                                 (unify (normalize-type/guarded env head-ty-now)
                                        (normalize-type/guarded env expected-arrow)))])
                      (loop (cdr args)
                            (subst-compose s-u s-now)
                            (apply-subst s-u β)
                            (apply-subst/env s-arg env))))))])])))))

;; Parallel let: each rhs is typed in the env at let-entry (with
;; substitutions threaded), generalized, then made available.  The
;; binding-threading for/fold becomes a monadic named-let.
(define (infer-let/m bindings body env)
  ;; A binding's RHS has its own type, not the `let`'s result type, so the
  ;; expected type is cleared for the RHSs and re-established only for the
  ;; body (when it is itself a GADT-elim site).
  (let/infer ([exp (m:read-expected)])
   (let/infer ([acc (with-expected #f
                     (let loop ([bs bindings] [s empty-subst] [env-after env])
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
                                (loop (cdr bs) s-combined env-after*)))))])))])
    (let* ([s-acc (car acc)] [env-after (cdr acc)])
      (let/infer ([rb (with-expected (and (tail-elim-form? body) exp)
                                     (infer-expr/m body env-after))])
        (infer-return (cons (subst-compose (car rb) s-acc) (cdr rb))))))))

;; Mutual recursion: pre-bind each name with a fresh monomorphic tvar so each
;; rhs can reference every other binding (and itself).  After inferring all
;; rhs's, unify each tvar with the inferred type and generalize against the
;; OUTER env's free-var set.
(define (infer-letrec/m bindings body env)
 ;; As in `infer-let/m`: the RHSs carry no expected type; only the body
 ;; (if a GADT-elim site) inherits the enclosing result type.
 (let/infer ([exp (m:read-expected)])
  (with-expected #f
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
            (let/infer ([rb (with-expected (and (tail-elim-form? body) exp)
                                           (infer-expr/m body env-after))])
              (infer-return (cons (subst-compose (car rb) s-final) (cdr rb))))))))))))

;; Infer-monad arm.  Pattern for the conversion: `let/infer` binds each
;; recursive `infer-expr/m` result (a `subst . type` pair); a `let*` does the
;; pure work between binds (unify, subst-compose) and ends in the next monadic
;; computation.
(define (infer-if/m c t e stx env)
  ;; The condition is a Boolean (not the `if`'s result), so it never
  ;; carries the expected type; each branch DOES have the `if`'s result
  ;; type, so a branch that is itself a GADT-elim site inherits it.
  (let/infer ([exp (m:read-expected)])
   (let/infer ([rc (with-expected #f (infer-expr/m c env))])
    (let* ([s-c (car rc)] [t-c (cdr rc)]
           [s-cb
            (with-handlers
             ([exn:fail:unify?
               (lambda (_)
                 (raise-type-mismatch! (expr-stx c) t-bool (apply-subst s-c t-c)))])
             (unify (apply-subst s-c t-c) t-bool))]
           [s1 (subst-compose s-cb s-c)])
      (let/infer ([rt (with-expected (and (tail-elim-form? t) exp)
                                     (infer-expr/m t (apply-subst/env s1 env)))])
        (let* ([s-then (car rt)] [t-then (cdr rt)]
               [s2 (subst-compose s-then s1)])
          (let/infer ([re (with-expected (and (tail-elim-form? e) exp)
                                         (infer-expr/m e (apply-subst/env s2 env)))])
            (let* ([s-else (car re)] [t-else (cdr re)]
                   [s3 (subst-compose s-else s2)]
                   [s-branches
                    (with-handlers
                     ([exn:fail:unify?
                       (lambda (_)
                         (raise-type-mismatch! (expr-stx e)
                           (apply-subst s3 t-then) (apply-subst s3 t-else)))])
                     (unify (normalize-type/guarded env (apply-subst s3 t-then))
                            (normalize-type/guarded env (apply-subst s3 t-else))))]
                   [s-final (subst-compose s-branches s3)])
              (infer-return (cons s-final (apply-subst s-final t-then)))))))))))

(define (infer-ann/m expr ty-ast stx env)
  (kind-check-surface env ty-ast)
  (define resolved (resolve-type ty-ast env))
  (cond
    ;; An existential annotation is a PACK: hand off to the bidirectional
    ;; checker, which instantiates the hidden vars, checks `expr` against
    ;; the body, and discharges the `:where` constraints (see the
    ;; `texists?` branch of `check-expr/m`).
    [(texists? resolved)
     (check-expr/m expr env resolved)]
    [else
     (infer-ann-unify/m expr resolved stx env)]))

(define (infer-ann-unify/m expr resolved stx env)
  (let/infer ([re (infer-expr/m expr env)])
    (let* ([s-e (car re)] [t-e (cdr re)]
           [declared (qual-body-type resolved)]
           [s-u (with-handlers
                 ([exn:fail:unify?
                   (lambda (_)
                     (raise-type-mismatch! (expr-stx expr) declared (apply-subst s-e t-e)))])
                 (unify (normalize-type/guarded env (apply-subst s-e t-e))
                        (normalize-type/guarded env declared)))])
      (infer-return (cons (subst-compose s-u s-e) (apply-subst s-u declared))))))

;; `(open e (a … x) body)` — eliminate a first-class existential.  Mirror
;; the constructor-existential unpack: infer `e`, require a `texists`,
;; skolemize its hidden vars to fresh RIGID skolems, bind `x` to the
;; witness type and the packed constraints as hypotheses (so a method like
;; `show` resolves), infer `body`, then run the ESCAPE CHECK — no skolem
;; may appear in the body's result type (it would outlive its scope).
(define (infer-open/m e-expr tyvars valvar body stx env)
  (let/infer ([re (infer-expr/m e-expr env)])
    (let* ([s-e (car re)]
           [t-e (normalize-type/guarded env (apply-subst s-e (cdr re)))])
      (cond
        [(not (texists? t-e))
         (raise-syntax-error 'open
           (format "open expects a value of existential type, got ~a"
                   (pretty-type t-e))
           stx)]
        [else
         (match-define (texists vs ex-body) t-e)
         (unless (= (length tyvars) (length vs))
           (raise-syntax-error 'open
             (format "open binds ~a type variable(s) but the existential hides ~a"
                     (length tyvars) (length vs))
             stx))
         (define skolems
           (for/list ([tv (in-list tyvars)])
             (tcon (gensym (format "$ex-skolem.~a." tv)))))
         (define skolem-names (list->seteq (map tcon-name skolems)))
         (define sub
           (for/fold ([s empty-subst]) ([v (in-list vs)] [sk (in-list skolems)])
             (subst-extend s v sk)))
         (define ex-body* (apply-subst sub ex-body))
         (define hyps        (qual-constraints-of ex-body*))
         (define witness-ty  (qual-body-deep ex-body*))
         (define env* (env-extend-var env valvar (scheme '() witness-ty)))
         ;; The packed constraints are givens throughout the open body, so
         ;; an inner `let`/`letrec` generalized there may assume them.
         (let/infer ([rb (with-given-preds hyps (infer-expr/m body env*))])
           (let* ([s-acc (subst-compose (car rb) s-e)]
                  [t-body (apply-subst s-acc (cdr rb))])
             ;; Discharge pending preds the packed constraints prove (the
             ;; open is the proof), exactly as a match arm discharges an
             ;; existential constructor's hypotheses.
             (let/infer ([_ (if (null? hyps)
                                (infer-return (void))
                                (let/infer ([_ (m:apply-subst-to-preds s-acc)]
                                            [cur (m:snapshot-preds)])
                                  (m:set-preds
                                   (parameterize ([current-reduce-blame stx])
                                     (reduce-context env
                                                     (map (lambda (p) (apply-subst s-acc p)) hyps)
                                                     cur)))))])
               (let ()
                 ;; Escape check: the fresh skolem must not leak out in the
                 ;; result type.
                 (when (type-mentions-tcon-names? t-body skolem-names)
                   (raise-syntax-error 'open
                     (format "existential type escapes its scope: the result type ~a mentions the hidden type"
                             (pretty-type t-body))
                     stx))
                 (infer-return (cons s-acc t-body))))))]))))

;; Does `t` mention any tcon whose name is in `names` (a seteq)?  Used by
;; the `open` escape check against this site's fresh skolems.
(define (type-mentions-tcon-names? t names)
  (let walk ([t t])
    (match t
      [(tcon n)      (set-member? names n)]
      [(tvar _)      #f]
      [(tnat _)      #f]
      [(tapp h args) (or (walk h) (ormap walk args))]
      [(qual cs b)   (or (ormap (lambda (p) (ormap walk (pred-args p))) cs) (walk b))]
      [(tforall _ b) (walk b)]
      [(texists _ b) (walk b)]
      [_             #f])))

(define (infer-escape/m ty-ast vars stx env)
  (kind-check-surface env ty-ast)
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

;; ----- tuples -------------------------------------------------------
;; A tuple type is `(tapp (tcon 'Tuple) (list τ …))`: the built-in
;; `Tuple` constructor is variadic (its arity is the element count),
;; so ordinary tapp unification already gives elementwise, arity-checked
;; structural equality for free.

;; The head constructor name for a tuple of the given element count:
;; the binary tuple is `Pair` (so it doubles as a higher-kinded
;; constructor), every other arity is the variadic `Tuple`.  Used to
;; canonicalize so `(tapp (tcon 'Tuple) (list a b))` is never built —
;; `(Pair a b)` and the 2-`Tuple` are then literally the same type.
(define (tuple-head-for n) (if (= n 2) 'Pair 'Tuple))

;; Canonicalize a surface application head `n` applied to `args`: a
;; 2-arg `Tuple` becomes `Pair`; everything else is unchanged.
(define (tuple-head-name n args)
  (if (and (eq? n 'Tuple) (= (length args) 2)) 'Pair n))

;; Assemble the tuple type for the given element types.
(define (tuple-type-of elem-types)
  (make-tapp (tcon (tuple-head-for (length elem-types))) elem-types))

;; The element types of a concrete tuple type, or #f if `t` is not one.
;; Both heads denote tuples: `Tuple` (any arity) and `Pair` (arity 2).
(define (tuple-type-elems t)
  (match t
    [(tapp (tcon 'Tuple) elems)         elems]
    [(tapp (tcon 'Pair) (and es (list _ _))) es]
    [_ #f]))

;; Infer `(tuple e …)`: type each element left-to-right, threading the
;; running substitution through the env, and assemble the product type.
(define (infer-tuple/m elems stx env)
  (let loop ([elems elems] [s empty-subst] [tys-rev '()] [env env])
    (cond
      [(null? elems)
       (infer-return
        (cons s (apply-subst s (tuple-type-of (reverse tys-rev)))))]
      [else
       (let/infer ([r (infer-expr/m (car elems) env)])
         (let* ([s-e (car r)] [t-e (cdr r)]
                [s-now (subst-compose s-e s)])
           (loop (cdr elems) s-now (cons t-e tys-rev)
                 (apply-subst/env s-e env))))])))

;; The Rackton type a bit-segment's subject must have, given its declared
;; interpretation.  (Phase 1: integer / binary / bitstring.)
(define (bit-seg-subject-type ty)
  (case ty
    [(integer)   t-int]
    [(binary)    t-bytes]
    [(bitstring) t-bitstring]
    [else (error 'infer "unsupported bits segment type: ~a" ty)]))

;; `(bits seg …)` — every segment's subject is checked against the type its
;; interpretation demands; the whole form has type Bitstring.  A symbolic
;; (dependent) size is a runtime width only — it does not constrain typing
;; here — so segments are walked purely for their subject types.
(define (infer-bits/m segs stx env)
  (let loop ([segs segs] [s empty-subst])
    (cond
      [(null? segs) (infer-return (cons s t-bitstring))]
      [else
       (define sg (car segs))
       (let/infer ([r (infer-expr/m (bit-seg-subject sg) (apply-subst/env s env))])
         (let* ([s-e (car r)] [t-e (cdr r)]
                [s-now (subst-compose s-e s)]
                [expected (bit-seg-subject-type (bit-seg-type sg))]
                [s-u (with-handlers
                      ([exn:fail:unify?
                        (lambda (_)
                          (raise-type-mismatch! (expr-stx (bit-seg-subject sg))
                            (apply-subst s-now expected) (apply-subst s-now t-e)))])
                      (unify (apply-subst s-now t-e) (apply-subst s-now expected)))])
           (loop (cdr segs) (subst-compose s-u s-now))))])))

;; Infer `(tref t n)`: the target must resolve to a concrete tuple type
;; (its arity must be known), and the literal `n` must be in bounds.
;; The result is the n-th element type.
(define (infer-tref/m te idx stx env)
  (let/infer ([r (infer-expr/m te env)])
    (let* ([s (car r)] [t (apply-subst s (cdr r))]
           [elems (tuple-type-elems t)])
      (cond
        [(not elems)
         (raise-syntax-error 'infer
           (format "tref target must have a concrete tuple type, got ~a"
                   (pretty-type t))
           stx)]
        [(>= idx (length elems))
         (raise-syntax-error 'infer
           (format "tref index ~a is out of bounds for a ~a-element tuple"
                   idx (length elems))
           stx)]
        [else (infer-return (cons s (list-ref elems idx)))]))))

;; ----- fixed-size arrays --------------------------------------------
;; An array type is `(tapp (tcon 'Array) (list size elem))` where `size`
;; is a type-level Nat.  Construction and access mirror the tuple forms,
;; but arrays are homogeneous (one element type) and size-indexed.

;; `(array e …)` — every element unifies to one type; the size is the count.
(define (infer-array/m elems stx env)
  (define count (length elems))
  (let/infer ([β (m:fresh-tvar)])
    (let loop ([elems elems] [s empty-subst])
      (cond
        [(null? elems)
         (infer-return
          (cons s (apply-subst s (make-tapp (tcon 'Array) (list (tnat count) β)))))]
        [else
         (let/infer ([r (infer-expr/m (car elems) (apply-subst/env s env))])
           (let* ([s-e (car r)] [t-e (cdr r)]
                  [s-now (subst-compose s-e s)]
                  [s-u (with-handlers
                        ([exn:fail:unify?
                          (lambda (_)
                            (raise-type-mismatch! (expr-stx (car elems))
                              (apply-subst s-now β) (apply-subst s-now t-e)))])
                        (unify (apply-subst s-now t-e) (apply-subst s-now β)))])
             (loop (cdr elems) (subst-compose s-u s-now))))]))))

;; `(build-array n f)` — `n` literal; `f : (-> Integer a)` fills each slot.
(define (infer-build-array/m n proc stx env)
  (let/infer ([r (infer-expr/m proc env)])
    (let* ([s (car r)] [t-proc (cdr r)])
      (let/infer ([β (m:fresh-tvar)])
        (let* ([expected (make-arrow t-int β)]
               [s-u (with-handlers
                     ([exn:fail:unify?
                       (lambda (_)
                         (raise-type-mismatch! (expr-stx proc)
                           (apply-subst s expected) (apply-subst s t-proc)))])
                     (unify (apply-subst s t-proc) (apply-subst s expected)))]
               [s-final (subst-compose s-u s)])
          (infer-return
           (cons s-final
                 (apply-subst s-final
                              (make-tapp (tcon 'Array) (list (tnat n) β))))))))))

;; `(aref arr n)` — the target must be an array.  We UNIFY it with a fresh
;; `(Array size elem)` rather than demanding it already be one: inside a
;; size-polymorphic function (or an instance method) the target's type is
;; often still a bare variable when inference reaches the `aref`, only
;; pinned afterwards by unifying the body against the signature.  When the
;; size resolves to a concrete Nat the literal index is bounds-checked;
;; with a symbolic size the runtime element read is the safety net.
(define (infer-aref/m ae idx stx env)
  (let/infer ([r (infer-expr/m ae env)])
    (let* ([s (car r)] [t (apply-subst s (cdr r))])
      (let/infer ([elem (m:fresh-tvar)])
        (let/infer ([size (m:fresh-tvar)])
          (let* ([arr-ty (make-tapp (tcon 'Array) (list size elem))]
                 [s-u (with-handlers
                       ([exn:fail:unify?
                         (lambda (_)
                           (raise-syntax-error 'infer
                             (format "aref target must be an array, got ~a"
                                     (pretty-type t))
                             stx))])
                       (unify t arr-ty))]
                 [s-now (subst-compose s-u s)]
                 ;; Reduce a computed size like `(* 2 3)` to a literal so it
                 ;; stays bounds-checkable; a variable size stays symbolic.
                 [size* (normalize-nat-type (apply-subst s-now size))])
            (cond
              [(and (tnat? size*) (>= idx (tnat-value size*)))
               (raise-syntax-error 'infer
                 (format "aref index ~a is out of bounds for an array of size ~a"
                         idx (tnat-value size*))
                 stx)]
              [else (infer-return (cons s-now (apply-subst s-now elem)))])))))))

;; `(array-take k arr)` / `(array-drop k arr)` / `(array-split-at k arr)`
;; — the target must resolve to a CONCRETE-size array `(Array n elem)`;
;; the split point `k` is bounds-checked (0 ≤ k ≤ n) and the result
;; size(s) are computed.  take → (Array k elem); drop → (Array (n-k)
;; elem); split → (Pair (Array k elem) (Array (n-k) elem)).
(define (infer-array-slice/m op k ae stx env)
  (let/infer ([r (infer-expr/m ae env)])
    (let* ([s (car r)] [t (apply-subst s (cdr r))])
      (match t
        [(tapp (tcon 'Array) (list size elem))
         (define size* (normalize-nat-type size))
         (cond
           [(not (tnat? size*))
            (raise-syntax-error 'infer
              (format "array-~a needs an array of concrete size, got ~a"
                      op (pretty-type t))
              stx)]
           [(> k (tnat-value size*))
            (raise-syntax-error 'infer
              (format "array-~a point ~a exceeds array size ~a"
                      op k (tnat-value size*))
              stx)]
           [else
            (define n (tnat-value size*))
            (define (arr sz) (make-tapp (tcon 'Array) (list (tnat sz) elem)))
            (define result
              (case op
                [(take)  (arr k)]
                [(drop)  (arr (- n k))]
                [(split) (make-tapp (tcon 'Pair) (list (arr k) (arr (- n k))))]))
            (infer-return (cons s result))])]
        [_
         (raise-syntax-error 'infer
           (format "array-~a target must have a concrete array type, got ~a"
                   op (pretty-type t))
           stx)]))))

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
;; A constructor is GADT-style when its result type FIXES an index — its
;; result is `(T … concrete-or-repeated …)` rather than `(T v1 … vn)` with
;; distinct bound tvars (the generic shape).  Matching such a constructor
;; learns a local index equality; when several arms refine the SAME
;; scrutinee index and no result type pins each arm independently, the
;; arms collide and the conflict surfaces as a confusing mismatch.
(define (ctor-scheme-gadt? sch)
  (match sch
    [(scheme vs body)
     (define result
       (let loop ([t (qual-body-deep body)])
         (if (arrow? t) (loop (arrow-cod t)) t)))
     (match result
       [(tapp (tcon _) args)
        (not (and (andmap tvar? args)
                  (let ([ns (map tvar-name args)])
                    (and (andmap (lambda (n) (memq n vs)) ns)
                         (= (length ns) (length (remove-duplicates ns)))))))]
       [_ #f])]))

(define (gadt-ctor-pattern? env pat)
  (and (p:ctor? pat)
       (let ([di (env-ref-data env (p:ctor-name pat))])
         (and di (ctor-scheme-gadt? (data-info-scheme di))))))

;; Appended to a match error when a GADT constructor is involved: such a
;; match's arms refine the scrutinee's index and so cannot be unified with
;; one another — each needs to be checked against a known result type.
(define gadt-match-hint
  (string-append
   "\n  note: this is a GADT match — its arms refine the scrutinee's index, "
   "so they\n  cannot be reconciled with each other.  Give the enclosing "
   "definition a type\n  signature (keeping the match in its tail position), "
   "or factor the match into its\n  own signed helper, so each arm is checked "
   "against the declared result type."))

(define (infer-clause/m cl scrut-type result-type env [earlier-arms? #t])
  (let/infer ([rp (infer-pattern/m (clause-pattern cl) env)])
   (let ()
   (define-values (bindings pat-type ex-hyps)
    (values (car rp) (cadr rp) (caddr rp)))
  ;; Reduce any ground type-level family application (e.g. a per-address
  ;; `(ShapeAt 0)` exposed by an earlier index refinement) on both sides
  ;; before matching, so the pattern unifies against the reduced shape.
  (define pat-type*   (normalize-type/guarded env pat-type))
  (define scrut-type* (normalize-type/guarded env scrut-type))
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
                (string-append
                 (format "pattern type ~a does not match scrutinee type ~a"
                         (pretty-type pat-type*) (pretty-type scrut-type*))
                 (if (gadt-ctor-pattern? env (clause-pattern cl)) gadt-match-hint ""))
                (clause-stx cl)))])
          (gadt-unify pat-type* scrut-type*)))])
     (values (unify pat-type* scrut-type*) (hash))))
  ;; Refine the whole arm-local env by the GADT skolem-subst: both the
  ;; pre-existing in-scope bindings and this pattern's own bindings get
  ;; the learned index equality.  Scoped to this arm only (env* is built
  ;; fresh from the outer env), so later arms are unaffected.
  (define env*
    (apply-skolem-subst/env
     arm-skolem-subst
     (for/fold ([e (apply-subst/env s-pat env)])
               ([b (in-list bindings)])
       (env-extend-var e (car b)
                       (scheme '() (apply-subst s-pat (cdr b)))))))
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
      ;; An existential-constructor pattern brings its packed
      ;; constraints (`ex-hyps`) into the arm as givens, so an inner
      ;; `let`/`letrec` generalized in the arm body may assume them.
      ;; Non-existential clauses have `ex-hyps` empty, so this is a no-op
      ;; for them.
      (let/infer ([rb (with-given-preds ex-hyps
                        (infer-expr/m (clause-body cl) env-pre-body))])
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
                                (parameterize ([current-reduce-blame (clause-stx cl)])
                                  (reduce-context env
                                                  (map (lambda (p) (apply-subst s-acc p)) ex-hyps)
                                                  current)))))])
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
                   (string-append
                    (format (if earlier-arms?
                                "match clause body has type ~a but earlier arms have ~a"
                                "match clause body has type ~a but the expected result type is ~a")
                            got exp)
                    (if (gadt-ctor-pattern? env (clause-pattern cl)) gadt-match-hint ""))
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
    [(p:tuple args stx)
     ;; A tuple pattern is structural: infer each sub-pattern, thread the
     ;; substitution, and assemble the `(Tuple …)` type.  No ctor lookup
     ;; or arity table — the arity is the sub-pattern count, unified with
     ;; the scrutinee's tuple type by the caller.
     (let/infer ([acc (let loop ([args args] [bindings '()] [tys-rev '()]
                                 [s empty-subst] [hyps '()])
                        (cond
                          [(null? args)
                           (infer-return (list bindings (reverse tys-rev) s hyps))]
                          [else
                           (let/infer ([rp (infer-pattern/m (car args) env)])
                             (let* ([bs (car rp)] [t (cadr rp)] [inner-hyps (caddr rp)])
                               (loop (cdr args)
                                     (append bindings bs)
                                     (cons t tys-rev)
                                     s
                                     (append hyps inner-hyps))))]))])
       (let* ([all-bindings (car acc)] [elem-tys (cadr acc)]
              [s-acc (caddr acc)] [all-hyps (cadddr acc)])
         (infer-return
          (list (for/list ([b (in-list all-bindings)])
                  (cons (car b) (apply-subst s-acc (cdr b))))
                (apply-subst s-acc (tuple-type-of elem-tys))
                all-hyps))))]
    [(p:bits segs stx)
     ;; A bits pattern is structural: each segment's subject is checked
     ;; against the type its interpretation demands (integer→Integer,
     ;; binary→Bytes, bitstring→Bitstring); the whole matches a Bitstring.
     ;; A symbolic (dependent) size is a runtime width, resolved by an
     ;; earlier segment's binding at codegen — it places no type constraint
     ;; here.  Bindings accumulate left to right.
     (let/infer ([acc (let loop ([segs segs] [bindings '()] [s empty-subst] [hyps '()])
                        (cond
                          [(null? segs) (infer-return (list bindings s hyps))]
                          [else
                           (define sg (car segs))
                           (let/infer ([rp (infer-pattern/m (bit-seg-subject sg) env)])
                             (let* ([bs (car rp)] [t (cadr rp)] [inner-hyps (caddr rp)]
                                    [expected (bit-seg-subject-type (bit-seg-type sg))]
                                    [s-u (unify (apply-subst s t) (apply-subst s expected))])
                               (loop (cdr segs)
                                     (append bindings bs)
                                     (subst-compose s-u s)
                                     (append hyps inner-hyps))))]))])
       (let* ([all-bindings (car acc)] [s-acc (cadr acc)] [all-hyps (caddr acc)])
         (infer-return
          (list (for/list ([b (in-list all-bindings)])
                  (cons (car b) (apply-subst s-acc (cdr b))))
                t-bitstring
                all-hyps))))]
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
        ;; A keyword pattern `(C :f p …)` records its labels on stx;
        ;; verify they match the constructor's declared fields in order.
        (let ([kw-labels (and (syntax? stx)
                              (syntax-property stx 'rackton:kw-labels))])
          (when kw-labels
            (validate-kw-labels-for-ctor! name kw-labels stx env)))
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
;; A pattern that matches every value of its type: a wildcard, a
;; variable, or a tuple of irrefutable sub-patterns (a tuple type has a
;; single fixed-arity shape, so destructuring it can never fail).
(define (irrefutable-pat? p)
  (match p
    [(p:wild _)     #t]
    [(p:var _ _)    #t]
    [(p:tuple ps _) (andmap irrefutable-pat? ps)]
    [_              #f]))

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
         (irrefutable-pat? (clause-pattern c))))
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

;; ----- variadic-call gathering -------------------------------------
;;
;; After the env's `variadics` table is complete (Phase A, plus any
;; imported entries), rewrite every DIRECT call of a variadic name so
;; its trailing arguments are collected into a rest-list.  A call
;; `(f a₀ … a_{k-1} x … z)` of a variadic `f` with `k` fixed parameters
;; becomes `(f a₀ … a_{k-1} (Cons x … (Cons z Nil)))` — a saturated,
;; ordinary curried application against `f`'s binary core type.  Both
;; inference and codegen then run on this rewritten AST, oblivious to
;; the variadicity.  The pass also drops the now-consumed `top:variadic`
;; markers.
;;
;; Local bindings shadow: a `let` / `lambda` / `match` that rebinds a
;; variadic name suppresses gathering for that name in its scope, so a
;; shadowing local function is called by ordinary currying.

(define (hash-remove* h keys)
  (for/fold ([h h]) ([k (in-list keys)]) (hash-remove h k)))

;; Build the AST of a list literal `(Cons e₀ … (Cons e_n Nil))`.
(define (build-cons-list elems stx)
  (foldr (lambda (el acc) (e:app (e:var 'Cons stx) (list el acc) stx))
         (e:var 'Nil stx)
         elems))

;; Names bound by a core pattern.
(define (pattern-vars p)
  (match p
    [(p:var n _)      (list n)]
    [(p:ctor _ as _)  (append-map pattern-vars as)]
    [(p:tuple es _)   (append-map pattern-vars es)]
    [(p:bits segs _)  (append-map (lambda (sg) (pattern-vars (bit-seg-subject sg))) segs)]
    [_                '()]))

(define (gather-variadic-calls forms env)
  (define vmap (env-variadics env))
  (if (hash-empty? vmap)
      (filter (lambda (f) (not (top:variadic? f))) forms)
      (filter-map (lambda (f) (gv-top f vmap)) forms)))

;; Rewrite one top-form (or drop it, returning #f, for a consumed marker).
(define (gv-top f vmap)
  (cond
    [(top:variadic? f) #f]
    [(top:def? f)
     (top:def (top:def-name f) (gv-expr (top:def-expr f) vmap) (top:def-stx f))]
    [(top:instance? f)
     (top:instance (top:instance-context f) (top:instance-head f)
                   (map (lambda (m) (gv-method m vmap)) (top:instance-methods f))
                   (top:instance-stx f))]
    [(top:derive-instance? f)
     (top:derive-instance (top:derive-instance-context f) (top:derive-instance-head f)
                          (map (lambda (m) (gv-method m vmap))
                               (top:derive-instance-methods f))
                          (top:derive-instance-stx f))]
    [else f]))

;; An instance method is a `top:def` (or an `inst-type-fam`, passed through).
(define (gv-method m vmap)
  (if (top:def? m)
      (top:def (top:def-name m) (gv-expr (top:def-expr m) vmap) (top:def-stx m))
      m))

(define (gv-expr e vmap)
  (define (R x) (gv-expr x vmap))
  (match e
    [(e:app head args stx)
     (define head* (R head))
     (define args* (map R args))
     (define k (and (e:var? head) (hash-ref vmap (e:var-name head) #f)))
     (cond
       [(not k) (e:app head* args* stx)]
       [else
        ;; A truly zero-argument call `(f)` was desugared to a single
        ;; implicit `Unit` argument; for a variadic head that means zero
        ;; user arguments, so the rest-list is empty.
        (define zero-arg?
          (and (= (length args*) 1)
               (e:var? (car args*))
               (eq? (e:var-name (car args*)) 'Unit)))
        (define user-args (if zero-arg? '() args*))
        (cond
          [(>= (length user-args) k)
           (define-values (fixed rest) (split-at user-args k))
           (e:app head* (append fixed (list (build-cons-list rest stx))) stx)]
          ;; Too few arguments to satisfy the fixed parameters: leave the
          ;; call as-is, so it reads as an ordinary (partial) application.
          [else (e:app head* args* stx)])])]
    [(e:lam params body stx)
     (e:lam params (gv-expr body (hash-remove* vmap params)) stx)]
    [(e:let bs body stx)
     ;; Parallel let: each rhs is in the OUTER scope; the names shadow in
     ;; the body only.
     (define names (map car bs))
     (e:let (for/list ([b (in-list bs)]) (cons (car b) (R (cdr b))))
            (gv-expr body (hash-remove* vmap names)) stx)]
    [(e:letrec bs body stx)
     ;; Recursive: the names shadow in every rhs and in the body.
     (define vmap* (hash-remove* vmap (map car bs)))
     (e:letrec (for/list ([b (in-list bs)]) (cons (car b) (gv-expr (cdr b) vmap*)))
               (gv-expr body vmap*) stx)]
    [(e:if a b c stx)        (e:if (R a) (R b) (R c) stx)]
    [(e:open e tvs vv body stx)
     (e:open (R e) tvs vv (gv-expr body (hash-remove vmap vv)) stx)]
    [(e:ann ex t stx)        (e:ann (R ex) t stx)]
    [(e:match scrut cs irr stx)
     (e:match (R scrut) (for/list ([c (in-list cs)]) (gv-clause c vmap)) irr stx)]
    [(e:match* scruts cs irr stx)
     (e:match* (map R scruts)
               (for/list ([c (in-list cs)]) (gv-clause* c vmap)) irr stx)]
    [(e:tuple es stx)        (e:tuple (map R es) stx)]
    [(e:bits segs stx)       (e:bits (map (lambda (sg) (gv-bit-seg sg vmap)) segs) stx)]
    [(e:tref t i stx)        (e:tref (R t) i stx)]
    [(e:update rec ups stx)
     (e:update (R rec)
               (for/list ([u (in-list ups)]) (cons (car u) (R (cdr u)))) stx)]
    [(e:array es stx)        (e:array (map R es) stx)]
    [(e:build-array n p stx) (e:build-array n (R p) stx)]
    [(e:aref a i stx)        (e:aref (R a) i stx)]
    [(e:array-slice op i a stx) (e:array-slice op i (R a) stx)]
    [(e:handle ex cs ret stx)
     (e:handle (R ex) (map (lambda (c) (gv-handle-clause c vmap)) cs)
               (gv-handle-return ret vmap) stx)]
    ;; e:escape splices raw Racket — leave it untouched; e:literal /
    ;; e:var / type nodes have no nested expressions to rewrite.
    [_ e]))

;; Rewrite variadic calls in a bit-segment's construction subject.
(define (gv-bit-seg sg vmap)
  (bit-seg (gv-expr (bit-seg-subject sg) vmap)
           (bit-seg-size sg) (bit-seg-type sg)
           (bit-seg-signed? sg) (bit-seg-endian sg) (bit-seg-stx sg)))

;; A `match` clause: the pattern's variables shadow in its guard and body.
(define (gv-clause c vmap)
  (define vmap* (hash-remove* vmap (pattern-vars (clause-pattern c))))
  (clause (clause-pattern c)
          (and (clause-guard c) (gv-expr (clause-guard c) vmap*))
          (gv-expr (clause-body c) vmap*)
          (clause-stx c)))

(define (gv-clause* c vmap)
  (define vmap* (hash-remove* vmap (append-map pattern-vars (clause*-patterns c))))
  (clause* (clause*-patterns c)
           (and (clause*-guard c) (gv-expr (clause*-guard c) vmap*))
           (gv-expr (clause*-body c) vmap*)
           (clause*-stx c)))

(define (gv-handle-clause c vmap)
  (define vmap* (hash-remove* vmap
                              (cons (handle-clause-k-name c) (handle-clause-params c))))
  (handle-clause (handle-clause-op c) (handle-clause-params c)
                 (handle-clause-k-name c)
                 (gv-expr (handle-clause-body c) vmap*)
                 (handle-clause-stx c)))

(define (gv-handle-return r vmap)
  (define vmap* (hash-remove* vmap (list (handle-return-var r))))
  (handle-return (handle-return-var r)
                 (gv-expr (handle-return-body r) vmap*)
                 (handle-return-stx r)))

;; Like `infer-program`, but also returns the post-expansion form list
;; (with every `:derive-supers` instance replaced by the plain
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
  (define-values (env* _ forms* final-st)
    (infer-program/phases forms env (hasheq) #:report-dangling-decs? #t))
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
(define (infer-program/phases forms env prior-declared [st0 (make-infer-state)]
                              #:report-dangling-decs? [report-dangling? #f])
  ;; ---- Phase A: type infrastructure ----
  (define-values (env-after-A declared)
    (run-phase-A env forms prior-declared))
  ;; ---- Cross-class derivation expansion ----
  ;; Now that every class (prelude, local, imported) is in env, rewrite
  ;; each `:derive-supers` instance into the plain instances it
  ;; synthesizes.  Every later phase — and codegen — runs over `forms*`.
  (define forms*0 (expand-derive-instances forms env-after-A))
  ;; ---- Variadic-call gathering ----
  ;; With env's `variadics` table complete, rewrite each direct call of a
  ;; variadic name to collect its trailing args into a rest-list, and drop
  ;; the consumed `top:variadic` markers.  Every later phase — and codegen
  ;; — runs over this rewritten list.
  (define forms* (gather-variadic-calls forms*0 env-after-A))
  ;; ---- Superclass existence ----
  ;; Every superclass a protocol names must be a class that actually
  ;; exists.  Checked here, after Phase A, so a forward reference (a
  ;; subclass declared before its superclass) and an imported
  ;; superclass both resolve; only a genuinely undefined name — a typo,
  ;; or a non-class identifier — is flagged.
  (check-superclass-existence env-after-A forms*)
  ;; ---- Dangling type signatures ----
  ;; A `(: name τ)` with no matching definition is a typo; reject it.
  ;; Whole-module callers opt in; the prelude and REPL do not (see
  ;; `check-dangling-decs`).
  (when report-dangling? (check-dangling-decs forms*))
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
  ;; ---- Protocol laws ----
  ;; Type-check each protocol's `:laws` now that every class AND
  ;; instance is in env, so a law may compare results at concrete types
  ;; (e.g. `(Num Integer)`) or rely on derived instances.
  (check-class-laws env-after-C forms*)
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
;; A top-level type signature `(: name τ)` (a `top:dec`) must have a
;; matching definition somewhere in the same form list: a `(define name
;; …)`, or a `foreign`/`foreign-c` host import that supplies the binding.
;; A signature with no such definition is "dangling" — almost always a
;; typo on the signature's or the define's name — and is rejected.
;;
;; Only top-level decs are checked: protocol method signatures parse as
;; `method-sig` (inside `top:class`), not `top:dec`, so they are never
;; flagged.  This runs only for whole-module compilation (opted in via
;; `infer-program/phases`' `#:report-dangling-decs?`); the prelude (whose
;; `mconcat`/`array-map`/`enum-from-to` are intentionally bare decs backed
;; by `prelude-runtime`) and the REPL (where a `(: …)` may precede its
;; define by several inputs) are exempt.
(define (check-dangling-decs forms)
  (define defined
    (for/seteq ([f (in-list forms)]
                #:when (or (top:def? f) (top:foreign? f) (top:foreign-c? f)))
      (cond [(top:def? f)       (top:def-name f)]
            [(top:foreign? f)   (top:foreign-name f)]
            [else               (top:foreign-c-name f)])))
  (for ([f (in-list forms)] #:when (top:dec? f)
        #:unless (set-member? defined (top:dec-name f)))
    (raise-syntax-error 'infer
      (format (string-append "type signature for ~a has no matching definition"
                             "\n  add a (define ~a …) or remove the signature")
              (top:dec-name f) (top:dec-name f))
      (top:dec-stx f))))

(define (check-superclass-existence env forms)
  (for* ([f (in-list forms)] #:when (top:class? f)
         [s (in-list (top:class-supers f))]
         #:unless (eq? (constraint-class s) '~))
    (unless (env-ref-class env (constraint-class s) #f)
      (raise-syntax-error 'infer
        (format "protocol ~a: superprotocol ~a is not a defined protocol"
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
;; Type-check every protocol's `:laws` against the fully-elaborated
;; env.  Run after instance registration so a law may compare results at
;; concrete element types — `(Num Integer)`, `(Eq (List Integer))` — and
;; rely on derived instances.  The class's parameters and superclass
;; predicates are read back from its registered `class-info`.
(define (check-class-laws env forms)
  (for ([f (in-list forms)] #:when (top:class? f))
    (define cname (constraint-class (top:class-head f)))
    (define cinfo (env-ref-class env cname #f))
    (when cinfo
      (for ([law (in-list (class-info-laws cinfo))])
        (check-class-law law cname
                         (class-info-params cinfo)
                         (class-info-supers cinfo)
                         env)))))

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
;; A `:derive-supers` instance bundles only the irreducible
;; primitives (e.g. `pure` + `flatmap`).  Rewrite it into plain
;; `top:instance` forms: one synthesized instance per MISSING superclass
;; (filling its methods from the deriving class's `:derive` table and
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
    (raise-syntax-error 'derive-supers
      (format ":derive-supers on an instance of unknown protocol ~a" C)
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
  ;; A3c: constraint synonyms.  Resolve each synonym's components to core
  ;; preds and register them, before class methods / data ctors / decs
  ;; (whose signatures may use a synonym in a `=>` context).
  (define env-A3c (register-constraint-syns env-A3 forms))
  ;; A3d: constraint families (same timing rationale as synonyms).
  (define env-A3d (register-constraint-fams env-A3c forms))
  (define env-A4
    (for/fold ([e env-A3d]) ([f (in-list forms)] #:when (top:struct-fields? f))
      (env-extend-struct-fields e (top:struct-fields-struct-name f)
                                (top:struct-fields-field-names f))))
  ;; A4.4: DataKinds promotion.  Lift eligible monomorphic datatypes to
  ;; the kind level so the next pass can infer the kinds of types indexed
  ;; by them (e.g. a stack-machine `Code` indexed by promoted stack
  ;; shapes).  Runs after tcon shells (A2) so datatype arities are known.
  (define env-A4.4 (promote-data env-A4 forms))
  ;; A4.5: infer each data type's kind from its constructor field types
  ;; (replacing the arity-placeholder kinds on the shells).  Runs after
  ;; aliases (A3) — field types may use them — and before any kind-
  ;; checked resolution of a type that mentions these constructors.
  (define env-A4.5 (infer-data-kinds env-A4.4 forms))
  ;; A4.6: standalone type families.  Register declarations + open
  ;; equations so later type resolution (class methods, data ctors, def
  ;; signatures) can reduce family applications; then infer each family's
  ;; kind from its clauses so its applications are kind-checked.
  (define env-A4.6 (infer-tyfam-kinds (register-type-families env-A4.5 forms) forms))
  ;; A4.7: infer each data family's kind from its instance heads (result
  ;; always *), so a family indexed by a promoted tag kind-checks.
  (define env-A4.7 (infer-data-family-kinds env-A4.6 forms))
  ;; A5: effects (resolve op types against the tcon-complete env).
  (define env-A5
    (for/fold ([e env-A4.7]) ([f (in-list forms)] #:when (top:effect? f))
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
  ;; A7b: data-instance constructors (their result type is the instance
  ;; head; the family tcon shell already exists from A2).
  (define env-A7b
    (resolve-data-instance-ctors env-A7 forms))
  ;; Data family instances must be coherent (non-overlapping heads).
  (check-data-family-coherence env-A7b forms)
  ;; A8: resolve every top:dec into the shared declared table; mirror
  ;; the entry into env so the rest of the pipeline can env-ref-var
  ;; before the def's body has been inferred.
  (define declared
    (for/fold ([d prior-declared]) ([f (in-list forms)] #:when (top:dec? f))
      ;; env-A6 carries the imported (A1) and local (A3) aliases that the
      ;; declared types may reference; the bare `env` does not.
      (hash-set d (top:dec-name f) (resolve-scheme (top:dec-type f) env-A6))))
  (define env-A8
    (for/fold ([e env-A7b]) ([(name sch) (in-hash declared)])
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
  ;; A10: variadic-arity markers (from `...` signatures and dotted
  ;; defines).  A name may carry both — a signature and its definition —
  ;; in which case their fixed-arg counts must agree.
  (define env-A10
    (for/fold ([e env-A9]) ([f (in-list forms)] #:when (top:variadic? f))
      (define name (top:variadic-name f))
      (define k    (top:variadic-arity f))
      (define prev (env-ref-variadic e name #f))
      (when (and prev (not (= prev k)))
        (raise-syntax-error 'rackton
          (format (string-append "variadic arity mismatch for ~a: its signature "
                                 "and definition disagree on the fixed-argument "
                                 "count (~a vs ~a)")
                  name prev k)
          (top:variadic-stx f)))
      (env-extend-variadic e name k)))
  (values env-A10 declared))

;; Pre-register every top:data's tcon header in env: name + arity +
;; full ctor-name list + abstract flag.  Ctor schemes are resolved
;; separately in `resolve-data-ctors` after every tcon (and class) is
;; in env.  Pre-registering the ctor name list (not just the count)
;; matches what `env-extend-tcon`'s final state would look like, so
;; `env-ref-tcon` answers correctly during type resolution of other
;; forms' bodies.
(define (pre-register-tcon-shells env forms)
  ;; A data family's constructors live in its (separate) data-instance
  ;; forms; collect them per family so the family tcon shell lists them
  ;; (exhaustiveness, ,info).
  (define fam-ctors
    (for/fold ([h (hasheq)]) ([f (in-list forms)] #:when (top:data-instance? f))
      (hash-update h (top:data-instance-name f)
                   (lambda (acc)
                     (append acc (map data-ctor-name (top:data-instance-ctors f))))
                   '())))
  (define e1
    (for/fold ([e env]) ([f (in-list forms)] #:when (top:data? f))
      (define tname    (top:data-name f))
      (define tparams  (top:data-params f))
      (define ctors    (top:data-ctors f))
      (define abstract? (top:data-abstract? f))
      (env-extend-tcon e tname
                       (tcon-info tname (length tparams)
                                  ;; Placeholder kind scheme; Phase A2.5
                                  ;; (infer-data-kinds) replaces it with the
                                  ;; inferred kind before any type is checked.
                                  (kscheme-mono (arity->star-kind (length tparams)))
                                  (for/list ([c (in-list ctors)])
                                    (data-ctor-name c))
                                  abstract?
                                  (top:data-runtime-tag f)))))
  ;; Data family tcon shells (no constructors of their own).
  (for/fold ([e e1]) ([f (in-list forms)] #:when (top:data-family? f))
    (define name   (top:data-family-name f))
    (define params (top:data-family-params f))
    (env-extend-tcon e name
                     (tcon-info name (length params)
                                (kscheme-mono (arity->star-kind (length params)))
                                (hash-ref fam-ctors name '())
                                #f #f))))

;; ----- kinds: the elaboration walk -----------------------------------

;; The kinds of the primitive type constructors that are never
;; registered as data: the scalar types and the function arrow.  All
;; other constructors carry their kind in tcon-info.
(define primitive-kind-table
  (hasheq 'Integer kstar 'Boolean kstar 'String kstar 'Float kstar
          '-> (kind-arrow* (list kstar kstar) kstar)
          ;; Type-level Nat arithmetic operators: Nat -> Nat -> Nat.
          '+ (kind-arrow* (list (kind-nat) (kind-nat)) (kind-nat))
          '* (kind-arrow* (list (kind-nat) (kind-nat)) (kind-nat))
          ;; Fixed-size array: size (Nat) then element type (*).
          'Array (kind-arrow* (list (kind-nat) kstar) kstar)))

;; Is `name` a primitive scalar type constructor (Integer, Boolean,
;; String, Float)?  The function arrow `->` shares the kind table but is
;; not a scalar a user would inspect, so it is excluded.
(define (primitive-type? name)
  (and (hash-has-key? primitive-kind-table name)
       (not (memq name '(-> + *)))))

;; The kind of type constructor `name`: a batch seed (during
;; data-kind inference) wins, then the env's stored kind, then the
;; primitive table; #f when unknown (a resolved type should never
;; mention an unknown tcon, so callers may treat #f leniently).
;; Stored kinds may be kind-schemes (tcon-info, promoted-ctors); a fresh
;; instantiation per use site is what lets a kind-polymorphic constructor
;; kind-check at different kinds in different places.  Bare kinds (batch
;; seeds, primitives) pass through `instantiate-kind` unchanged.
(define (tcon-kind-of env batch-kinds name)
  (define k
    (or (hash-ref batch-kinds name #f)
        (let ([ti (env-ref-tcon env name #f)]) (and ti (tcon-info-kind ti)))
        ;; A standalone type family's inferred kind (Feature 1, Phase 2).
        (let ([fi (env-ref-tyfam env name #f)]) (and fi (tyfam-info-kind fi)))
        ;; A DataKinds-promoted type-level constructor (TInt, SPush, …).
        ;; Checked after real tcons so an ordinary type of the same name
        ;; always wins — promotion never reinterprets an existing type.
        (env-ref-promoted-ctor env name #f)
        (hash-ref primitive-kind-table name #f)))
  (and k (instantiate-kind k)))

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
    [(tnat _) (values (kind-nat) s)]
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
     ;; A concrete head kind that accepts fewer arguments than supplied
     ;; gets a precise message (over-application of a constructor, or a
     ;; `*`-kinded type applied at all); other failures fall through to
     ;; the generic kind-unify error.
     (define kh* (apply-ksubst s2 kh))
     (when (and (set-empty? (kind-vars kh*))
                (< (kind-arity kh*) (length args)))
       (raise (exn:fail:kind-unify
               (if (zero? (kind-arity kh*))
                   (format "~a has kind ~s and cannot be applied"
                           (type-head-name h) (kind->datum kh*))
                   (format "~a has kind ~s but is applied to ~a argument~a"
                           (type-head-name h) (kind->datum kh*)
                           (length args) (if (= (length args) 1) "" "s")))
               (current-continuation-marks) kh* expected)))
     (define s3 (ksubst-compose
                 (unify-kind kh* (apply-ksubst s2 expected))
                 s2))
     (values (apply-ksubst s3 result) s3)]
    [(tforall vs body)
     (for ([v (in-list vs)]) (hash-set! tvar-kinds v (kvar (gensym 'k))))
     (elab-kind body env batch-kinds tvar-kinds s)]
    [(texists vs body)
     (for ([v (in-list vs)]) (hash-set! tvar-kinds v (kvar (gensym 'k))))
     (elab-kind body env batch-kinds tvar-kinds s)]
    [(qual _cs body)
     ;; The predicates' own kinds are checked at their resolution sites;
     ;; the qualified type's kind is its body's.
     (elab-kind body env batch-kinds tvar-kinds s)]
    [_ (values (kvar (gensym 'k)) s)]))

;; ----- kinds: the surface-AST checker --------------------------------
;; Kind-checking walks the SURFACE type AST, where every node carries
;; its own syntax — so a kind error blames the exact offending
;; sub-expression (line and column), however deeply nested.  An alias
;; application is expanded through resolve-type and the core walker
;; (elab-kind), blaming the alias use site.

;; Rollout control: 'off disables checking, 'warn logs ill-kinded
;; types to stderr (used to surface false positives across a full
;; build), 'error rejects them.
(define current-kind-check (make-parameter 'error))

(define (ty-ast->stx t)
  (match t
    [(ty:var _ stx)      stx]
    [(ty:con _ stx)      stx]
    [(ty:nat _ stx)      stx]
    [(ty:app _ _ stx)    stx]
    [(ty:forall _ _ stx) stx]
    [(ty:exists _ _ stx) stx]
    [(ty:qual _ _ stx)   stx]
    [_                   #f]))

(define (blame-suffix stx)
  (if (and (syntax? stx) (syntax-source stx) (syntax-line stx))
      (format " (~a:~a)" (syntax-source stx) (syntax-line stx))
      ""))

;; A readable name for the head of an application (surface or core).
(define (type-head-name t)
  (match t
    [(ty:con n _)   n]
    [(ty:var n _)   n]
    [(ty:app h _ _) (type-head-name h)]
    [(tcon n)       n]
    [(tvar n)       n]
    [(tapp h _)     (type-head-name h)]
    [_              "this type"]))

;; Report an ill-kinded type at `stx`: skip when off, log when warn,
;; raise a located syntax error when enforcing.
(define (kind-fail! stx msg)
  (case (current-kind-check)
    [(off)  (void)]
    [(warn) (eprintf "rackton kind warning: ~a~a\n" msg (blame-suffix stx))]
    [else   (raise-syntax-error 'infer (format "kind error: ~a" msg) stx)]))

;; Infer the kind of a surface type AST, reporting any ill-kinded
;; application at the offending node's syntax.  `tvar-kinds` is a
;; mutable name→kind hash, seeded with the in-scope variables and
;; auto-extended for the rest; `batch-kinds` holds data-type seed kinds
;; during data-kind inference (empty otherwise).  Returns
;; (values kind ksubst).
(define (elab-surface t env batch-kinds tvar-kinds s)
  (match t
    [(ty:var n _)
     (values (hash-ref! tvar-kinds n (lambda () (kvar (gensym 'k)))) s)]
    [(ty:nat _ _)
     ;; A type-level natural literal has kind `Nat`.
     (values (kind-nat) s)]
    [(ty:con n stx)
     (if (env-ref-alias env n #f)
         (elab-alias t env batch-kinds tvar-kinds s)
         (values (or (tcon-kind-of env batch-kinds n) (kvar (gensym 'k))) s))]
    [(ty:app h args stx)
     (cond
       [(and (ty:con? h) (env-ref-alias env (ty:con-name h) #f))
        (elab-alias t env batch-kinds tvar-kinds s)]
       ;; `Tuple` is variadic: it accepts any number of `*`-kinded
       ;; arguments and yields `*`.  A fixed arrow kind can't express
       ;; "any arity", so demand each element be `*` directly rather
       ;; than unifying the head against a built arrow.
       [(and (ty:con? h) (eq? (ty:con-name h) 'Tuple))
        (define s*
          (for/fold ([s s]) ([a (in-list args)])
            (define-values (ka s1) (elab-surface a env batch-kinds tvar-kinds s))
            (with-handlers ([exn:fail:kind-unify?
                             (lambda (e) (kind-fail! (ty-ast->stx a) (exn-message e)) s1)])
              (ksubst-compose (unify-kind (apply-ksubst s1 ka) kstar) s1))))
        (values kstar s*)]
       [else
        (define-values (kh s1) (elab-surface h env batch-kinds tvar-kinds s))
        (define-values (kargs s2)
          (for/fold ([acc '()] [s s1] #:result (values (reverse acc) s))
                    ([a (in-list args)])
            (define-values (ka s*) (elab-surface a env batch-kinds tvar-kinds s))
            (values (cons ka acc) s*)))
        (define kh* (apply-ksubst s2 kh))
        ;; A concrete head kind that accepts fewer arguments than
        ;; supplied gets a precise message at this application's node.
        (when (and (set-empty? (kind-vars kh*))
                   (< (kind-arity kh*) (length args)))
          (kind-fail! stx
            (if (zero? (kind-arity kh*))
                (format "~a has kind ~s and cannot be applied"
                        (type-head-name h) (kind->datum kh*))
                (format "~a has kind ~s but is applied to ~a argument~a"
                        (type-head-name h) (kind->datum kh*)
                        (length args) (if (= (length args) 1) "" "s")))))
        (define result (kvar (gensym 'k)))
        (define expected (kind-arrow* kargs result))
        (define s3
          (with-handlers ([exn:fail:kind-unify?
                           (lambda (e) (kind-fail! stx (exn-message e)) s2)])
            (ksubst-compose (unify-kind kh* (apply-ksubst s2 expected)) s2)))
        (values (apply-ksubst s3 result) s3)])]
    [(ty:forall vs body _)
     (for ([v (in-list vs)]) (hash-set! tvar-kinds v (kvar (gensym 'k))))
     (elab-surface body env batch-kinds tvar-kinds s)]
    [(ty:exists vs body _)
     ;; An existential binds its own vars (give each a fresh kvar) and has
     ;; the kind of its body — the same treatment as a forall.
     (for ([v (in-list vs)]) (hash-set! tvar-kinds v (kvar (gensym 'k))))
     (elab-surface body env batch-kinds tvar-kinds s)]
    [(ty:qual _cs body _)
     ;; A qualified body's constraints are checked by the entry points;
     ;; the type's kind is its body's.
     (elab-surface body env batch-kinds tvar-kinds s)]
    [_ (values (kvar (gensym 'k)) s)]))

;; An alias application: resolve it (expanding the alias, which also
;; guards against recursive aliases) and infer the expansion's kind via
;; the core walker, reporting any failure at the alias use site.
(define (elab-alias t env batch-kinds tvar-kinds s)
  (with-handlers ([exn:fail:kind-unify?
                   (lambda (e)
                     (kind-fail! (ty-ast->stx t) (exn-message e))
                     (values (kvar (gensym 'k)) s))])
    (elab-kind (resolve-type t env) env batch-kinds tvar-kinds s)))

;; Require a surface type to have kind `*` (a value-type position),
;; blaming the type's node on failure.
(define (demand-star env t tvar-kinds)
  (define-values (k s) (elab-surface t env (hasheq) tvar-kinds empty-ksubst))
  (with-handlers ([exn:fail:kind-unify?
                   (lambda (e) (kind-fail! (ty-ast->stx t) (exn-message e)))])
    (unify-kind (apply-ksubst s k) kstar)
    (void)))

;; Check a surface constraint: each argument's kind must match the
;; class's declared parameter kind, blaming the specific argument.  A
;; non-class head (the `~` equality predicate) or an unknown class is
;; skipped.
(define (kc-constraint env c tvar-kinds)
  (match c
    [(constraint cname args _)
     (define cinfo (env-ref-class env cname #f))
     (when cinfo
       (for ([arg (in-list args)] [param (in-list (class-info-params cinfo))])
         (define pk (hash-ref (class-info-kinds cinfo) param kstar))
         (define-values (ka s) (elab-surface arg env (hasheq) tvar-kinds empty-ksubst))
         ;; A failure here is an argument whose kind ≠ the class
         ;; parameter's expected kind; phrase it in those terms rather
         ;; than as a raw unification failure.
         (with-handlers ([exn:fail:kind-unify?
                          (lambda (e)
                            (kind-fail! (ty-ast->stx arg)
                              (format "~a expects an argument of kind ~s, but this one has kind ~s"
                                      cname
                                      (kind->datum pk)
                                      (kind->datum (default-kind (apply-ksubst s ka))))))])
           (unify-kind (apply-ksubst s ka) pk)
           (void))))]
    [_ (void)]))

;; A value-type position (signature, annotation, effect op, …): the
;; whole surface type must have kind `*`.  Handles a leading forall and
;; a qualified body (whose constraints are checked too).
(define (kind-check-surface env ty-ast)
  (unless (eq? (current-kind-check) 'off)
    (define tvar-kinds (make-hasheq))
    (let loop ([t ty-ast])
      (match t
        [(ty:forall vs body _)
         (for ([v (in-list vs)]) (hash-set! tvar-kinds v (kvar (gensym 'k))))
         (loop body)]
        [(ty:exists vs body _)
         (for ([v (in-list vs)]) (hash-set! tvar-kinds v (kvar (gensym 'k))))
         (loop body)]
        [(ty:qual cs body _)
         (for ([c (in-list cs)]) (kc-constraint env c tvar-kinds))
         (demand-star env body tvar-kinds)]
        [_ (demand-star env t tvar-kinds)]))))

;; A standalone surface constraint (instance head/context, class
;; superclass, :requires) — its free type variables get fresh kvars.
(define (kind-check-constraint-surface env c)
  (unless (eq? (current-kind-check) 'off)
    (kc-constraint env c (make-hasheq))))

;; ----- kinds: DataKinds-style promotion (Phase A4.4) -----------------

;; Promote each eligible datatype to the kind level.  A monomorphic
;; datatype T promotes to the kind `(kind-con T)`; a PARAMETERISED
;; datatype `(T p…)` promotes to the applied kind `(kapp (kind-con T)
;; κp…)`, with its parameters' kinds quantified — so `(List κ)` is a
;; reusable kind for any element kind κ (PolyKinds).  Each ordinary
;; constructor C with field types F1..Fn becomes a TYPE-LEVEL constructor
;; whose kind SCHEME is `∀κ⃗. φ(F1) -> … -> φ(Fn) -> R`, where R is T's
;; result kind and φ maps a field type to its promoted kind (a parameter
;; to its kvar, a datatype reference to its promoted kind constructor),
;; recorded in the env's promoted-ctors table.  Promotion only *adds*
;; type-level identities; value-level data is untouched.  A constructor
;; whose name already denotes a type, alias, or primitive — or any
;; GADT-syntax constructor (one with an explicit result type) — is left
;; value-only, so promotion never reinterprets an existing type name.
(define (promote-data env forms)
  (define (datatype-arity name)
    (define ti (env-ref-tcon env name #f))
    (and ti (tcon-info-arity ti)))
  ;; The promoted kind of a constructor field's surface type — a parameter
  ;; maps to its kvar, a bare monomorphic-datatype reference to its
  ;; kind-con, and an applied datatype `(D a…)` to `(kapp (kind-con D)
  ;; …)`.  #f when the field is not promotable (e.g. a scalar type, an
  ;; arrow, or a parameter applied as a higher-kinded head).
  (define (field-kind ft param-kinds)
    (match ft
      [(ty:var n _) (hash-ref param-kinds n (lambda () (kvar (gensym 'k))))]
      [(ty:con n _) (and (equal? (datatype-arity n) 0) (kind-con n))]
      [(ty:app (ty:con h _) args _)
       (define ar (datatype-arity h))
       (cond
         [(and ar (positive? ar) (= ar (length args)))
          (define aks (map (lambda (a) (field-kind a param-kinds)) args))
          (and (andmap values aks) (kapp (kind-con h) aks))]
         [else #f])]
      [_ #f]))
  (define (name-taken? env name)
    (or (env-ref-tcon env name #f)
        (env-ref-alias env name #f)
        (hash-has-key? primitive-kind-table name)))
  (for/fold ([env env]) ([f (in-list forms)] #:when (top:data? f))
    (match-define (top:data tname tparams ctors _ _ _) f)
    (define param-kinds
      (for/hasheq ([p (in-list tparams)]) (values p (kvar (gensym 'k)))))
    (define result-kind
      (if (null? tparams)
          (kind-con tname)
          (kapp (kind-con tname)
                (for/list ([p (in-list tparams)]) (hash-ref param-kinds p)))))
    (for/fold ([env env]) ([c (in-list ctors)])
      (define cname (data-ctor-name c))
      (define fks (map (lambda (ft) (field-kind ft param-kinds))
                       (data-ctor-field-types c)))
      (cond
        ;; A GADT-result ctor, an unpromotable field, or a name already
        ;; bound as a type leaves this constructor value-only.
        [(or (data-ctor-result-type c)
             (memq #f fks)
             (name-taken? env cname))
         env]
        [else
         (env-extend-promoted-ctor
          env cname
          (generalize-kind (kind-arrow* fks result-kind)))]))))

;; ----- kinds: data-type kind inference (Phase A2.5) ------------------

;; Infer and record the kind of every `top:data` constructor in this
;; batch.  Seed each `T(p1..pn)` with `κp1 -> … -> κpn -> *` (a data
;; type's result is always `*`), seeding ALL batch types before
;; constraining so self- and mutual recursion resolve against the
;; shared seeds; constrain every constructor field (and GADT result)
;; to kind `*`; then default residual param kvars to `*` and write the
;; concrete kind into the tcon shell.  `env` must already carry the
;; tcon shells (Phase A2) and aliases (A3) — field types may use both.
;; The explicitly-declared kind of data parameter `pname` in form `f`,
;; or #f when it was written without an annotation.  The parser stashes
;; the annotations as an `(name . surface-kind)` alist on the form's stx
;; under 'rackton:data-param-kinds (the channel `protocol` uses for
;; 'rackton:kind).
(define (data-param-declared-kind f pname)
  (define s (top:data-stx f))
  (define alist (and (syntax? s) (syntax-property s 'rackton:data-param-kinds)))
  (define entry (and alist (assq pname alist)))
  (and entry (surface-kind->core (cdr entry))))

(define (infer-data-kinds env forms)
  (define datas (filter top:data? forms))
  (cond
    [(null? datas) env]
    [else
     ;; 1. Seed.  A parameter written `(g :: k)` seeds its DECLARED kind
     ;; (so constructor usage is checked against the annotation); an
     ;; unannotated parameter seeds a fresh kvar, as before.
     (define seeds
       (for/list ([f (in-list datas)])
         (match-define (top:data _ tparams _ _ _ _) f)
         (define pkvars
           (for/list ([p (in-list tparams)])
             (or (data-param-declared-kind f p) (kvar (gensym 'k)))))
         (list f (map cons tparams pkvars) (kind-arrow* pkvars kstar))))
     (define batch-kinds
       (for/fold ([h (hasheq)]) ([sd (in-list seeds)])
         (hash-set h (top:data-name (car sd)) (caddr sd))))
     ;; 2. Constrain: every constructor field and GADT result is kind *.
     ;; Walk the SURFACE field type so an ill-kinded field blames its
     ;; own node.
     (define (demand-star surface-ty tvar-kinds s)
       (define-values (k s*) (elab-surface surface-ty env batch-kinds tvar-kinds s))
       (with-handlers ([exn:fail:kind-unify?
                        (lambda (e)
                          (kind-fail! (ty-ast->stx surface-ty) (exn-message e))
                          s*)])
         (ksubst-compose (unify-kind (apply-ksubst s* k) kstar) s*)))
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
               (demand-star ft tvar-kinds s)))
           (cond
             [(data-ctor-result-type c)
              (demand-star (data-ctor-result-type c) tvar-kinds s-fields)]
             [else s-fields]))))
     ;; 3. Solve & default, writing the concrete kind into each shell.
     (for/fold ([env env]) ([sd (in-list seeds)])
       (match-define (list f _ seed) sd)
       (define name (top:data-name f))
       (define ti (env-ref-tcon env name))
       (env-extend-tcon env name
                        (struct-copy tcon-info ti
                                     [kind (generalize-kind (apply-ksubst s seed))])))]))

;; ----- constraint synonyms: registration (Phase A3c) ----------------

;; Resolve each `define-constraint`'s component constraints to core preds
;; and register them.  Component preds name classes by symbol only, so
;; this does not require the classes to be in env yet.
(define (register-constraint-syns env forms)
  (for/fold ([e env]) ([f (in-list forms)] #:when (top:constraint-syn? f))
    (match-define (top:constraint-syn name params constraints _stx) f)
    (env-extend-constraint-syn
     e name params
     (for/list ([c (in-list constraints)]) (resolve-constraint c e)))))

;; Resolve each `constraint-family`'s clauses (LHS type patterns + RHS
;; constraint templates) to core form and register it.  Component
;; constraints may have a parameter head (`(c x)`); resolve-constraint
;; builds a pred whose class is that symbol, substituted at reduction.
(define (register-constraint-fams env forms)
  (for/fold ([e env]) ([f (in-list forms)] #:when (top:constraint-fam? f))
    (match-define (top:constraint-fam name params clauses _stx) f)
    (env-extend-constraint-fam
     e name
     (constraint-fam-info
      name (length params)
      (for/list ([c (in-list clauses)])
        (cons (for/list ([pt (in-list (cfam-clause-pats c))]) (resolve-type pt e))
              (for/list ([ct (in-list (cfam-clause-constraints c))])
                (resolve-constraint ct e))))))))

;; ----- standalone type families: registration (Phase A4.6) ----------

;; Resolve one closed-family clause to a `(pats . rhs)` of core types.
(define (resolve-tyfam-clause c env)
  (cons (for/list ([p (in-list (tyfam-clause-pats c))]) (resolve-type p env))
        (resolve-type (tyfam-clause-rhs c) env)))

;; Register every `type-family` declaration and fold in every standalone
;; `type-instance` equation, then check open families for coherence.
;; Runs after DataKinds promotion / data-kind inference so clause types
;; may mention promoted constructors and data kinds; before classes and
;; data ctors so their field/method types can mention families.  Kinds
;; are left to infer leniently for now (Phase 1): a family head used in a
;; type kind-checks as an unknown constructor.
(define (register-type-families env forms)
  (define env1
    (for/fold ([e env]) ([f (in-list forms)] #:when (top:type-family? f))
      (match-define (top:type-family name params _kind clauses _stx) f)
      (env-extend-tyfam
       e name
       (tyfam-info name (length params) #f
                   (if (null? clauses) 'open 'closed)
                   (for/list ([c (in-list clauses)]) (resolve-tyfam-clause c e))))))
  (define env2
    (for/fold ([e env1]) ([f (in-list forms)] #:when (top:type-instance? f))
      (match-define (top:type-instance name args rhs stx) f)
      (define info (env-ref-tyfam e name))
      (cond
        [(not info)
         (raise-syntax-error 'infer
           (format "type-instance for unknown type family ~a" name) stx)]
        [(eq? (tyfam-info-openness info) 'closed)
         (raise-syntax-error 'infer
           (format "cannot add a type-instance to the closed type family ~a" name) stx)]
        [(not (= (length args) (tyfam-info-arity info)))
         (raise-syntax-error 'infer
           (format "type family ~a expects ~a argument~a"
                   name (tyfam-info-arity info)
                   (if (= 1 (tyfam-info-arity info)) "" "s")) stx)]
        [else
         (env-add-tyfam-clause
          e name
          (cons (for/list ([a (in-list args)]) (resolve-type a e))
                (resolve-type rhs e)))])))
  ;; Open families must be coherent: no two equations may overlap.
  (for ([f (in-list forms)]
        #:when (and (top:type-family? f) (null? (top:type-family-clauses f))))
    (define name (top:type-family-name f))
    (define info (env-ref-tyfam env2 name))
    (when (and info (tyfam-clauses-overlap? name (tyfam-info-clauses info)))
      (raise-syntax-error 'infer
        (format "overlapping type-instance equations for open type family ~a" name)
        (top:type-family-stx f))))
  env2)

;; Infer each newly-declared family's KIND from its clauses/equations
;; (Phase 2), mirroring `infer-data-kinds`: seed every family with param
;; kvars and a result kvar (so self/mutual references resolve against the
;; seed), constrain each clause's i-th pattern to the i-th param kind and
;; its rhs to the result kind, then generalise residual kvars into a kind
;; scheme stored on the `tyfam-info`.  Only families whose kind is still
;; #f are inferred — an imported family already carries its kind.
(define (infer-tyfam-kinds env forms)
  (define fam-stx
    (for/hasheq ([f (in-list forms)] #:when (top:type-family? f))
      (values (top:type-family-name f) (top:type-family-stx f))))
  (define targets
    (for/list ([(name info) (in-hash (env-tyfams env))]
               #:when (not (tyfam-info-kind info)))
      (cons name info)))
  (cond
    [(null? targets) env]
    [else
     ;; 1. Seed: name → (list param-kvars result-kvar seed-kind).
     (define seeds
       (make-immutable-hasheq
        (for/list ([p (in-list targets)])
          (match-define (cons name info) p)
          (define pks (for/list ([_ (in-range (tyfam-info-arity info))])
                        (kvar (gensym 'k))))
          (define rk (kvar (gensym 'k)))
          (cons name (list pks rk (kind-arrow* pks rk))))))
     (define batch-kinds
       (make-immutable-hasheq
        (for/list ([(name sd) (in-hash seeds)]) (cons name (caddr sd)))))
     ;; Unify two kinds into the running ksubst; an inconsistent clause
     ;; blames the family's declaration.
     (define (demand ka kb s stx)
       (with-handlers ([exn:fail:kind-unify?
                        (lambda (e) (kind-fail! stx (exn-message e)) s)])
         (ksubst-compose (unify-kind (apply-ksubst s ka) (apply-ksubst s kb)) s)))
     ;; 2. Constrain every clause of every target family.
     (define s
       (for/fold ([s empty-ksubst]) ([p (in-list targets)])
         (match-define (cons name info) p)
         (match-define (list pks rk _) (hash-ref seeds name))
         (define stx (hash-ref fam-stx name #f))
         (for/fold ([s s]) ([clause (in-list (tyfam-info-clauses info))])
           (match-define (cons pats rhs) clause)
           (define tvar-kinds (make-hasheq))
           (define s-pats
             (for/fold ([s s]) ([pat (in-list pats)] [pk (in-list pks)])
               (define-values (kp s*) (elab-kind pat env batch-kinds tvar-kinds s))
               (demand kp pk s* stx)))
           (define-values (kr s2) (elab-kind rhs env batch-kinds tvar-kinds s-pats))
           (demand kr rk s2 stx))))
     ;; 3. Solve & generalise, writing each kind scheme into its tyfam-info.
     (for/fold ([env env]) ([p (in-list targets)])
       (match-define (cons name info) p)
       (match-define (list _ _ seed) (hash-ref seeds name))
       (env-extend-tyfam env name
         (struct-copy tyfam-info info
                      [kind (generalize-kind (apply-ksubst s seed))])))]))

;; Infer each data family's kind (Phase A4.7).  A data family's result
;; kind is always `*` (it classifies values); its parameter kinds come
;; from how its instance heads use them — so a family indexed by a
;; promoted tag (`(Val TI)`) gets kind `Ty -> *`, not the `*`-placeholder.
;; An explicit `:: kind` annotation on the family is used verbatim.  Two
;; instances that disagree on a parameter's kind unify-fail with a kind
;; error.  The family tcon shell already exists (Phase A2).
(define (infer-data-family-kinds env forms)
  (define insts-by-fam
    (for/fold ([h (hasheq)]) ([f (in-list forms)] #:when (top:data-instance? f))
      (hash-update h (top:data-instance-name f)
                   (lambda (a) (cons f a)) '())))
  (for/fold ([env env]) ([df (in-list forms)] #:when (top:data-family? df))
    (match-define (top:data-family name params kind-ann stx) df)
    (define ti (env-ref-tcon env name))
    (cond
      [kind-ann
       (env-extend-tcon env name
         (struct-copy tcon-info ti
                      [kind (generalize-kind (surface-kind->core kind-ann))]))]
      [else
       (define pks (for/list ([_ (in-list params)]) (kvar (gensym 'k))))
       (define seed (kind-arrow* pks kstar))
       (define batch-kinds (hasheq name seed))   ; self-reference seed
       (define insts (reverse (hash-ref insts-by-fam name '())))
       (define s
         (for/fold ([s empty-ksubst]) ([inst (in-list insts)])
           (define tvar-kinds (make-hasheq))
           (for/fold ([s s]) ([a (in-list (top:data-instance-args inst))]
                              [pk (in-list pks)])
             (define-values (ka s*)
               (elab-kind (resolve-type a env) env batch-kinds tvar-kinds s))
             (with-handlers ([exn:fail:kind-unify?
                              (lambda (e) (kind-fail! stx (exn-message e)) s*)])
               (ksubst-compose (unify-kind (apply-ksubst s* ka) pk) s*)))))
       (env-extend-tcon env name
         (struct-copy tcon-info ti
                      [kind (generalize-kind (apply-ksubst s seed))]))])))

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
                                  extra-tvars
                                  (data-ctor-field-names c))))))

;; ----- data families: instance constructor registration -------------

;; Register the constructors of ONE data instance `(F args…)`.  Each
;; constructor's result type is the instance head (GADT-style), so it
;; quantifies over the free variables of its fields and head — exactly
;; the GADT path in `resolve-data-ctors`.  The family tcon `F` already
;; exists (a `data-family` shell).  Shared by the batch pass and the
;; per-form REPL path.
(define (register-data-instance env fname args ctors)
  (define result-type
    (make-tapp (tcon fname)
               (for/list ([a (in-list args)]) (resolve-type a env))))
  (for/fold ([e env]) ([c (in-list ctors)])
    (define field-tys
      (for/list ([t (in-list (data-ctor-field-types c))]) (resolve-type t env)))
    (define ctor-fn-type (foldr make-arrow result-type field-tys))
    (define quant
      (sort (set->list
             (for/fold ([acc (type-vars result-type)]) ([t (in-list field-tys)])
               (set-union acc (type-vars t))))
            symbol<?))
    (env-extend-data e (data-ctor-name c)
                     (data-info fname (data-ctor-name c)
                                (length field-tys)
                                (scheme quant ctor-fn-type)
                                '()
                                (data-ctor-field-names c)))))

;; Batch pass: register every data-instance's constructors.  Runs with
;; resolve-data-ctors (Phase A7), the type-level env complete.
(define (resolve-data-instance-ctors env forms)
  (for/fold ([env env]) ([f (in-list forms)] #:when (top:data-instance? f))
    (match-define (top:data-instance fname args ctors _stx) f)
    (register-data-instance env fname args ctors)))

;; A data family's instances must be coherent: no two heads may overlap
;; (unify).  Reuses the closed-family overlap check by treating each
;; instance head as a clause LHS (the rhs is irrelevant to head apartness).
(define (check-data-family-coherence env forms)
  (define insts-by-fam
    (for/fold ([h (hasheq)]) ([f (in-list forms)] #:when (top:data-instance? f))
      (hash-update h (top:data-instance-name f) (lambda (a) (cons f a)) '())))
  (for ([(name insts) (in-hash insts-by-fam)])
    (define clauses
      (for/list ([inst (in-list insts)])
        (cons (for/list ([a (in-list (top:data-instance-args inst))])
                (resolve-type a env))
              (tcon 'Unit))))      ; dummy rhs — only the heads are compared
    (when (tyfam-clauses-overlap? name clauses)
      (raise-syntax-error 'infer
        (format "overlapping data-instance heads for data family ~a" name)
        (top:data-instance-stx (car insts))))))

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
    ;; Blame this instance for any constraint error raised while
    ;; checking it — including ones deep in generalize* — unless a
    ;; narrower blame (e.g. a method body) is set inside.
    (define-values (e* _ st′)
      (parameterize ([current-reduce-blame (top:instance-stx f)])
        (handle-top-form f e declared st)))
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
    ;; Blame this SCC's first definition for a constraint error raised
    ;; while inferring it — including ones deep in generalize* — unless
    ;; a per-definition handler sets a narrower blame inside.
    (define blame
      (let ([f (hash-ref defs-by-name (car scc) #f)])
        (and f (top:def-stx f))))
    (parameterize ([current-reduce-blame blame])
      (infer-def-scc env declared def-tvars defs-by-name scc st))))

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
      [(e:open e _ vv body _)
       (walk e shadowed) (walk body (set-add shadowed vv))]
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
      [(e:tuple elems _)
       (for ([el (in-list elems)]) (walk el shadowed))]
      [(e:bits segs _)
       (for ([sg (in-list segs)]) (walk (bit-seg-subject sg) shadowed))]
      [(e:tref t _ _) (walk t shadowed)]
      [(e:array elems _)
       (for ([el (in-list elems)]) (walk el shadowed))]
      [(e:build-array _ p _) (walk p shadowed)]
      [(e:aref a _ _) (walk a shadowed)]
      [(e:array-slice _ _ a _) (walk a shadowed)]
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
    [(p:tuple args _)
     (for/fold ([s (seteq)]) ([a (in-list args)])
       (set-union s (pattern-bound-names a)))]
    [(p:bits segs _)
     (for/fold ([s (seteq)]) ([sg (in-list segs)])
       (set-union s (pattern-bound-names (bit-seg-subject sg))))]
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
  (define-values (decl-ty0 decl-preds dict-skolems dict-arg-names)
    (cond
      [(null? needs-dict-reqs)
       (define-values (t p) (skolemize decl-scheme))
       (values t p (hasheq) '())]
      [else
       (define-values (t p s) (skolemize/tracked decl-scheme))
       (define-values (sk-map args) (build-dict-skolems needs-dict-reqs s env))
       (values t p sk-map args)]))
  ;; Reduce any type-family applications in the declared type up front so
  ;; the arrow-shape check, argument unfolding, and the body/decl unify
  ;; all see the normal form (e.g. `(Sel PTrue A B)` ⇒ `A`).
  (define decl-ty (normalize-type/guarded env decl-ty0))
  (define saved-skolems (current-dict-skolems))
  (current-dict-skolems dict-skolems)
  ;; Make the signature's skolem givens visible while the body is
  ;; inferred, so an inner `let`/`letrec` generalized mid-body can
  ;; discharge a constraint over those skolems (see `generalize*`).
  (define saved-given (current-given-preds))
  (current-given-preds decl-preds)
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
           [(tail-elim-form? (e:lam-body expr))
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
     (unify (normalize-type/guarded env (apply-subst s t)) decl-ty)))
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
    (parameterize ([current-reduce-blame stx])
      (reduce-context env (map (lambda (p) (apply-subst final-subst p)) decl-preds)
                      (st:preds st4))))
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
  (current-given-preds saved-given)
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
    ;; A `:derive-supers` instance expands into several plain
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
     (pass (lambda () (handle-data-form tname tparams ctors stx abstract? runtime-tag env declared)))]
    [(top:type-family name params kind clauses stx)
     (pass (lambda ()
             (values (env-extend-tyfam
                      env name
                      (tyfam-info name (length params) #f
                                  (if (null? clauses) 'open 'closed)
                                  (for/list ([c (in-list clauses)])
                                    (resolve-tyfam-clause c env))))
                     declared)))]
    [(top:type-instance name args rhs stx)
     (pass (lambda ()
             (define info (env-ref-tyfam env name))
             (cond
               [(not info)
                (raise-syntax-error 'infer
                  (format "type-instance for unknown type family ~a" name) stx)]
               [(eq? (tyfam-info-openness info) 'closed)
                (raise-syntax-error 'infer
                  (format "cannot add a type-instance to the closed type family ~a" name) stx)]
               [else
                (values (env-add-tyfam-clause
                         env name
                         (cons (for/list ([a (in-list args)]) (resolve-type a env))
                               (resolve-type rhs env)))
                        declared)])))]
    [(top:data-family name params kind stx)
     (pass (lambda ()
             (values (env-extend-tcon env name
                       (tcon-info name (length params)
                                  (kscheme-mono (arity->star-kind (length params)))
                                  '() #f #f))
                     declared)))]
    [(top:constraint-syn name params constraints stx)
     (pass (lambda ()
             (values (env-extend-constraint-syn
                      env name params
                      (for/list ([c (in-list constraints)]) (resolve-constraint c env)))
                     declared)))]
    [(top:constraint-fam name params clauses stx)
     (pass (lambda ()
             (values (env-extend-constraint-fam
                      env name
                      (constraint-fam-info
                       name (length params)
                       (for/list ([c (in-list clauses)])
                         (cons (for/list ([pt (in-list (cfam-clause-pats c))])
                                 (resolve-type pt env))
                               (for/list ([ct (in-list (cfam-clause-constraints c))])
                                 (resolve-constraint ct env))))))
                     declared)))]
    [(top:data-instance name args ctors stx)
     (pass (lambda ()
             ;; Register the instance's constructors, then add their names
             ;; to the family tcon so a match on `(F args)` sees them.
             (define env1 (register-data-instance env name args ctors))
             (define ti (env-ref-tcon env1 name))
             (define env2
               (if ti
                   (env-extend-tcon env1 name
                     (struct-copy tcon-info ti
                       [ctors (append (tcon-info-ctors ti)
                                      (map data-ctor-name ctors))]))
                   env1))
             (values env2 declared)))]))

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
        ;; Make the signature's skolem givens visible while the body is
        ;; inferred (see `current-given-preds` / `generalize*`), so an
        ;; inner `let`/`letrec` can discharge a constraint over them.
        (define saved-given (current-given-preds))
        (current-given-preds decl-preds)
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
             ;; Seed the expected-type parameter when the lambda body is a
             ;; GADT-elim site — a `match`, or an `if`/`let`/`letrec`
             ;; whose tail leads to one — so the declared codomain reaches
             ;; the match's result-tv and unlocks local skolem refinement.
             ;; `with-expected` (in infer-if/let/letrec) clears the type
             ;; for every NON-tail position, so it can't propagate too
             ;; deep (into a condition, a binding RHS, an app argument, …)
             ;; and pin the wrong subexpression.
             (define-values (s-body t-body st-b)
               (cond
                 [(tail-elim-form? (e:lam-body expr))
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
           (unify (normalize-type/guarded env (apply-subst s t)) decl-ty)))
        ;; Discharge any constraints raised inside the body against the
        ;; declaration's preds (hypotheses).
        (define final-subst (subst-compose s-u s))
        (define st2 (st:apply-subst-to-preds st1 final-subst))
        (define remaining-preds
          (parameterize ([current-reduce-blame stx])
            (reduce-context env decl-preds (st:preds st2))))
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
        (current-given-preds saved-given)
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
         (for ([t (in-list (effect-op-arg-types o))]) (kind-check-surface env t))
         (kind-check-surface env (effect-op-result-type o))
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
                                   (kscheme-mono (arity->star-kind (length tparams)))
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
                                     extra-tvars
                                     (data-ctor-field-names c)))))
     (values env** declared))

;; ----- class / instance elaboration --------------------------------

;; Type-check one `:laws` entry of a protocol.  A law must hold for an
;; arbitrary instance, so each class parameter is skolemized to a rigid
;; constant and the class head plus its superclasses are assumed as
;; hypotheses; the law's own `=>` context (e.g. `(Eq a)`) is assumed too,
;; letting the equation compare results without that constraint becoming
;; a superprotocol requirement.  The quantifier binders are bound at
;; their annotated types (parameters replaced by their skolems), and the
;; body must type to `Boolean` using only constraints those hypotheses
;; entail.  Raises a syntax error blaming the law when it does not —
;; making the law a checked part of the protocol's contract rather than
;; inert text.  This mirrors `handle-def-form`'s discipline (skolemize
;; the declared context, infer the body, reduce residual constraints).
;; Rewrite every type annotation in a law body, replacing the
;; class-parameter type variables named in `ty-sub` with their skolem
;; type constructors.  Only `e:ann` carries a type; the rest of the walk
;; just recurses through the expression forms a law body can contain.
(define (relabel-law-ann-types e ty-sub)
  (let R ([e e])
    (match e
      [(e:ann ex t stx)    (e:ann (R ex) (substitute-tyvars ty-sub t) stx)]
      [(e:lam ps b stx)    (e:lam ps (R b) stx)]
      [(e:app h as stx)    (e:app (R h) (map R as) stx)]
      [(e:if a b c stx)    (e:if (R a) (R b) (R c) stx)]
      [(e:open ex tvs vv body stx) (e:open (R ex) tvs vv (R body) stx)]
      [(e:let bs b stx)
       (e:let (for/list ([p (in-list bs)]) (cons (car p) (R (cdr p)))) (R b) stx)]
      [(e:letrec bs b stx)
       (e:letrec (for/list ([p (in-list bs)]) (cons (car p) (R (cdr p)))) (R b) stx)]
      [_ e])))

;; Collect, in order of first appearance, the names of every free
;; surface type variable in a law's binder types and `=>` context
;; constraints.  Used to find a law's element type variables — those not
;; already bound as class parameters.
(define (law-surface-type-vars ty [bound '()])
  (match ty
    [(ty:var n _)        (if (memq n bound) '() (list n))]
    [(ty:con _ _)        '()]
    [(ty:app h args _)   (append (law-surface-type-vars h bound)
                                 (append-map (lambda (a) (law-surface-type-vars a bound)) args))]
    [(ty:forall vs b _)  (law-surface-type-vars b (append vs bound))]
    [(ty:exists vs b _)  (law-surface-type-vars b (append vs bound))]
    [(ty:qual cs b _)    (append (append-map (lambda (c) (law-constraint-type-vars c bound)) cs)
                                 (law-surface-type-vars b bound))]
    [_ '()]))
(define (law-constraint-type-vars c [bound '()])
  (append-map (lambda (a) (law-surface-type-vars a bound)) (constraint-args c)))

;; Improvement by hypotheses (law-checking only).  The law's hypotheses
;; are SKOLEMIZED givens; a residual constraint introduced by a method
;; whose type underdetermines a parameter — `arr`'s product `p` (the
;; `cat -> p` fundep cannot fire for a skolem `cat`), or `on-first`'s
;; untouched component `c` (a fresh result variable) — carries FLEXIBLE
;; variables that should be pinned to the givens' skolems.  `reduce-context`
;; only discharges a hypothesis by rigid match, so it never closes these.
;; Here we UNIFY each residual against the unique hypothesis sharing its
;; class, accumulating the substitution, so the subsequent reduce-context
;; can discharge it.  Conservative: only a unique, successful unification
;; improves — an ambiguous (two same-class hyps) or failing match is left
;; for reduce-context to resolve or report, exactly as before.  Bounded by
;; one pass per residual (a chain can need several to propagate), which is
;; idempotent once stable, so it terminates.
(define (law-unify-pred-args p h)
  (and (= (length (pred-args p)) (length (pred-args h)))
       (with-handlers ([exn:fail:unify? (lambda (_) #f)])
         (for/fold ([s empty-subst])
                   ([pa (in-list (pred-args p))] [ha (in-list (pred-args h))])
           (subst-compose (unify (apply-subst s pa) (apply-subst s ha)) s)))))

(define (law-improve-by-hyps hyps preds)
  (for/fold ([s empty-subst]) ([_ (in-range (add1 (length preds)))])
    (for/fold ([s s]) ([p (in-list preds)])
      (define p* (apply-subst s p))
      (define cands
        (filter (lambda (h) (eq? (pred-class h) (pred-class p*))) hyps))
      (cond
        [(and (pair? cands) (null? (cdr cands)))
         (define u (law-unify-pred-args p* (car cands)))
         (if u (subst-compose u s) s)]
        [else s]))))

(define (check-class-law law class-name class-params super-preds env)
  (match-define (class-law law-name context binders body law-stx) law)
  ;; A law is universally quantified over its element type variables —
  ;; every free type variable in the `=>` context and binder annotations
  ;; that is not a class parameter.  Skolemize those alongside the class
  ;; parameters so the unifier cannot re-orient them during body
  ;; inference (which would desynchronize the equation's goal from the
  ;; pre-computed hypotheses) and so the law states a genuine `forall`.
  (define extra-vars
    (remove-duplicates
     (filter (lambda (n) (not (memq n class-params)))
             (append (append-map law-constraint-type-vars context)
                     (append-map (lambda (b) (law-surface-type-vars (law-binder-type b))) binders)))))
  (define skol-params (append class-params extra-vars))
  ;; One rigid skolem constant per class parameter, shared between the
  ;; core substitution (for binder types and hypotheses) and the surface
  ;; type-constructor used to relabel annotations in the body.
  (define skol-names
    (for/hasheq ([p (in-list skol-params)])
      (values p (gensym (format "$skolem.~a." p)))))
  (define skol
    (for/fold ([s empty-subst]) ([p (in-list skol-params)])
      (subst-extend s p (tcon (hash-ref skol-names p)))))
  (define ty-sub
    (for/hasheq ([p (in-list skol-params)])
      (values p (ty:con (hash-ref skol-names p) #f))))
  (define law-preds
    (for/list ([c (in-list context)]) (resolve-constraint c env)))
  (define hyps
    (append (cons (apply-subst skol (pred class-name (map tvar class-params)))
                  (for/list ([sp (in-list super-preds)]) (apply-subst skol sp)))
            (for/list ([lp (in-list law-preds)]) (apply-subst skol lp))))
  (define env+binders
    (for/fold ([e env]) ([b (in-list binders)])
      (kind-check-surface e (law-binder-type b))
      (env-extend-var e (law-binder-name b)
                      (scheme '() (apply-subst skol
                                    (resolve-type (law-binder-type b) env))))))
  ;; Pin class parameters in body annotations to the skolems, so an
  ;; `(ann (pure x) (f Integer))` resolves a return-typed method's
  ;; container to the law's rigid skolem rather than a fresh variable.
  (define body* (relabel-law-ann-types body ty-sub))
  ;; The law's skolemized hypotheses (its `=>` context, the class head,
  ;; and superclass preds) are givens while the body is checked, so an
  ;; inner `let`/`letrec` generalized in the body may assume them.
  (define-values (s t st1)
    (parameterize ([current-given-preds (append hyps (current-given-preds))])
      (infer-expr body* env+binders (make-infer-state))))
  (define s-bool
    (with-handlers
     ([exn:fail:unify?
       (lambda (_)
         (raise-syntax-error 'infer
           (format "law ~s of protocol ~s must be a Boolean equation, but its body has type ~a"
                   law-name class-name (pretty-type (apply-subst s t)))
           law-stx))])
     (unify (apply-subst s t) t-bool)))
  (define final (subst-compose s-bool s))
  (define st2 (st:apply-subst-to-preds st1 final))
  ;; Improve flexible residual variables against the skolemized
  ;; hypotheses before reduction (see law-improve-by-hyps), so a law that
  ;; underdetermines a class parameter — e.g. `arr`'s product, `on-first`'s
  ;; untouched component — pins it to the law's own givens.
  (define improve (law-improve-by-hyps hyps (st:preds st2)))
  (define preds*
    (for/list ([p (in-list (st:preds st2))]) (apply-subst improve p)))
  (define residual
    (parameterize ([current-reduce-blame law-stx])
      (reduce-context env hyps preds*)))
  (unless (null? residual)
    (raise-syntax-error 'infer
      (format "law ~s of protocol ~s relies on constraints the protocol does not guarantee: ~a"
              law-name class-name (string-join (map pretty-pred residual) ", "))
      law-stx)))

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
              "protocol head arguments must be (kind-annotated) type variables"
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
  ;; Each parameter's kind: an explicit `::` annotation if present, else
  ;; one inherited from a superclass bound, else inferred from how the
  ;; parameter is used in the method signatures (a parameter applied as
  ;; `(s a)` is `* -> *`), defaulting to `*` when nothing constrains it.
  ;; Seeding the unannotated/uninherited params with fresh kvars and
  ;; solving across all method signatures is the class analogue of
  ;; data-type kind inference.
  (define class-kinds
    (let ()
      (define param-seeds (make-hasheq))
      (define seeds
        (for/list ([raw (in-list (constraint-args head))]
                   [name (in-list class-params)])
          (define annotated
            (match raw
              [(ty:var _ var-stx)
               (let ([sk (and (syntax? var-stx)
                              (syntax-property var-stx 'rackton:kind))])
                 (and sk (surface-kind->core sk)))]
              [_ #f]))
          (define seed (or annotated (kind-from-supers name) (kvar (gensym 'k))))
          (hash-set! param-seeds name seed)
          (cons name seed)))
      ;; Constrain via each method signature (a value type, kind `*`).
      ;; A per-method copy isolates method-local tvars while the shared
      ;; param seeds accumulate constraints through the threaded subst.
      ;; An inconsistency here is left to the dedicated checker; we keep
      ;; the seed (annotation/superclass) on conflict.
      (define s
        (for/fold ([s empty-ksubst]) ([m (in-list methods)] #:when (method-sig? m))
          (define tk (hash-copy param-seeds))
          (define core (resolve-type (method-sig-type m) env))
          (with-handlers ([exn:fail:kind-unify? (lambda (_) s)])
            (define-values (k s*) (elab-kind core env (hasheq) tk s))
            (ksubst-compose (unify-kind (apply-ksubst s* k) kstar) s*))))
      (for/fold ([acc (hasheq)]) ([sd (in-list seeds)])
        (hash-set acc (car sd) (default-kind (apply-ksubst s (cdr sd)))))))
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
         (kind-check-surface env (method-sig-type m))
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
           (format "protocol method ~s does not have any argument whose type mentions a protocol parameter — single dispatch cannot resolve it"
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
  ;; instance can be checked for matching :type bindings.
  (define type-families
    (for/list ([m (in-list methods)] #:when (class-type-fam? m))
      (class-type-fam-name m)))
  ;; Cross-class derivation table: superclass-name → (method-name → expr),
  ;; built from each `[Super (define …) …]` clause in the body's `:derive` list.
  (define super-derives
    (for/fold ([acc (hasheq)]) ([m (in-list methods)] #:when (class-super-derive? m))
      (define inner
        (for/fold ([h (hasheq)]) ([d (in-list (class-super-derive-methods m))])
          (hash-set h (method-default-name d) (method-default-expr d))))
      (hash-set acc (class-super-derive-super m) inner)))
  ;; Named, quantified law declarations from the body's `:laws` clause.
  (define laws
    (for/list ([m (in-list methods)] #:when (class-law? m)) m))
  (define info (class-info class-name class-params class-kinds
                           super-preds method-schemes defaults
                           dispatchpos fundeps dictreqs
                           type-families super-derives laws))
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
  ;; Laws are collected here but checked later, in `check-class-laws`
  ;; after instance registration (Phase C), so a law may reference
  ;; concrete instances such as `(Num Integer)` or `(Eq (List Integer))`
  ;; that do not yet exist while classes are being elaborated.
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
      (define base-mp (require-spec->module-path spec-stx))
      (define submod-spec (require-spec->submod-spec spec-stx))
      (cond
        [(not submod-spec) e]
        ;; A module path that names no module on disk is a genuine
        ;; mistake.  Flag it here — before the broad recovery handler
        ;; below — so it cannot be mistaken for the tolerated
        ;; "resolved, but no rackton sidecar" case.
        [(not (module-path-exists? base-mp))
         (raise-syntax-error 'require
           (format "cannot find module ~s" (syntax->datum spec-stx))
           spec-stx)]
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
           ;; DataKinds-promoted constructors of the imported data types
           ;; (name → kind).  Absent in legacy sidecars → empty.  Folded
           ;; below so the importer's kind checker enforces a promoted
           ;; index instead of treating it as a fresh, anything-goes kind.
           (define promoted
             (with-handlers ([exn:fail? (lambda (_) '())])
               (dynamic-require submod-spec 'rackton-promoted)))
           ;; Standalone type families (Feature 1) of the imported module
           ;; (name → encoded tyfam-info).  Absent in legacy sidecars →
           ;; empty.  Folded below so the importer reduces family
           ;; applications using the defining module's clauses + kind.
           (define tyfams
             (with-handlers ([exn:fail? (lambda (_) '())])
               (dynamic-require submod-spec 'rackton-tyfams)))
           ;; Constraint synonyms of the imported module (name → encoded).
           ;; Absent in legacy sidecars → empty.
           (define constraint-syns
             (with-handlers ([exn:fail? (lambda (_) '())])
               (dynamic-require submod-spec 'rackton-constraint-syns)))
           (define constraint-fams
             (with-handlers ([exn:fail? (lambda (_) '())])
               (dynamic-require submod-spec 'rackton-constraint-fams)))
           ;; Variadic functions of the imported module (name → fixed
           ;; arity).  Absent in legacy sidecars → empty.  Folded below so
           ;; the importer's call sites gather trailing arguments into the
           ;; rest-list, exactly as in the defining module.
           (define variadics
             (with-handlers ([exn:fail? (lambda (_) '())])
               (dynamic-require submod-spec 'rackton-variadics)))
           ;; The names a require sub-form (only-in / rename-in / prefix-in
           ;; / except-in) selects and renames.  A bare spec is the
           ;; identity, so a plain require is unchanged.  Instances carry
           ;; no name and stay global (coherence), so they are not filtered.
           (define xform (require-spec->name-transform (syntax->datum spec-stx)))
           ;; `qualified-in` namespaces only term-level names (values and
           ;; data constructors): type constructors and classes keep their
           ;; plain names so a constructor's result type stays consistent
           ;; with the type it builds.  For every other sub-form the type
           ;; and term transforms coincide.
           (define type-xform
             (require-spec->type-name-transform (syntax->datum spec-stx)))
           (define e1
             (fold-imported xform e bindings
               (lambda (acc nm enc) (env-extend-var acc nm (sexp->scheme enc)))))
           (define e2
             (fold-imported xform e1 data-ctors
               (lambda (acc nm enc) (env-extend-data acc nm (decode-data-info enc)))))
           ;; A type constructor's `ctors` list names term-level data
           ;; constructors, which the term `xform` renames; rewrite the
           ;; list so the type's notion of its constructors matches the
           ;; names they are actually bound under (keeps exhaustiveness
           ;; checking and `qualified-in` patterns in agreement).
           (define e3
             (fold-imported type-xform e2 tcons
               (lambda (acc nm enc)
                 (env-extend-tcon acc nm
                   (rename-tcon-info-ctors (decode-tcon-info enc) xform)))))
           (define e4
             (fold-imported type-xform e3 classes
               (lambda (acc nm enc) (env-extend-class acc nm (decode-class-info enc)))))
           (define e5
             (fold-imported type-xform e4 promoted
               (lambda (acc nm enc)
                 (env-extend-promoted-ctor acc nm (decode-kind-scheme enc)))))
           (define e6
             (fold-imported type-xform e5 tyfams
               (lambda (acc nm enc) (env-extend-tyfam acc nm (decode-tyfam-info enc)))))
           (define e7
             (fold-imported type-xform e6 constraint-syns
               (lambda (acc nm enc)
                 (define syn (decode-constraint-syn enc))
                 (env-extend-constraint-syn acc nm (car syn) (cdr syn)))))
           (define e8
             (fold-imported type-xform e7 constraint-fams
               (lambda (acc nm enc)
                 (env-extend-constraint-fam acc nm (decode-constraint-fam-info enc)))))
           (define e9
             (fold-imported xform e8 variadics
               (lambda (acc nm arity) (env-extend-variadic acc nm arity))))
           (for/fold ([acc e9]) ([entry (in-list instances)])
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

;; Resolve a require spec syntax to the *base* module path it names
;; (before the `rackton-schemes` submodule is appended).  Relative-path
;; strings are interpreted relative to the source file of the spec
;; itself.  Returns #f for a spec shape we do not handle.
(define (require-spec->module-path spec-stx)
  (define base (require-spec-base-datum (syntax->datum spec-stx)))
  (define src (syntax-source spec-stx))
  (cond
    [(and (string? base) (path-string? base) src)
     (define caller-dir
       (let-values ([(dir _name _dir?) (split-path src)])
         dir))
     (define full (path->complete-path base caller-dir))
     `(file ,(path->string full))]
    [(symbol? base) base]
    [else #f]))

;; Peel the standard wrapper sub-forms — only-in / except-in / rename-in
;; (inner spec is the second element) and prefix-in (the third) — down to
;; the inner module reference (a path string or a collection symbol).  An
;; unhandled shape (e.g. combine-in, which names several modules) yields #f
;; and is skipped, exactly as a bare unrecognised spec was before.
(define (require-spec-base-datum d)
  (cond
    [(string? d) d]
    [(symbol? d) d]
    [(pair? d)
     (case (car d)
       [(only-in except-in rename-in) (require-spec-base-datum (cadr d))]
       [(prefix-in qualified-in)      (require-spec-base-datum (caddr d))]
       [else #f])]
    [else #f]))

;; The name transform a require spec imposes on the importee's exported
;; names: a (symbol -> (or/c symbol #f)) where #f means "not imported".
;; Transforms compose inner-first, so a nested spec such as
;; `(prefix-in p (only-in m a [b c]))` selects/renames before prefixing,
;; matching Racket's own sub-form semantics.
(define (require-spec->name-transform d)
  (cond
    [(pair? d)
     (case (car d)
       [(only-in)
        (define inner (require-spec->name-transform (cadr d)))
        (define table (require-rename-table (cddr d)))
        (lambda (n) (let ([m (inner n)]) (and m (hash-ref table m #f))))]
       [(rename-in)
        (define inner (require-spec->name-transform (cadr d)))
        (define table (require-rename-table (cddr d)))
        (lambda (n) (let ([m (inner n)]) (and m (hash-ref table m m))))]
       [(except-in)
        (define inner (require-spec->name-transform (cadr d)))
        (define dropped (list->seteq (cddr d)))
        (lambda (n)
          (let ([m (inner n)]) (and m (not (set-member? dropped m)) m)))]
       [(prefix-in)
        (define inner (require-spec->name-transform (caddr d)))
        (define pfx (symbol->string (cadr d)))
        (lambda (n)
          (let ([m (inner n)])
            (and m (string->symbol (string-append pfx (symbol->string m))))))]
       ;; `qualified-in` is `prefix-in` with a colon-suffixed prefix:
       ;; `(qualified-in p mod)` imports each `name` as `p:name`, matching
       ;; the codegen rewrite to `(prefix-in p: mod)`.
       [(qualified-in)
        (define inner (require-spec->name-transform (caddr d)))
        (define pfx (format "~a:" (cadr d)))
        (lambda (n)
          (let ([m (inner n)])
            (and m (string->symbol (string-append pfx (symbol->string m))))))]
       ;; An unhandled wrapper imports nothing (its module path is #f too).
       [else (lambda (n) #f)])]
    ;; A bare module reference imports every name unchanged.
    [else (lambda (n) n)]))

;; The transform a require spec imposes on TYPE-level names (type
;; constructors, classes, type families).  It matches the term-level
;; transform for every sub-form except `qualified-in`, which prefixes
;; only term-level names: types keep their plain names, so an imported
;; constructor's result type still names the same (unprefixed) type the
;; importer writes in annotations.
(define (require-spec->type-name-transform d)
  (cond
    [(and (pair? d) (eq? (car d) 'qualified-in))
     (require-spec->name-transform (caddr d))]
    [else (require-spec->name-transform d)]))

;; Rewrite a tcon-info's `ctors` list through the term-level name
;; transform `xform`, so the type's constructors are named as they are
;; bound in the importing env (a constructor dropped by the transform is
;; removed from the list).
(define (rename-tcon-info-ctors ti xform)
  (struct-copy tcon-info ti
    [ctors (filter values (map xform (tcon-info-ctors ti)))]))

;; Index a require clause list (only-in / rename-in) by importee name.  A
;; bare `id` keeps its name; a `[orig new]` clause renames `orig` to `new`.
(define (require-rename-table clauses)
  (for/hasheq ([c (in-list clauses)])
    (cond
      [(symbol? c)                      (values c c)]
      [(and (pair? c) (pair? (cdr c)))  (values (car c) (cadr c))]
      [else                             (values c c)])))

;; Fold imported entries into `acc` under the names `xform` selects, each
;; entry being `(name . encoded)`.  An entry whose name `xform` drops (#f)
;; is skipped; the rest are added via `extend`, called as
;; `(extend acc selected-name encoded)`.
(define (fold-imported xform acc entries extend)
  (for/fold ([a acc]) ([entry (in-list entries)])
    (define nm (xform (car entry)))
    (if nm (extend a nm (cdr entry)) a)))

;; Resolve a require spec syntax to a usable `(submod ... rackton-schemes)`
;; module path, or #f for an unhandled spec shape.
(define (require-spec->submod-spec spec-stx)
  (define mp (require-spec->module-path spec-stx))
  (and mp `(submod ,mp rackton-schemes)))

;; Does a base module path name a module that actually exists?  A
;; collection path that names no installed collection makes
;; `resolve-module-path` raise; a `(file ...)` path that resolves but
;; is not on disk comes back as a path that fails `file-exists?`.  Both
;; mean "no such module".
(define (module-path-exists? mp)
  (with-handlers ([exn:fail? (lambda (_) #f)])
    (define resolved (resolve-module-path mp))
    (and (path? resolved) (file-exists? resolved))))

(define (surface-kind->core k)
  (match k
    [(k:star)      (kind-star)]
    [(k:nat)       (kind-nat)]
    [(k:con n)     (kind-con n)]
    [(k:app h as) (kapp (kind-con h) (map surface-kind->core as))]
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
         [(e:open e _ vv body _)
          (walk body (or shadowed? (eq? vv name)) (walk e shadowed? st))]
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
         [(e:tuple elems _)
          (for/fold ([st st]) ([el (in-list elems)]) (walk el shadowed? st))]
         [(e:bits segs _)
          (for/fold ([st st]) ([sg (in-list segs)]) (walk (bit-seg-subject sg) shadowed? st))]
         [(e:tref t _ _) (walk t shadowed? st)]
         [(e:array elems _)
          (for/fold ([st st]) ([el (in-list elems)]) (walk el shadowed? st))]
         [(e:build-array _ p _) (walk p shadowed? st)]
         [(e:aref a _ _) (walk a shadowed? st)]
         [(e:array-slice _ _ a _) (walk a shadowed? st)]
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
    [(p:tuple args _)
     (for/or ([a (in-list args)]) (pattern-binds-name? a name))]
    [(p:bits segs _)
     (for/or ([sg (in-list segs)]) (pattern-binds-name? (bit-seg-subject sg) name))]
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
      [(e:open? e) (visit (e:open-expr e))
                   (visit (e:open-body e))]
      [(e:ann? e) (visit (e:ann-expr e))]
      [(e:match? e) (visit (e:match-scrutinee e))
                    (for ([c (in-list (e:match-clauses e))])
                      (when (clause-guard c) (visit (clause-guard c)))
                      (visit (clause-body c)))]
      [(e:update? e) (visit (e:update-record e))
                     (for ([u (in-list (e:update-updates e))])
                       (visit (cdr u)))]
      [(e:bits? e) (for ([sg (in-list (e:bits-segs e))])
                     (visit (bit-seg-subject sg)))]
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

;; Cross-class default/derived cycle check for a `:derive-supers`
;; instance.  `check-instance-default-cycle` above is intra-class: it
;; only sees edges between methods of ONE class, so it cannot detect a
;; loop that runs between a deriving-class method (left to its class
;; default) and a superclass method whose body comes from the deriving
;; class's `:derive` table.  That is exactly the loop the user can write
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
  ;; write it: a `:derive`-table entry (superclass methods only) else the
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
        "default/derived chain across protocols (~a); define at least one "
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
      (format "unknown protocol: ~s~a"
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
        (format "instance head for protocol ~s expects ~a type argument~a ~s, but got ~a: ~s"
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
      (kind-check-surface env (inst-type-fam-type m))
      (hash-set acc (inst-type-fam-name m) (resolve-type (inst-type-fam-type m) env))))
  (for ([fam (in-list (class-info-type-families cinfo))])
    (unless (hash-has-key? type-family-bindings fam)
      (raise-syntax-error 'infer
        (format "instance ~s missing :type binding for associated type ~s"
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
    (normalize-type/guarded env-with-inst
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
      (parameterize ([current-reduce-blame stx])
        (reduce-context env
                        (map (lambda (p) (apply-subst final-subst p)) hyp-preds)
                        (st:preds st4))))
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


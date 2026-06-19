#lang racket/base

;; Rackton — typing environments.
;;
;; An `env` aggregates three persistent maps:
;;   - vars       : identifier symbol → type scheme
;;   - data-ctors : data-constructor symbol → data-info
;;   - tcons      : type-constructor symbol → tcon-info
;;
;; Each map is an immutable eq-keyed hash; extensions return new envs.

(provide (struct-out env)
         (struct-out data-info)
         (struct-out tcon-info)
         (struct-out class-info)
         (struct-out instance-info)
         (struct-out tyfam-info)

         empty-env
         env-extend-var
         env-ref-var
         env-extend-data
         env-ref-data
         env-extend-tcon
         env-ref-tcon
         env-extend-promoted-ctor
         env-ref-promoted-ctor
         env-extend-tyfam
         env-ref-tyfam
         env-tyfam-names
         env-add-tyfam-clause
         env-extend-constraint-syn
         env-ref-constraint-syn
         env-extend-class
         env-clear-instances
         env-ref-class
         env-return-typed-methods
         env-class-owning-family
         env-extend-instance
         env-instances
         env-set-instances
         env-ref-method-class
         env-extend-alias
         env-ref-alias
         env-extend-struct-fields
         env-ref-struct-fields
         env-extend-effect
         env-ref-effect
         env-effect-of-op
         env-vars-free-vars
         env-remove-var
         apply-subst/env

         initial-env)

(require racket/set
         "types.rkt")

;; ----- structures ----------------------------------------------------

;; `struct-fields` maps a struct's type-name to its ordered list of
;; field-name symbols.  Populated by handling `top:struct-fields`
;; forms; consumed by inference and codegen for `e:update`.
;; `effects` maps an effect's name to its ordered list of operation
;; names.  Consumed by inference and codegen for `(handle ...)` so
;; each op is dispatched on the correct prompt-tag.
;; `promoted-ctors` maps a DataKinds-promoted type-level constructor's
;; name to its promoted kind (e.g. SPush ↦ Ty -> Stack -> Stack).  It is
;; a separate table from `tcons` so promotion only adds type-level
;; identities and never perturbs value-level data, codegen, or
;; exhaustiveness, which read `data-ctors`/`tcons` alone.
;; `tyfams` maps a STANDALONE type-family name to its `tyfam-info`
;; (Feature 1): the ordered clauses of a closed family or the coherent
;; equation set of an open family.  Separate from `classes` (associated
;; `#:type` families) so the two reduction mechanisms stay independent.
;; `constraint-syns` maps a constraint-synonym name to `(params . preds)`
;; — its parameters and the core predicates it abbreviates.  A `(C T…)`
;; constraint expands to those preds with params substituted by T….
(struct env (vars data-ctors tcons classes instance-table method-owners aliases
             struct-fields effects promoted-ctors tyfams constraint-syns)
  #:transparent)

;; A standalone type family's reduction information.
;;   `arity`     — number of parameters;
;;   `kind`      — the family's kind scheme (or #f until inferred);
;;   `openness`  — 'closed (ordered clauses) or 'open (coherent equations);
;;   `clauses`   — list of `(cons (listof core-type) core-type)` =
;;                 (LHS patterns . RHS), resolved to core types.
(struct tyfam-info (name arity kind openness clauses) #:transparent)

;; A data-constructor's typing information.  `scheme` is the polymorphic
;; type assigned to the constructor when used as a value.  `arity` is
;; the number of arguments the constructor takes.
;; `ex-tvars` lists the existentially-quantified tvars
;; introduced by a `#:forall ... #:where ...` ctor.  These are the
;; tail of `scheme`'s quantifier list — at pattern-match time the
;; inferer skolemizes only these (the data type's tparams stay as
;; fresh tvars to unify with the scrutinee).  Empty for ordinary
;; constructors.
(struct data-info (type-name ctor-name arity scheme ex-tvars) #:transparent)

;; A type-constructor's kinding information.  `ctors` lists every
;; data constructor that produces this type — used for exhaustiveness.
;; `abstract?` records whether the data type is sealed —
;; its ctors are not re-exported across module boundaries.  Default
;; #f keeps existing code untouched.
;; `runtime-tag` (U Symbol #f): for an opaque (data T) whose runtime
;; values are a host struct (reached via `foreign`), the dispatch tag
;; those values carry (= the struct's type name, what dispatch-tag
;; returns).  When set, an instance's positional methods register under
;; this tag instead of T's (nonexistent) constructor tags, so dispatch
;; on those opaque values resolves.  #f for ordinary types.
;; `kind` is the type constructor's kind SCHEME (e.g. `* -> *` for
;; `List`, or `∀k. k -> *` for a phantom-parameter type), inferred and
;; generalised at the data declaration; `(kscheme-mono (arity->star-kind
;; arity))` is the placeholder/fallback (e.g. for legacy sidecars).
(struct tcon-info (name arity kind ctors abstract? runtime-tag) #:transparent)

;; A class's static information.
;;   name        : symbol — the class name
;;   params      : (Listof symbol) — class type parameter names
;;   kinds       : (HashEq symbol → kind) — each param's kind (default *)
;;   supers      : (Listof pred) — superclass constraints, over the params
;;   methods     : (HashEq method-name → scheme)
;;                 — each method's qualified scheme as visible at the value
;;                   level (the class constraint is already attached).
;;   defaults    : (HashEq method-name → surface-expr) — class defaults
;;   dispatchpos : (HashEq method-name → exact-nonnegative-integer)
;;                 — index of the argument whose runtime tag selects the
;;                   instance for each method call.  Computed at class
;;                   definition by walking the method type and locating
;;                   the first top-level arg whose type mentions a class
;;                   parameter.
;;   fundeps     : (Listof (Pair (Listof symbol) (Listof symbol)))
;;                 — functional dependencies; each pair maps the
;;                   determining-param names to the determined-param
;;                   names.  Empty list when no FDs are declared.
;;   dictreqs    : (HashEq method-name → (Listof symbol))
;;                 — for each method whose qualifying context demands
;;                   an additional class constraint over a param that
;;                   appears in the method type (e.g. `traverse`'s
;;                   `Applicative f`), the list of constraint class
;;                   names whose return-typed methods need to be
;;                   passed as extra leading args at call sites.
;; `type-families` is the list of associated-type names declared by
;; the class via `#:type Foo`.  Each instance must supply concrete
;; bindings for every declared family.
;;   super-derives : (HashEq superclass-name → (HashEq method-name →
;;                   surface-expr)) — cross-class derivation table.  For
;;                   each `[Super …]` clause in the body's `#:derive` list,
;;                   the canonical bodies that fill `Super`'s methods in
;;                   terms of this class's own methods.  Consumed when an
;;                   instance opts into `#:derive-supers`.  Empty
;;                   for classes that declare no derivations.  Not
;;                   serialized (like `defaults`), so a USER class's
;;                   derivations are available only within its defining
;;                   module; the prelude monad stack works everywhere.
;;   laws        : (Listof class-law) — the named quantified equations
;;                   declared by the body's `#:laws` clause, type-checked
;;                   at class elaboration.  Formal documentation of the
;;                   invariants instances must satisfy; carries no runtime
;;                   behaviour.  Not serialized (like `defaults`), so a
;;                   class's laws are available only within its defining
;;                   module.  Empty for classes that declare none.
(struct class-info (name params kinds supers methods defaults dispatchpos
                    fundeps dictreqs type-families super-derives laws)
  #:transparent)

;; An instance's information.
;;   head    : pred — the instance head, e.g. (Eq Integer) or (Eq (Maybe a))
;;   context : (Listof pred) — qualifying preds for this instance
;;   methods : (HashEq method-name → surface-expr) — method bodies
;;   type-family-bindings : (HashEq family-name → type) — empty for
;;                          classes with no associated types
;;   origin  : (U String #f) — identity of the module that ORIGINALLY
;;             declared this instance (its source path), preserved across
;;             re-export through the rackton-schemes sidecar.  Lets the
;;             coherence check dedup the SAME instance reaching the env by
;;             two import paths (a diamond) while still rejecting two
;;             DIFFERENT instances that share a head.  #f when unknown
;;             (prelude instances, hand-built test fixtures).
;; `prelude?` is #t for instances registered while building the prelude env —
;; runtime-only impls the monomorphization resolver must not redirect to a
;; named (compile-instance-emitted) define.  Replaces the old module-level
;; prelude-instances-table; set intrinsically at construction.
(struct instance-info (head context methods type-family-bindings origin prelude?) #:transparent)

(define empty-env (env (hasheq) (hasheq) (hasheq) (hasheq) (hasheq) (hasheq) (hasheq) (hasheq) (hasheq) (hasheq) (hasheq) (hasheq)))

;; ----- basic accessors ----------------------------------------------

(define (env-extend-var e x sch)
  (struct-copy env e [vars (hash-set (env-vars e) x sch)]))

(define (env-ref-var e x [default #f])
  (hash-ref (env-vars e) x default))

(define (env-extend-data e ctor info)
  (struct-copy env e [data-ctors (hash-set (env-data-ctors e) ctor info)]))

(define (env-ref-data e ctor [default #f])
  (hash-ref (env-data-ctors e) ctor default))

(define (env-extend-tcon e name info)
  (struct-copy env e [tcons (hash-set (env-tcons e) name info)]))

(define (env-ref-tcon e name [default #f])
  (hash-ref (env-tcons e) name default))

(define (env-extend-promoted-ctor e name kind)
  (struct-copy env e
               [promoted-ctors (hash-set (env-promoted-ctors e) name kind)]))

(define (env-ref-promoted-ctor e name [default #f])
  (hash-ref (env-promoted-ctors e) name default))

;; ----- standalone type families -------------------------------------

(define (env-extend-tyfam e name info)
  (struct-copy env e [tyfams (hash-set (env-tyfams e) name info)]))

(define (env-ref-tyfam e name [default #f])
  (hash-ref (env-tyfams e) name default))

;; The set of all standalone type-family names — consumed by the
;; normalizer's guard so reduction is attempted only on types that
;; actually mention a family.
(define (env-tyfam-names e)
  (for/seteq ([k (in-hash-keys (env-tyfams e))]) k))

;; Append one equation `(pats . rhs)` to an existing OPEN family — used
;; by the per-form (REPL) path where a `type-instance` arrives after its
;; family declaration.  No-op if the family is unknown.
;; ----- constraint synonyms ------------------------------------------

(define (env-extend-constraint-syn e name params preds)
  (struct-copy env e
               [constraint-syns
                (hash-set (env-constraint-syns e) name (cons params preds))]))

(define (env-ref-constraint-syn e name [default #f])
  (hash-ref (env-constraint-syns e) name default))

(define (env-add-tyfam-clause e name clause)
  (define info (env-ref-tyfam e name))
  (cond
    [(not info) e]
    [else
     (env-extend-tyfam
      e name
      (struct-copy tyfam-info info
                   [clauses (append (tyfam-info-clauses info) (list clause))]))]))

;; ----- classes & instances ------------------------------------------

;; Drop all instances registered for `class-name`.  Called
;; when a class is redeclared so its prior instances (which belonged
;; to the old class definition) don't leak into the new one's
;; instance set.
(define (env-clear-instances e class-name)
  (struct-copy env e
               [instance-table
                (hash-remove (env-instance-table e) class-name)]))

(define (env-extend-class e name info)
  (define vars*
    ;; Make each method visible at the value level with its method scheme.
    (for/fold ([acc (env-vars e)])
              ([(method-name sch) (in-hash (class-info-methods info))])
      (hash-set acc method-name sch)))
  (define owners*
    (for/fold ([acc (env-method-owners e)])
              ([method-name (in-hash-keys (class-info-methods info))])
      (hash-set acc method-name name)))
  (struct-copy env e
               [vars vars*]
               [classes (hash-set (env-classes e) name info)]
               [method-owners owners*]))

(define (env-ref-class e name [default #f])
  (hash-ref (env-classes e) name default))

;; The set of all method names (across every class in scope) whose
;; dispatch is return-typed ('return).  Codegen consults this to route
;; their call sites through the per-method runtime dispatch table
;; (so an instance defined in another module is reachable), rather than
;; a direct per-instance impl reference that doesn't cross the boundary.
(define (env-return-typed-methods e)
  (for*/fold ([acc (seteq)])
             ([(_cn ci) (in-hash (env-classes e))]
              [(m dp)   (in-hash (class-info-dispatchpos ci))]
              #:when (eq? dp 'return))
    (set-add acc m)))

(define (env-extend-instance e class-name inst)
  (struct-copy env e
               [instance-table
                (hash-update (env-instance-table e) class-name
                             (lambda (cur) (append cur (list inst)))
                             (lambda () '()))]))

(define (env-instances e class-name)
  (hash-ref (env-instance-table e) class-name '()))

;; Replace the whole instance list for `class-name`.  Used for REPL
;; instance redefinition (drop an α-equivalent old instance, then add the
;; new one); the equivalence test lives in the caller.
(define (env-set-instances e class-name insts)
  (struct-copy env e
               [instance-table
                (hash-set (env-instance-table e) class-name insts)]))

(define (env-ref-method-class e method [default #f])
  (hash-ref (env-method-owners e) method default))

;; Which class declared this associated-type name?
;; Returns the class name on the first match, or #f.
(define (env-class-owning-family e family-name)
  (for/or ([(cname ci) (in-hash (env-classes e))])
    (and (memq family-name (class-info-type-families ci))
         cname)))

;; ----- type aliases -----------------------------------------------
;; `aliases` maps an alias name to (cons param-list target-ty-ast).
;; The target-ty-ast is the surface ty:* AST so that resolution can
;; substitute and recurse uniformly.

(define (env-extend-alias e name params target)
  (struct-copy env e
               [aliases (hash-set (env-aliases e) name (cons params target))]))

(define (env-ref-alias e name [default #f])
  (hash-ref (env-aliases e) name default))

;; Register a struct's ordered field-name list.
(define (env-extend-struct-fields e struct-name field-names)
  (struct-copy env e
               [struct-fields
                (hash-set (env-struct-fields e) struct-name field-names)]))

;; Look up a struct's field-name list, or #f if unknown.
(define (env-ref-struct-fields e struct-name [default #f])
  (hash-ref (env-struct-fields e) struct-name default))

;; Register an effect's operation list under its name.
(define (env-extend-effect e effect-name op-names)
  (struct-copy env e
               [effects (hash-set (env-effects e) effect-name op-names)]))

;; Look up an effect's operation list, or #f if unknown.
(define (env-ref-effect e effect-name [default #f])
  (hash-ref (env-effects e) effect-name default))

;; Which effect declared a given operation name?  Returns
;; the effect's name on the first match or #f.
(define (env-effect-of-op e op-name)
  (for/or ([(ename ops) (in-hash (env-effects e))])
    (and (memq op-name ops) ename)))

;; Free type variables across every value binding's scheme — needed
;; for `generalize` at let bindings.
(define (env-vars-free-vars e)
  (for/fold ([acc (seteq)]) ([(_ sch) (in-hash (env-vars e))])
    (set-union acc (scheme-free-vars sch))))

;; Drop a single variable binding from env.  Used during SCC-based
;; generalization: when generalizing one binding of a mutual group,
;; the *other* bindings' placeholder tvars must be invisible so that
;; `generalize`'s env-vars-free-vars computation doesn't pin them.
(define (env-remove-var e x)
  (struct-copy env e [vars (hash-remove (env-vars e) x)]))

;; Lift a substitution over every scheme in the value env.  Data and
;; tcon envs are unaffected — their schemes never mention free tvars.
(define (apply-subst/env s e)
  (cond
    [(hash-empty? s) e]
    [else
     (struct-copy env e
                  [vars
                   (for/hasheq ([(k sch) (in-hash (env-vars e))])
                     (values k (apply-subst/scheme s sch)))])]))

;; ----- the initial env ----------------------------------------------
;; Built-in primitive operators are available from the very start of
;; any rackton program.  Each is monomorphic; arithmetic ops fix
;; Integer because we have no Num class yet.

(define (mono t) (scheme '() t))
(define INT->INT->INT  (make-arrow t-int  (make-arrow t-int t-int)))
(define INT->INT->BOOL (make-arrow t-int  (make-arrow t-int t-bool)))

(define initial-env
  (for/fold ([e empty-env])
            ([binding (in-list
                       `((+  . ,INT->INT->INT)
                         (-  . ,INT->INT->INT)
                         (*  . ,INT->INT->INT)
                         (=  . ,INT->INT->BOOL)
                         (<  . ,INT->INT->BOOL)
                         (>  . ,INT->INT->BOOL)
                         (<= . ,INT->INT->BOOL)
                         (>= . ,INT->INT->BOOL)))])
    (env-extend-var e (car binding) (mono (cdr binding)))))

#lang racket/base

;; Rackton — the typed-core source AST.
;;
;; The data vocabulary shared by the parser (surface.rkt), inference
;; (infer.rkt), codegen (codegen.rkt), and deriving (deriving.rkt): every
;; expression / type / pattern / top-form struct, plus the syntax-handle
;; re-anchoring operations (relocate-ast / freshen-ast) and `fresh-stx`.
;; Factored out of surface.rkt so the AST is one module with one reason to
;; change, and so deriving.rkt can build AST nodes without a require cycle
;; back through the parser.
;;
;; Every node carries its originating `stx` for sourcemap-aware errors.

(require racket/match)

(provide (all-defined-out))

;; ----- AST -----------------------------------------------------------

(struct e:literal (value stx) #:transparent)
(struct e:var     (name stx) #:transparent)
(struct e:lam     (params body stx) #:transparent)
(struct e:app     (head args stx) #:transparent)
;; Functional record update.  `record-expr` evaluates to a
;; record value; each `update` element is (cons field-name value-expr),
;; meaning the resulting record has `field-name` replaced by the
;; value of `value-expr` and all other fields preserved.  The result
;; type equals the type of `record-expr`.
(struct e:update  (record updates stx) #:transparent)
;; A variadic tuple constructor: `(tuple e …)` builds a heterogeneous,
;; fixed-arity product of type `(Tuple T …)`.  `elems` is the list of
;; element expressions, in order.
(struct e:tuple   (elems stx) #:transparent)
;; Indexed tuple access: `(tref t n)`.  `index` is a non-negative
;; integer LITERAL (the parser rejects anything else), so the reference
;; is bounds-checked against the tuple's arity at inference time.
(struct e:tref    (tuple-expr index stx) #:transparent)
(struct e:let     (bindings body stx) #:transparent)
(struct e:if      (test then else stx) #:transparent)
(struct e:ann     (expr type stx) #:transparent)
;; `irrefutable?` flag: when #t the match was synthesized by a
;; destructuring binding (a pattern LHS in `let` / `where`) and is
;; allowed to be non-exhaustive — the user is asserting the pattern
;; fits.  When #f (the default for user-written `match`), exhaustiveness
;; is checked.
(struct e:match   (scrutinee clauses irrefutable? stx) #:transparent)
;; A host-language escape: (racket τ (var ...) body) drops into raw
;; Racket, returning a value typed as τ.  `vars` lists the Rackton
;; bindings that must be in scope.  `body` is a single Racket syntax
;; object that is spliced verbatim at codegen time.
(struct e:escape  (type vars body stx) #:transparent)
;; A `(letrec ([name expr] ...) body)` form.  Each binding's rhs may
;; reference every binding's name (mutual recursion).
(struct e:letrec  (bindings body stx) #:transparent)
;; A `match` clause.  `guard` is either #f (no guard) or a surface
;; expression that must evaluate to #t for the clause to fire.
(struct clause    (pattern guard body stx) #:transparent)

;; Multi-scrutinee match.  Used internally to lower multi-clause
;; `define` forms like
;;     (define (head (Cons x _)) x)
;;     (define (head Nil)        ...)
;; into a single function whose body matches on every parameter at
;; once with cross-clause fall-through.  Each `clause*` carries one
;; pattern per scrutinee in source order.  `irrefutable?` flag has
;; the same meaning as on `e:match`: when #t the exhaustiveness
;; checker skips the form (the user — or the multi-clause combiner —
;; has asserted the clauses cover every case).
(struct e:match*  (scrutinees clauses irrefutable? stx) #:transparent)
(struct clause*   (patterns guard body stx) #:transparent)

(struct ty:var    (name stx) #:transparent)
(struct ty:con    (name stx) #:transparent)
;; A type-level natural-number literal in type position, e.g. the `3`
;; in the type `(Array 3 a)`.  `value` is a non-negative integer.
(struct ty:nat    (value stx) #:transparent)
(struct ty:app    (head args stx) #:transparent)
(struct ty:forall (vars body stx) #:transparent)
(struct ty:qual   (constraints body stx) #:transparent)
;; A constraint is `(C arg ...)` in surface syntax — class name + type args.
(struct constraint (class args stx) #:transparent)

(struct p:wild    (stx) #:transparent)
(struct p:var     (name stx) #:transparent)
(struct p:lit     (value stx) #:transparent)
(struct p:ctor    (name args stx) #:transparent)
;; A tuple pattern `(tuple p …)`: matches a tuple of exactly `(length
;; elems)` elements, destructuring each.  Structurally irrefutable —
;; the tuple's arity is fixed by its type.
(struct p:tuple   (elems stx) #:transparent)

;; Fixed-size arrays (see private/array-runtime.rkt).
;; `(array e …)` — listing constructor; size is the element count.
(struct e:array  (elems stx) #:transparent)
;; `(build-array n f)` — sized builder; `size` is a non-negative integer
;; LITERAL (so it fixes the type-level size), `proc` an `(-> Integer a)`.
(struct e:build-array (size proc stx) #:transparent)
;; `(aref arr n)` — indexed element read; `index` is a non-negative
;; integer literal, bounds-checked against a concrete size at inference.
(struct e:aref   (array-expr index stx) #:transparent)
;; A concrete-size slice: `op` is 'take / 'drop / 'split, `index` a
;; non-negative integer literal (the split point), `array-expr` the
;; array.  Requires a concrete array size so the result size is computed
;; and the point is bounds-checked at inference.
(struct e:array-slice (op index array-expr stx) #:transparent)

(struct top:def      (name expr stx) #:transparent)
(struct top:dec      (name type stx) #:transparent)
;; `abstract?` flag — when #t, the data type's ctors are
;; NOT re-exported to importing modules.  The type-name itself is.
(struct top:data     (name params ctors stx abstract? runtime-tag) #:transparent)
;; A data ctor may carry its own existential quantifier
;; via `#:forall (a) #:where (Cls a) ...` keywords between the ctor
;; name and the field types.  `extra-tvars` lists the existentially
;; quantified tvars; `extra-context` lists the constraints over them.
;; Existing (non-existential) ctors use empty lists for both.
;; `result-type` is either #f (default — the data type's
;; `(T tparams)` shape) or a ty-AST giving the ctor's specific
;; result type for GADTs (declared via `: (-> ft … RT)`, where the
;; final arrow type is the result and the leading types are fields).
(struct data-ctor    (name field-types stx extra-tvars extra-context
                      result-type)
  #:transparent)

;; Helper: build a non-existential data-ctor (most common case).
(define (data-ctor-plain name field-types stx)
  (data-ctor name field-types stx '() '() #f))
;; A class declaration carries an explicit list of parameters with kinds
;; (defaulting to *), the optional superclass list, the head class name,
;; and the body (signatures + defaults).
(struct top:class    (supers head methods stx) #:transparent)
(struct top:instance (context head methods stx) #:transparent)
;; An instance written with `#:derive-supers`: the user bundles
;; only the irreducible primitives (e.g. `pure` and `flatmap`) and the
;; compiler synthesizes the missing superclass instances from the
;; deriving class's cross-class derivation table.  Expanded into plain
;; `top:instance` forms by `expand-derive-instances` in infer.rkt before
;; any other phase sees it.  Same shape as `top:instance`.
(struct top:derive-instance (context head methods stx) #:transparent)
;; A `[Super (define …) …]` clause from the `#:derive (… …)` list in a
;; `protocol` body: canonical bodies that fill superclass `super`'s methods
;; in terms of this class's own methods.  `methods` is a list of
;; `method-default`.
(struct class-super-derive  (super methods stx) #:transparent)

;; Re-anchor every syntax handle in a parsed AST.  A class default (or a
;; cross-class derivation body) was parsed in the class's defining
;; module; relocating it to the instance site makes its identifier
;; references resolve via the instance module's imports.  `relocate-ast`
;; gives every node the same `new-stx` — fine for codegen, which runs
;; after method resolution is fixed.  `freshen-ast` gives every node a
;; FRESH, distinct handle (same context + source location as `anchor`) —
;; needed when an AST is relocated BEFORE inference and inferred per
;; instance, since the method-resolution map keys uses by syntax
;; identity, so a shared handle would collapse unrelated uses together.
(define (relocate-ast node new-stx)
  (remap-ast-stx node (lambda (_) new-stx)))

(define (freshen-ast node anchor)
  (remap-ast-stx node (lambda (_) (fresh-stx anchor))))

;; Rebuild `node`, replacing each node's syntax handle with `(mk old)`.
(define (remap-ast-stx node mk)
  (define (R x) (remap-ast-stx x mk))
  (define new-stx (mk node))
  (match node
    [(e:literal v _)     (e:literal v new-stx)]
    [(e:var n _)         (e:var n new-stx)]
    [(e:lam p body _)    (e:lam p (R body) new-stx)]
    [(e:app h args _)    (e:app (R h) (map R args) new-stx)]
    [(e:let bs body _)
     (e:let (for/list ([b (in-list bs)]) (cons (car b) (R (cdr b))))
            (R body) new-stx)]
    [(e:letrec bs body _)
     (e:letrec (for/list ([b (in-list bs)]) (cons (car b) (R (cdr b))))
               (R body) new-stx)]
    [(e:if a b c _)      (e:if (R a) (R b) (R c) new-stx)]
    [(e:ann e t _)       (e:ann (R e) (R t) new-stx)]
    [(e:escape t vs body _)
     ;; Escapes splice raw Racket syntax — relocating it would mean
     ;; rewriting that user-written code, which we don't want to do.
     (e:escape (R t) vs body new-stx)]
    [(e:match s cs irr? _)
     (e:match (R s)
              (for/list ([c (in-list cs)])
                (clause (R (clause-pattern c))
                        (and (clause-guard c) (R (clause-guard c)))
                        (R (clause-body c)) new-stx))
              irr? new-stx)]
    [(e:match* ss cs irr? _)
     (e:match* (map R ss)
               (for/list ([c (in-list cs)])
                 (clause* (map R (clause*-patterns c))
                          (and (clause*-guard c) (R (clause*-guard c)))
                          (R (clause*-body c)) new-stx))
               irr? new-stx)]
    [(e:tuple es _)      (e:tuple (map R es) new-stx)]
    [(e:tref t i _)      (e:tref (R t) i new-stx)]
    [(e:array es _)      (e:array (map R es) new-stx)]
    [(e:build-array n p _) (e:build-array n (R p) new-stx)]
    [(e:aref a i _)      (e:aref (R a) i new-stx)]
    [(e:array-slice op i a _) (e:array-slice op i (R a) new-stx)]
    [(p:wild _)          (p:wild new-stx)]
    [(p:var n _)         (p:var n new-stx)]
    [(p:lit v _)         (p:lit v new-stx)]
    [(p:ctor n args _)   (p:ctor n (map R args) new-stx)]
    [(p:tuple ps _)      (p:tuple (map R ps) new-stx)]
    [(ty:var n _)        (ty:var n new-stx)]
    [(ty:con n _)        (ty:con n new-stx)]
    [(ty:nat v _)        (ty:nat v new-stx)]
    [(ty:app h args _)   (ty:app (R h) (map R args) new-stx)]
    [(ty:forall vs b _)  (ty:forall vs (R b) new-stx)]
    [(ty:qual cs b _)
     (ty:qual (for/list ([c (in-list cs)]) (R c)) (R b) new-stx)]
    [(constraint c args _) (constraint c (map R args) new-stx)]))
;; Items inside a `protocol` body:
(struct method-sig     (name type stx) #:transparent)
(struct method-default (name expr stx) #:transparent)
;; A functional dependency declaration: `(#:fundep lhs … -> rhs …)`
;; appearing in a class body says that, in every instance, the
;; rhs-positioned types are uniquely determined by the lhs-positioned
;; ones.  `lhs` and `rhs` are lists of parameter-name symbols.
(struct class-fundep   (lhs rhs stx) #:transparent)
;; A `#:type FamilyName` declaration inside a class body.
;; The family is a one-parameter type-level function whose argument
;; is the class's parameter; each instance supplies a concrete rhs.
(struct class-type-fam  (name stx) #:transparent)

;; A STANDALONE type family (Feature 1), distinct from the associated
;; `#:type` families above.  `(type-family (F p …) [::k] clause …)`:
;;   `params` are the family's parameter names;
;;   `kind` is the surface kind AST after `::`, or #f to infer;
;;   `clauses` is a list of `tyfam-clause` — non-empty ⇒ CLOSED (ordered
;;   equations), empty ⇒ OPEN (extended by `top:type-instance` forms).
(struct top:type-family  (name params kind clauses stx) #:transparent)
;; One closed-family equation: `pats` are the per-parameter LHS type
;; patterns (surface ty AST), `rhs` the result type (surface ty AST).
(struct tyfam-clause     (pats rhs stx) #:transparent)
;; A standalone open-family equation `(type-instance (F T …) = U)`:
;; `args` are the LHS argument types, `rhs` the result type.
(struct top:type-instance (name args rhs stx) #:transparent)

;; A data family `(data-family (F p …) [:: k])` — a type constructor with
;; NO constructors of its own; each `data-instance` adds some.  `kind` is
;; the surface kind after `::`, or #f to infer.
(struct top:data-family   (name params kind stx) #:transparent)
;; A data instance `(data-instance (F T …) ctor …)`: `args` are the head
;; type arguments, `ctors` the `data-ctor`s introduced for this instance
;; (their result type is the head, GADT-style).
(struct top:data-instance (name args ctors stx) #:transparent)
;; One named law from a `#:laws ([name (ctx … => (All …))] …)` clause in
;; a class body: a quantified equation documenting an invariant the
;; class's instances must satisfy.  `name` is the law's identifier;
;; `context` is a list of `constraint`s assumed only while type-checking
;; this law — typically `(Eq a)` or `(Eq (f Integer))`, so the equation
;; may compare results — without becoming a superclass requirement on
;; instances; `binders` is a list of `law-binder`; `body` is the
;; (Boolean-typed) equation expression.  Laws are formal documentation —
;; type-checked at class elaboration but not executed here.
(struct class-law      (name context binders body stx) #:transparent)
;; One `[var : type]` quantifier binder of a `class-law`.
(struct law-binder     (name type stx) #:transparent)
;; A `#:type (FamilyName = Type)` clause inside an instance
;; body, binding the named family to a concrete type for this
;; instance.
(struct inst-type-fam   (name type stx) #:transparent)
;; A multi-file import: `(require "file.rkt" ...)` inside a rackton form.
;; Specs are the raw require specs (passed verbatim to Racket's require).
(struct top:require    (specs stx) #:transparent)
;; A foreign (host) import: `(foreign name τ #:from M [#:as rkt-id])`.
;; Declares the Rackton-typed binding `name` of type `type`, backed by
;; the Racket binding `racket-id` from module path `module-path`.  The
;; declared type is the trust boundary (unchecked — FFI-style).  Like a
;; bare `(: …)` dec at the type level (mconcat is the prelude precedent),
;; plus a Racket-level `require` emitted by codegen for the binding.
(struct top:foreign    (name type module-path racket-id stx) #:transparent)
;; An inline C-function import:
;;   (foreign-c name τ #:lib L #:symbol S #:sig (cty ... -> cty))
;; Binds `name` of Rackton type `type` to the C function `symbol` in
;; shared library `lib` (a string, or #f for the running process), with
;; the C signature given by `arg-tags` / `result-tag` (ctype keywords:
;; double int string pointer void byte).  `io?` (computed from τ: does
;; the result, after the C arity's arrows, sit in IO?) selects whether
;; the binding is a pure function or an IO action.  Trusted boundary,
;; like `foreign` — lowers to get-ffi-obj via private/ffi-runtime.
(struct top:foreign-c  (name type lib symbol arg-tags result-tag io? stx) #:transparent)
;; A user-level export declaration: `(provide spec ...)` inside a
;; rackton block.  `specs` is the raw list of syntax objects — the
;; elaborator resolves each spec against the final env into four
;; export sets (vars, data-ctors, tcons, classes), so we keep specs
;; literal here rather than parsing into a Rackton-specific AST.
(struct top:provide    (specs stx) #:transparent)
;; A type alias.  `params` is a list of symbols (possibly empty); `target`
;; is a parsed surface type AST that may mention the params.
(struct top:alias      (name params target stx) #:transparent)
;; Side-channel record-field-name registration.  Emitted by
;; parse-struct-form alongside the top:data form so the inference
;; engine can record the field names on the struct's data-info.
;; Carrying this as a separate top-form keeps data-ctor untouched.
(struct top:struct-fields (struct-name field-names stx) #:transparent)

;; An effect declaration.  `name` is the effect's name;
;; `ops` is a list of effect-op describing each operation.
(struct top:effect    (name ops stx) #:transparent)
;; A single operation inside a `define-effect`.  `arg-types` is a
;; list of surface type ASTs; `result-type` is the result type AST.
(struct effect-op     (name arg-types result-type stx) #:transparent)

;; (handle EXPR [op (args ...) k -> body] ... [return v -> body])
;; `expr` is a 0-arg thunk (or a value that's run/forced inside
;; handle's compiled form).  `clauses` lists op-clauses; `return`
;; describes the return-clause.
(struct e:handle       (expr clauses return stx) #:transparent)
;; One operation-handling clause: (op (param ...) k -> body).
(struct handle-clause  (op params k-name body stx) #:transparent)
;; The single `[return v -> body]` clause that wraps the final
;; value when the body finishes without performing an op.
(struct handle-return  (var body stx) #:transparent)

;; Kinds at the surface level — used to annotate class and data parameters.
(struct k:star ()        #:transparent)
(struct k:arr  (dom cod) #:transparent)
;; The surface `Nat` kind.
(struct k:nat  ()        #:transparent)
;; A DataKinds-promoted datatype used as a kind, e.g. `Stack` in
;; `(g :: Stack)`.  `name` is the promoted type's name (a symbol).
(struct k:con  (name)    #:transparent)
;; An applied promoted kind constructor, e.g. `(List Ty)` in
;; `(s :: (List Ty))`.  `head` is the promoted type name; `args` the
;; kind arguments.
(struct k:app  (head args) #:transparent)

;; fresh-stx creates a new syntax object sharing `base`'s
;; lexical context but distinct as a struct.  Synthesizers that emit
;; multiple references to the same class method (Semigroup `mappend`,
;; Monoid `mempty`) need each reference to have a unique stx, since
;; current-method-resolutions / dict-resolutions are keyed by stx.
;; Sharing stxs across leaves caused all e:vars in a synth body to
;; collide on the most-recently-resolved class method.
(define (fresh-stx base)
  (datum->syntax base (gensym 'syn) base))

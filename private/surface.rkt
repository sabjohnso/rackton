#lang racket/base

;; Rackton — surface parser.
;;
;; Translates syntax objects from the surface language into a typed-core
;; source AST.  Used by both the `(rackton ...)` macro and the
;; `#lang rackton` reader; both feed the same elaboration pipeline.
;;
;; Lexical convention
;;   - Identifiers whose first character is a lowercase letter are
;;     "lowercase".  In type position they introduce a fresh type
;;     variable; in pattern position they introduce a fresh pattern
;;     variable; in expression position they are an ordinary
;;     reference to a value binding.
;;   - Every other identifier (uppercase letters, symbols like ->,
;;     punctuation) is "non-lowercase".  It is always a reference to
;;     an already-bound name, never a fresh binding: a type
;;     constructor / class name in a type, a data constructor as a
;;     pattern head, and a value / function / class method / data
;;     constructor in an expression.
;;
;; The AST exported below carries the originating syntax object in a
;; trailing `stx` slot so that downstream stages can produce sourcemap-
;; aware errors.

(provide (struct-out e:literal)
         (struct-out e:var)
         (struct-out e:lam)
         (struct-out e:app)
         (struct-out e:update)
         (struct-out e:let)
         (struct-out e:if)
         (struct-out e:ann)
         (struct-out e:match)
         (struct-out e:match*)
         (struct-out e:escape)
         (struct-out e:letrec)
         (struct-out clause)
         (struct-out clause*)

         (struct-out ty:var)
         (struct-out ty:con)
         (struct-out ty:app)
         (struct-out ty:forall)
         (struct-out ty:qual)
         (struct-out constraint)

         (struct-out p:wild)
         (struct-out p:var)
         (struct-out p:lit)
         (struct-out p:ctor)

         (struct-out top:def)
         (struct-out top:dec)
         (struct-out top:data)
         (struct-out data-ctor)
         (struct-out top:class)
         (struct-out top:instance)
         (struct-out method-sig)
         (struct-out method-default)
         (struct-out class-fundep)
         (struct-out class-type-fam)
         (struct-out inst-type-fam)
         (struct-out top:require)
         (struct-out top:foreign)
         (struct-out top:foreign-c)
         (struct-out top:provide)
         (struct-out top:alias)
         (struct-out top:struct-fields)
         (struct-out top:effect)
         (struct-out effect-op)
         (struct-out e:handle)
         (struct-out handle-clause)
         (struct-out handle-return)
         (struct-out k:star)
         (struct-out k:arr)
         parse-kind-stx

         parse-expr
         parse-type
         parse-pattern
         parse-top
         parse-toplevel-list

         lowercase-id?)

(require syntax/parse
         racket/match
         racket/list
         racket/set)

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
(struct e:let     (bindings body stx) #:transparent)
(struct e:if      (test then else stx) #:transparent)
(struct e:ann     (expr type stx) #:transparent)
;; `irrefutable?` flag: when #t the match was synthesized by a
;; destructuring form like `match-let` and is allowed to be
;; non-exhaustive — the user is asserting the pattern fits.  When #f
;; (the default for user-written `match`), exhaustiveness is checked.
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
(struct ty:app    (head args stx) #:transparent)
(struct ty:forall (vars body stx) #:transparent)
(struct ty:qual   (constraints body stx) #:transparent)
;; A constraint is `(C arg ...)` in surface syntax — class name + type args.
(struct constraint (class args stx) #:transparent)

(struct p:wild    (stx) #:transparent)
(struct p:var     (name stx) #:transparent)
(struct p:lit     (value stx) #:transparent)
(struct p:ctor    (name args stx) #:transparent)

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

;; Kinds at the surface level — used to annotate class parameters.
(struct k:star ()        #:transparent)
(struct k:arr  (dom cod) #:transparent)

;; ----- deriving: instance synthesis --------------------------------

;; Build a head type expression `(tname a b …)` (or just `tname` if
;; un-parameterised) for use inside a synthesised instance head.
(define (data-head-type-ast tname tparams stx)
  (cond
    [(null? tparams) (ty:con tname stx)]
    [else (ty:app (ty:con tname stx)
                  (for/list ([p (in-list tparams)]) (ty:var p stx))
                  stx)]))

(define (a-name i) (string->symbol (format "a~a" i)))
(define (b-name i) (string->symbol (format "b~a" i)))
(define (c-name i) (string->symbol (format "c~a" i)))

;; Build a fully-curried wrapper around an n-ary constructor:
;;   (lambda (c0) (lambda (c1) … (Ctor c0 c1 …)))
;; Used by Traversable deriving so the applicative composition
;; `Ctor <$> l0 <*> l1 <*> …` feeds the constructor one lifted field at
;; a time — Rackton constructors are n-ary, so `(fmap Ctor …)` /
;; `(liftA2 Ctor …)` only fully apply at arity 1 / 2 and would
;; arity-mismatch for arity ≥ 3.  Each emitted node gets a fresh stx
;; (resolution is keyed by stx).
(define (curried-ctor-lambda ctor-name arity stx)
  (define applied
    (e:app (e:var ctor-name (fresh-stx stx))
           (for/list ([i (in-range arity)]) (e:var (c-name i) (fresh-stx stx)))
           (fresh-stx stx)))
  (for/foldr ([body applied]) ([i (in-range arity)])
    (e:lam (list (c-name i)) body (fresh-stx stx))))

;; fresh-stx creates a new syntax object sharing `base`'s
;; lexical context but distinct as a struct.  Synthesizers that emit
;; multiple references to the same class method (Semigroup `<>`,
;; Monoid `mempty`) need each reference to have a unique stx, since
;; current-method-resolutions / dict-resolutions are keyed by stx.
;; Sharing stxs across leaves caused all e:vars in a synth body to
;; collide on the most-recently-resolved class method.
(define (fresh-stx base)
  (datum->syntax base (gensym 'syn) base))

(define (ctor-x-pattern name arity stx)
  (p:ctor name
          (for/list ([i (in-range arity)]) (p:var (a-name i) stx))
          stx))

(define (ctor-y-pattern name arity stx)
  (p:ctor name
          (for/list ([i (in-range arity)]) (p:var (b-name i) stx))
          stx))

;; `(== a0 b0)` then `(== a1 b1)` … chained as nested ifs.
;; Every reference gets its OWN syntax object: method/dict resolution is
;; keyed by stx, so a `==` reference sharing a stx with a value
;; reference (e.g. the match subject) would make the resolved dict
;; overwrite that value at codegen — see `fresh-stx`.
(define (chained-eq arity stx)
  (cond
    [(zero? arity) (e:literal #t (fresh-stx stx))]
    [else
     (foldr
      (lambda (i acc)
        (e:if (e:app (e:var '== (fresh-stx stx))
                     (list (e:var (a-name i) (fresh-stx stx))
                           (e:var (b-name i) (fresh-stx stx)))
                     (fresh-stx stx))
              acc
              (e:literal #f (fresh-stx stx))
              (fresh-stx stx)))
      (e:literal #t (fresh-stx stx))
      (build-list arity values))]))

(define (synthesize-eq-instance tname tparams ctors stx)
  (define head-ty (data-head-type-ast tname tparams stx))
  (define ctx (for/list ([p (in-list tparams)])
                (constraint 'Eq (list (ty:var p stx)) stx)))
  (define head (constraint 'Eq (list head-ty) stx))
  ;; Outer match on x; for each ctor, inner match on y.
  (define eq-body
    (e:match
     (e:var 'x (fresh-stx stx))
     (for/list ([c (in-list ctors)])
       (define name (data-ctor-name c))
       (define arity (length (data-ctor-field-types c)))
       (clause (ctor-x-pattern name arity stx) #f
               (e:match (e:var 'y (fresh-stx stx))
                        (list (clause (ctor-y-pattern name arity stx) #f
                                      (chained-eq arity stx)
                                      stx)
                              (clause (p:wild stx) #f
                                      (e:literal #f (fresh-stx stx))
                                      stx))
                        #f (fresh-stx stx))
               stx))
     #f (fresh-stx stx)))
  (top:instance ctx head
                (list (top:def '== (e:lam '(x y) eq-body stx) stx))
                stx))

;; Derived Ord: an instance whose `<` does ctor-index comparison and
;; lexicographic field comparison.  Carries a (Ord a) context for each
;; type-parameter.  Other Ord methods (>, <=, >=) come from defaults.
(define (synthesize-ord-instance tname tparams ctors stx)
  (define head-ty (data-head-type-ast tname tparams stx))
  (define ctx (for/list ([p (in-list tparams)])
                (constraint 'Ord (list (ty:var p stx)) stx)))
  (define head (constraint 'Ord (list head-ty) stx))
  (define lt-body
    (e:match
     (e:var 'x (fresh-stx stx))
     (for/list ([c (in-list ctors)] [i (in-naturals)])
       (clause (ctor-x-pattern (data-ctor-name c)
                               (length (data-ctor-field-types c))
                               stx) #f
               (synthesize-ord-inner-match ctors c i stx)
               stx))
     #f (fresh-stx stx)))
  (top:instance ctx head
                (list (top:def '< (e:lam '(x y) lt-body stx) stx))
                stx))

(define (synthesize-ord-inner-match ctors current-ctor current-idx stx)
  (define current-name (data-ctor-name current-ctor))
  (define current-arity (length (data-ctor-field-types current-ctor)))
  (define clauses
    (append
     ;; Earlier ctors come *before* the current one — y < x, so x < y is #f.
     (for/list ([c (in-list ctors)] [j (in-naturals)] #:when (< j current-idx))
       (define ar (length (data-ctor-field-types c)))
       (clause (p:ctor (data-ctor-name c)
                       (for/list ([_ (in-range ar)]) (p:wild stx))
                       stx) #f
               (e:literal #f stx)
               stx))
     ;; Same ctor — recurse lexicographically on fields.
     (list (clause (ctor-y-pattern current-name current-arity stx) #f
                   (chained-lex-less current-arity stx)
                   stx))
     ;; Later ctors caught by wildcard — x < y is #t.
     (list (clause (p:wild stx) #f (e:literal #t (fresh-stx stx)) stx))))
  (e:match (e:var 'y (fresh-stx stx)) clauses #f (fresh-stx stx)))

(define (chained-lex-less arity stx)
  (cond
    [(zero? arity) (e:literal #f (fresh-stx stx))]
    [else
     (foldr
      (lambda (i acc)
        ;; (if (< ai bi) #t (if (== ai bi) <recurse> #f))
        (e:if (e:app (e:var '< (fresh-stx stx))
                     (list (e:var (a-name i) (fresh-stx stx))
                           (e:var (b-name i) (fresh-stx stx)))
                     (fresh-stx stx))
              (e:literal #t (fresh-stx stx))
              (e:if (e:app (e:var '== (fresh-stx stx))
                           (list (e:var (a-name i) (fresh-stx stx))
                                 (e:var (b-name i) (fresh-stx stx)))
                           (fresh-stx stx))
                    acc
                    (e:literal #f (fresh-stx stx))
                    (fresh-stx stx))
              (fresh-stx stx)))
      (e:literal #f (fresh-stx stx))
      (build-list arity values))]))

;; Derived Functor: synthesize `fmap` for an ADT whose LAST type
;; parameter is the one being mapped over.  For each field of each
;; constructor:
;;   - if the field's type is exactly the functor parameter, apply f;
;;   - if the field's type is a recursive use of the same data type,
;;     recurse via `fmap`;
;;   - otherwise pass the field through unchanged.
(define (synthesize-functor-instance tname tparams ctors stx)
  (define fparam (car (reverse tparams)))     ; last tparam
  (define other-tparams (reverse (cdr (reverse tparams))))
  (define head-ty
    (cond
      [(null? other-tparams) (ty:con tname stx)]
      [else (ty:app (ty:con tname stx)
                    (for/list ([p (in-list other-tparams)]) (ty:var p stx))
                    stx)]))
  (define head (constraint 'Functor (list head-ty) stx))
  (define fmap-body
    (e:match
     (e:var 'x (fresh-stx stx))
     (for/list ([c (in-list ctors)])
       (define name (data-ctor-name c))
       (define field-types (data-ctor-field-types c))
       (define arity (length field-types))
       (clause (ctor-x-pattern name arity stx) #f
               (synthesize-functor-rebuild name field-types fparam tname stx)
               stx))
     #f (fresh-stx stx)))
  (top:instance '() head
                (list (top:def 'fmap
                               (e:lam '(f x) fmap-body stx)
                               stx))
                stx))

(define (synthesize-functor-rebuild ctor-name field-types fparam tname stx)
  (cond
    [(null? field-types)
     ;; Nullary constructors are values — emit the bare reference.
     (e:var ctor-name (fresh-stx stx))]
    [else
     (define transformed
       (for/list ([ft (in-list field-types)] [i (in-naturals)])
         (transform-functor-field ft (a-name i) fparam tname stx)))
     (e:app (e:var ctor-name (fresh-stx stx)) transformed (fresh-stx stx))]))

(define (transform-functor-field ft arg-name fparam tname stx)
  (define arg-var (e:var arg-name (fresh-stx stx)))
  (match ft
    ;; field is exactly the functor parameter: apply `f`
    [(ty:var n _) #:when (eq? n fparam)
     (e:app (e:var 'f (fresh-stx stx)) (list arg-var) (fresh-stx stx))]
    ;; field is a recursive use of the same data type — recurse via fmap
    [(ty:app (ty:con t _) _ _) #:when (eq? t tname)
     (e:app (e:var 'fmap (fresh-stx stx)) (list (e:var 'f (fresh-stx stx)) arg-var) (fresh-stx stx))]
    [(ty:con t _) #:when (eq? t tname)
     (e:app (e:var 'fmap (fresh-stx stx)) (list (e:var 'f (fresh-stx stx)) arg-var) (fresh-stx stx))]
    ;; otherwise leave the field unchanged
    [_ arg-var]))

;; Derived Foldable: synthesize `foldr` for an ADT whose LAST type
;; parameter is the fold element.  For each ctor's fields, walk
;; right-to-left and combine with `f`:
;;   - field is exactly the foldable param → call `(f field acc)`
;;   - field is a recursive use of the data type → recurse via foldr
;;   - other fields are skipped (no `a` inside)
;; No qualifying context on the head — Foldable's `foldr` doesn't
;; constrain the fold element (unlike Eq/Show/Ord).
(define (synthesize-foldable-instance tname tparams ctors stx)
  (define fparam (last tparams))
  (define other-tparams (drop-right tparams 1))
  (define head-ty
    (cond
      [(null? other-tparams) (ty:con tname stx)]
      [else (ty:app (ty:con tname stx)
                    (for/list ([p (in-list other-tparams)]) (ty:var p stx))
                    stx)]))
  (define head (constraint 'Foldable (list head-ty) stx))
  (define foldr-body
    (e:match
     (e:var 'x (fresh-stx stx))
     (for/list ([c (in-list ctors)])
       (define name (data-ctor-name c))
       (define field-types (data-ctor-field-types c))
       (define arity (length field-types))
       (clause (ctor-x-pattern name arity stx) #f
               (synthesize-foldable-combine field-types fparam tname stx)
               stx))
     #f (fresh-stx stx)))
  (top:instance '() head
                (list (top:def 'foldr
                               (e:lam '(f z x) foldr-body stx)
                               stx))
                stx))

;; Build right-to-left combine of `f` over the ctor's fields, starting
;; from `z`.  Returns the body expression (`z` if no fields contribute).
(define (synthesize-foldable-combine field-types fparam tname stx)
  (for/foldr ([acc (e:var 'z (fresh-stx stx))])
             ([ft (in-list field-types)]
              [i  (in-naturals)])
    (define arg (e:var (a-name i) (fresh-stx stx)))
    (cond
      ;; field is exactly the foldable parameter — combine directly.
      [(and (ty:var? ft) (eq? (ty:var-name ft) fparam))
       (e:app (e:var 'f (fresh-stx stx)) (list arg acc) (fresh-stx stx))]
      ;; recursive use of the same data type — recurse via foldr.
      [(or (and (ty:app? ft)
                (ty:con? (ty:app-head ft))
                (eq? (ty:con-name (ty:app-head ft)) tname))
           (and (ty:con? ft)
                (eq? (ty:con-name ft) tname)))
       (e:app (e:var 'foldr (fresh-stx stx))
              (list (e:var 'f (fresh-stx stx)) acc arg)
              (fresh-stx stx))]
      ;; otherwise the field carries no `a`, skip.
      [else acc])))

;; ----- Traversable deriving --------------------------
;; For an ADT `(T a … b)` whose LAST tparam `b` is the traversed
;; element, emit `(define (traverse f x) (match x ctors…))` where each
;; ctor clause rebuilds the ctor via Applicative composition over
;; lifted fields:
;;   - field type is `b`         → `(f field)`
;;   - field is a recursive `(T …)` → `(traverse f field)`
;;   - otherwise                  → `(pure field)`
;; Composition shape by arity:
;;   0 fields → `(pure Ctor)`
;;   1 field  → `(fmap Ctor lift0)`
;;   2 fields → `(liftA2 Ctor lift0 lift1)`
;;   N≥3      → `(fapply (fapply … (liftA2 Ctor lift0 lift1) … lift_{N-2}) lift_{N-1})`
(define (synthesize-traversable-instance tname tparams ctors stx)
  (when (null? tparams)
    (raise-syntax-error 'data
      "cannot derive Traversable for a type with no type parameters"
      stx))
  (define fparam (last tparams))
  (define other-tparams (drop-right tparams 1))
  (define head-ty
    (cond
      [(null? other-tparams) (ty:con tname stx)]
      [else (ty:app (ty:con tname stx)
                    (for/list ([p (in-list other-tparams)]) (ty:var p stx))
                    stx)]))
  (define head (constraint 'Traversable (list head-ty) stx))
  (define traverse-body
    (e:match
     (e:var 'x (fresh-stx stx))
     (for/list ([c (in-list ctors)])
       (define name (data-ctor-name c))
       (define field-types (data-ctor-field-types c))
       (define arity (length field-types))
       (clause (ctor-x-pattern name arity stx) #f
               (synthesize-traverse-rebuild name field-types fparam tname stx)
               stx))
     #f (fresh-stx stx)))
  (top:instance '() head
                (list (top:def 'traverse
                               (e:lam '(f x) traverse-body stx)
                               stx))
                stx))

(define (synthesize-traverse-rebuild ctor-name field-types fparam tname stx)
  (define lifted-fields
    (for/list ([ft (in-list field-types)] [i (in-naturals)])
      (transform-traverse-field ft (a-name i) fparam tname stx)))
  (define arity (length field-types))
  (cond
    [(zero? arity)
     (e:app (e:var 'pure (fresh-stx stx)) (list (e:var ctor-name (fresh-stx stx))) (fresh-stx stx))]
    [(= arity 1)
     (e:app (e:var 'fmap (fresh-stx stx))
            (list (e:var ctor-name (fresh-stx stx)) (car lifted-fields))
            (fresh-stx stx))]
    [else
     ;; arity ≥ 3: Ctor <$> l0 <*> l1 <*> … <*> l_{N-1}, with Ctor
     ;; curried so each fmap/fapply supplies exactly one field (a bare
     ;; n-ary Ctor would arity-mismatch — liftA2 only reaches arity 2).
     (for/fold ([acc (e:app (e:var 'fmap (fresh-stx stx))
                            (list (curried-ctor-lambda ctor-name arity stx)
                                  (car lifted-fields))
                            (fresh-stx stx))])
               ([lf (in-list (cdr lifted-fields))])
       (e:app (e:var 'fapply (fresh-stx stx)) (list acc lf) (fresh-stx stx)))]))

(define (transform-traverse-field ft arg-name fparam tname stx)
  (define arg (e:var arg-name (fresh-stx stx)))
  (cond
    [(and (ty:var? ft) (eq? (ty:var-name ft) fparam))
     (e:app (e:var 'f (fresh-stx stx)) (list arg) (fresh-stx stx))]
    [(or (and (ty:app? ft)
              (ty:con? (ty:app-head ft))
              (eq? (ty:con-name (ty:app-head ft)) tname))
         (and (ty:con? ft)
              (eq? (ty:con-name ft) tname)))
     (e:app (e:var 'traverse (fresh-stx stx)) (list (e:var 'f (fresh-stx stx)) arg) (fresh-stx stx))]
    [else
     (e:app (e:var 'pure (fresh-stx stx)) (list arg) (fresh-stx stx))]))

;; ----- Bifunctor deriving ----------------------------
;; For an ADT with at least TWO tparams.  The penultimate is the
;; first bifunctor param, the last is the second.  For each ctor's
;; fields:
;;   - field type is penultimate-tparam → `(f field)`
;;   - field type is last-tparam        → `(g field)`
;;   - field is recursive `(T …)` (same outer ctor) → `(bimap f g field)`
;;   - otherwise                         → field
;; No qual context.
(define (synthesize-bifunctor-instance tname tparams ctors stx)
  (when (< (length tparams) 2)
    (raise-syntax-error 'data
      "cannot derive Bifunctor for a type with fewer than two type parameters"
      stx))
  (define f1 (list-ref tparams (- (length tparams) 2)))
  (define f2 (last tparams))
  (define other-tparams (drop-right tparams 2))
  (define head-ty
    (cond
      [(null? other-tparams) (ty:con tname stx)]
      [else (ty:app (ty:con tname stx)
                    (for/list ([p (in-list other-tparams)]) (ty:var p stx))
                    stx)]))
  (define head (constraint 'Bifunctor (list head-ty) stx))
  (define bimap-body
    (e:match
     (e:var 'x (fresh-stx stx))
     (for/list ([c (in-list ctors)])
       (define name (data-ctor-name c))
       (define field-types (data-ctor-field-types c))
       (define arity (length field-types))
       (clause (ctor-x-pattern name arity stx) #f
               (synthesize-bifunctor-rebuild name field-types
                                             f1 f2 tname stx)
               stx))
     #f (fresh-stx stx)))
  (top:instance '() head
                (list (top:def 'bimap
                               (e:lam '(f g x) bimap-body stx)
                               stx))
                stx))

(define (synthesize-bifunctor-rebuild ctor-name field-types f1 f2 tname stx)
  (cond
    [(null? field-types) (e:var ctor-name (fresh-stx stx))]
    [else
     (define transformed
       (for/list ([ft (in-list field-types)] [i (in-naturals)])
         (transform-bifunctor-field ft (a-name i) f1 f2 tname stx)))
     (e:app (e:var ctor-name (fresh-stx stx)) transformed (fresh-stx stx))]))

(define (transform-bifunctor-field ft arg-name f1 f2 tname stx)
  (define arg (e:var arg-name (fresh-stx stx)))
  (cond
    [(and (ty:var? ft) (eq? (ty:var-name ft) f1))
     (e:app (e:var 'f (fresh-stx stx)) (list arg) (fresh-stx stx))]
    [(and (ty:var? ft) (eq? (ty:var-name ft) f2))
     (e:app (e:var 'g (fresh-stx stx)) (list arg) (fresh-stx stx))]
    [(or (and (ty:app? ft)
              (ty:con? (ty:app-head ft))
              (eq? (ty:con-name (ty:app-head ft)) tname))
         (and (ty:con? ft)
              (eq? (ty:con-name ft) tname)))
     (e:app (e:var 'bimap (fresh-stx stx))
            (list (e:var 'f (fresh-stx stx)) (e:var 'g (fresh-stx stx)) arg)
            (fresh-stx stx))]
    [else arg]))

;; ----- Semigroup deriving ----------------------------
;; Single-ctor ADTs only.  Combine fields pairwise via `<>`.  Qual
;; context carries `(Semigroup ft)` for each unique field type.
;; Concrete field types (e.g. `String`) get discharged immediately
;; by reduce-context; tvar field types stay in the qual.
(define (synthesize-semigroup-instance tname tparams ctors stx)
  (unless (= (length ctors) 1)
    (raise-syntax-error 'data
      "cannot derive Semigroup for a type with multiple constructors — only single-ctor types can be combined pointwise"
      stx))
  (define ctor (car ctors))
  (define ctor-name (data-ctor-name ctor))
  (define arity (length (data-ctor-field-types ctor)))
  (define head-ty (data-head-type-ast tname tparams stx))
  (define ctx (for/list ([ft (in-list (data-ctor-field-types ctor))])
                (constraint 'Semigroup (list ft) stx)))
  (define head (constraint 'Semigroup (list head-ty) stx))
  (define combined
    (cond
      [(zero? arity) (e:var ctor-name (fresh-stx stx))]
      [else
       (e:app (e:var ctor-name (fresh-stx stx))
              (for/list ([i (in-range arity)])
                (e:app (e:var '<> (fresh-stx stx))
                       (list (e:var (a-name i) (fresh-stx stx))
                             (e:var (b-name i) (fresh-stx stx)))
                       (fresh-stx stx)))
              (fresh-stx stx))]))
  (define body
    (e:match
     (e:var 'x (fresh-stx stx))
     (list (clause (ctor-x-pattern ctor-name arity stx) #f
                   (e:match
                    (e:var 'y (fresh-stx stx))
                    (list (clause (ctor-y-pattern ctor-name arity stx) #f
                                  combined stx))
                    #f stx)
                   stx))
     #f stx))
  (top:instance ctx head
                (list (top:def '<> (e:lam '(x y) body stx) stx))
                stx))

;; ----- Monoid deriving -------------------------------
;; Single-ctor ADTs only.  `mempty` is the ctor applied to per-field
;; `mempty`s.  Qual context carries `(Monoid a)` per tparam.
(define (synthesize-monoid-instance tname tparams ctors stx)
  (unless (= (length ctors) 1)
    (raise-syntax-error 'data
      "cannot derive Monoid for a type with multiple constructors — only single-ctor types have a canonical empty"
      stx))
  (define ctor (car ctors))
  (define ctor-name (data-ctor-name ctor))
  (define arity (length (data-ctor-field-types ctor)))
  (define head-ty (data-head-type-ast tname tparams stx))
  (define ctx (for/list ([ft (in-list (data-ctor-field-types ctor))])
                (constraint 'Monoid (list ft) stx)))
  (define head (constraint 'Monoid (list head-ty) stx))
  (define empty-body
    (cond
      [(zero? arity) (e:var ctor-name (fresh-stx stx))]
      [else
       (e:app (e:var ctor-name (fresh-stx stx))
              (for/list ([i (in-range arity)])
                (e:var 'mempty (fresh-stx stx)))
              (fresh-stx stx))]))
  (top:instance ctx head
                (list (top:def 'mempty empty-body stx))
                stx))

;; ----- Prism deriving --------------------------------
;; For each constructor of a multi-ctor ADT, emit a top-level prism
;; definition.  Arity-0 ctors get a `(Prism T Unit)`; arity-1 ctors
;; get a `(Prism T fieldT)`.  Arity ≥ 2 ctors are silently skipped
;; (a tuple-focused prism would need a product encoding we don't
;; offer in this phase).
(define (synthesize-prism-defs tname tparams ctors ctx-stx)
  ;; One prism per constructor.  The focus type is the constructor's
  ;; payload: `Unit` for a nullary ctor, the field's type for a
  ;; single-field ctor, and the right-nested product of the fields for
  ;; a multi-field ctor — `(Pair a b)` for `(C a b)`, `(Pair a (Pair b
  ;; c))` for `(C a b c)`.
  (apply append
         (for/list ([c (in-list ctors)])
           (define name (data-ctor-name c))
           (define arity (length (data-ctor-field-types c)))
           (cond
             [(= arity 0) (list (synth-prism-0-arg tname name ctors ctx-stx))]
             [(= arity 1) (list (synth-prism-1-arg tname name ctors ctx-stx))]
             [else (list (synth-prism-n-arg tname name arity ctors ctx-stx))]))))

(define (synth-prism-0-arg tname ctor-name all-ctors ctx-stx)
  (define lens-name (string->symbol (format "~a-~a-prism" tname ctor-name)))
  (define extractor
    (e:lam '(s)
           (e:match
            (e:var 's (fresh-stx ctx-stx))
            (list (clause (p:ctor ctor-name '() ctx-stx) #f
                          (e:app (e:var 'Some (fresh-stx ctx-stx))
                                 (list (e:var 'MkUnit (fresh-stx ctx-stx)))
                                 (fresh-stx ctx-stx))
                          ctx-stx)
                  ;; Always emit a wildcard fallback even when the
                  ;; type has just one ctor — keeps the match
                  ;; well-formed without consulting exhaustiveness.
                  (clause (p:wild ctx-stx) #f
                          (e:var 'None (fresh-stx ctx-stx))
                          ctx-stx))
            #f ctx-stx)
           (fresh-stx ctx-stx)))
  (define builder
    (e:lam '(_)
           (e:var ctor-name (fresh-stx ctx-stx))
           (fresh-stx ctx-stx)))
  (top:def lens-name
           (e:app (e:var 'MkPrism (fresh-stx ctx-stx))
                  (list extractor builder)
                  (fresh-stx ctx-stx))
           ctx-stx))

(define (synth-prism-1-arg tname ctor-name all-ctors ctx-stx)
  (define lens-name (string->symbol (format "~a-~a-prism" tname ctor-name)))
  (define extractor
    (e:lam '(s)
           (e:match
            (e:var 's (fresh-stx ctx-stx))
            (list (clause (p:ctor ctor-name (list (p:var 'x ctx-stx))
                                  ctx-stx) #f
                          (e:app (e:var 'Some (fresh-stx ctx-stx))
                                 (list (e:var 'x (fresh-stx ctx-stx)))
                                 (fresh-stx ctx-stx))
                          ctx-stx)
                  (clause (p:wild ctx-stx) #f
                          (e:var 'None (fresh-stx ctx-stx))
                          ctx-stx))
            #f ctx-stx)
           (fresh-stx ctx-stx)))
  ;; Builder is just a reference to the ctor — auto-curry makes
  ;; `(Foo x)` and `Foo` interchangeable for arity-1 ctors.
  (define builder (e:var ctor-name (fresh-stx ctx-stx)))
  (top:def lens-name
           (e:app (e:var 'MkPrism (fresh-stx ctx-stx))
                  (list extractor builder)
                  (fresh-stx ctx-stx))
           ctx-stx))

;; Largest flat tuple a prism can focus.  arity 2 reuses the prelude
;; `Pair`; arity 3..MAX use `Tuple3`..`TupleMAX` from rackton/data/lens.
(define prism-max-tuple-arity 7)

;; The flat product constructor for a focus of the given arity, or #f
;; when no tuple type of that arity exists.
(define (prism-tuple-ctor arity)
  (cond
    [(= arity 2) 'MkPair]
    [(<= 3 arity prism-max-tuple-arity)
     (string->symbol (format "MkTuple~a" arity))]
    [else #f]))

;; Prism for a multi-field (arity ≥ 2) ctor.  The focus is the FLAT
;; tuple of the fields (`Pair` at arity 2, `TupleK` above):
;;   preview: (lambda (s) (match s [(C x0 … xn) (Some (Tup x0 … xn))] [_ None]))
;;   review:  (lambda (p) (match p [(Tup x0 … xn) (C x0 … xn)]))
;; review's match is irrefutable — a tuple value always matches its one
;; shape — so exhaustiveness is not consulted.
(define (synth-prism-n-arg tname ctor-name arity all-ctors ctx-stx)
  (define tup-ctor (prism-tuple-ctor arity))
  (unless tup-ctor
    (raise-syntax-error 'data
      (format "cannot derive Prism for ~a: constructor ~a has ~a fields — a multi-field prism focuses a flat tuple, which is defined only up to ~a fields"
              tname ctor-name arity prism-max-tuple-arity)
      ctx-stx))
  (define lens-name (string->symbol (format "~a-~a-prism" tname ctor-name)))
  (define vars (for/list ([i (in-range arity)]) (a-name i)))
  (define extractor
    (e:lam '(s)
           (e:match
            (e:var 's (fresh-stx ctx-stx))
            (list (clause (p:ctor ctor-name
                                  (for/list ([v (in-list vars)]) (p:var v ctx-stx))
                                  ctx-stx) #f
                          (e:app (e:var 'Some (fresh-stx ctx-stx))
                                 (list (e:app (e:var tup-ctor (fresh-stx ctx-stx))
                                              (for/list ([v (in-list vars)]) (e:var v (fresh-stx ctx-stx)))
                                              (fresh-stx ctx-stx)))
                                 (fresh-stx ctx-stx))
                          ctx-stx)
                  (clause (p:wild ctx-stx) #f
                          (e:var 'None (fresh-stx ctx-stx))
                          ctx-stx))
            #f ctx-stx)
           (fresh-stx ctx-stx)))
  (define builder
    (e:lam '(p)
           (e:match
            (e:var 'p (fresh-stx ctx-stx))
            (list (clause (p:ctor tup-ctor
                                  (for/list ([v (in-list vars)]) (p:var v ctx-stx))
                                  ctx-stx) #f
                          (e:app (e:var ctor-name (fresh-stx ctx-stx))
                                 (for/list ([v (in-list vars)]) (e:var v (fresh-stx ctx-stx)))
                                 (fresh-stx ctx-stx))
                          ctx-stx))
            #t ctx-stx)
           (fresh-stx ctx-stx)))
  (top:def lens-name
           (e:app (e:var 'MkPrism (fresh-stx ctx-stx))
                  (list extractor builder)
                  (fresh-stx ctx-stx))
           ctx-stx))

(define (synthesize-show-instance tname tparams ctors stx)
  (define head-ty (data-head-type-ast tname tparams stx))
  (define ctx (for/list ([p (in-list tparams)])
                (constraint 'Show (list (ty:var p stx)) stx)))
  (define head (constraint 'Show (list head-ty) stx))
  (define show-body
    (e:match
     (e:var 'x stx)
     (for/list ([c (in-list ctors)])
       (define name (data-ctor-name c))
       (define arity (length (data-ctor-field-types c)))
       (clause (ctor-x-pattern name arity stx) #f
               (cond
                 [(zero? arity)
                  (e:literal (symbol->string name) stx)]
                 [else
                  ;; Splice raw Racket calling the variadic $show-concat
                  ;; provided by prelude-runtime; the Rackton-typed
                  ;; string-append is binary and would arity-mismatch.
                  (define arg-shows
                    (apply append
                           (for/list ([i (in-range arity)])
                             (list " " `(show ,(a-name i))))))
                  (define body-datum
                    `($show-concat ,(format "(~a" name)
                                   ,@arg-shows
                                   ")"))
                  (e:escape (ty:con 'String stx)
                            (for/list ([i (in-range arity)]) (a-name i))
                            (datum->syntax stx body-datum stx)
                            stx)])
               stx))
     #f stx))
  (top:instance ctx head
                (list (top:def 'show (e:lam '(x) show-body stx) stx))
                stx))

;; ----- lexical classification ---------------------------------------

(define (lowercase-id? sym)
  (and (symbol? sym)
       (let ([s (symbol->string sym)])
         (and (positive? (string-length s))
              (let ([c (string-ref s 0)])
                (and (char-alphabetic? c)
                     (char-lower-case? c)))))))

(define (wildcard-symbol? sym) (eq? sym '_))

;; ----- expressions --------------------------------------------------

;; Build the Cons/Nil list AST from a list of already-parsed element
;; expressions.  Used by the variadic `describe`/`context` desugaring.
(define (build-list-ast elems stx)
  (cond
    [(null? elems) (e:var 'Nil stx)]
    [else (e:app (e:var 'Cons stx)
                 (list (car elems) (build-list-ast (cdr elems) stx))
                 stx)]))

(define (parse-expr stx)
  (syntax-parse stx
    #:datum-literals (lambda λ let let& let% let+ letrec match-let where if cond else ann match racket do <- update handle return describe context list ->)
    [n:number  (e:literal (syntax->datum #'n) stx)]
    [b:boolean (e:literal (syntax->datum #'b) stx)]
    [s:string  (e:literal (syntax->datum #'s) stx)]
    [c:char    (e:literal (syntax->datum #'c) stx)]
    [by:bytes  (e:literal (syntax->datum #'by) stx)]

    [(lambda (p:id ...) body)
     (e:lam (map syntax->datum (syntax->list #'(p ...)))
            (parse-expr #'body)
            stx)]
    [(λ (p:id ...) body)
     (e:lam (map syntax->datum (syntax->list #'(p ...)))
            (parse-expr #'body)
            stx)]

    ;; Named (loop) let — Scheme-style: `loop` is a recursive procedure
    ;; bound in the body, seeded by the initial RHS.  Matched before the
    ;; plain `let` because the head after `let` is an id here, a binding
    ;; group there, so the two never overlap.
    [(let loop:id ([x:id rhs] ...+) body)
     (build-named-let (syntax->datum #'loop)
                      (parse-binding-pairs #'(x ...) #'(rhs ...))
                      (parse-expr #'body)
                      stx)]

    [(let ([x:id rhs] ...) body)
     (e:let (for/list ([id (in-list (syntax->list #'(x ...)))]
                       [r  (in-list (syntax->list #'(rhs ...)))])
              (cons (syntax->datum id) (parse-expr r)))
            (parse-expr #'body)
            stx)]

    ;; let& — sequential monad bind (deps allowed); nested flatmap, same
    ;; family as `do`.  Body is the final monadic expression.
    [(let& ([x:id rhs] ...+) body)
     (build-sequential-let (parse-binding-pairs #'(x ...) #'(rhs ...))
                           (parse-expr #'body)
                           stx)]

    ;; let% (named) — monadic loop: loop params are the monadic values,
    ;; combined per iteration via the gathered-product engine.
    [(let% loop:id ([x:id rhs] ...+) body)
     (build-named-monadic-let (syntax->datum #'loop)
                              (parse-binding-pairs #'(x ...) #'(rhs ...))
                              (parse-expr #'body)
                              stx)]

    ;; let% — parallel/independent monad bind: gather via `product`,
    ;; then `flatmap` into a monadic body.
    [(let% ([x:id rhs] ...+) body)
     (build-gathered-let 'flatmap
                         (parse-binding-pairs #'(x ...) #'(rhs ...))
                         (parse-expr #'body)
                         stx)]

    ;; let+ — applicative bind: gather via `product`, then `fmap`.  The
    ;; body is a PURE expression; the result is wrapped by the functor.
    [(let+ ([x:id rhs] ...+) body)
     (build-gathered-let 'fmap
                         (parse-binding-pairs #'(x ...) #'(rhs ...))
                         (parse-expr #'body)
                         stx)]

    [(letrec ([x:id rhs] ...) body)
     (e:letrec (for/list ([id (in-list (syntax->list #'(x ...)))]
                          [r  (in-list (syntax->list #'(rhs ...)))])
                 (cons (syntax->datum id) (parse-expr r)))
               (parse-expr #'body)
               stx)]

    ;; (match-let ([pat expr] ...+) body) — desugars to nested
    ;; singleton matches.  Each binding pattern destructures its rhs
    ;; for the remainder of the chain.
    [(match-let ([pat rhs] ...+) body)
     (define pat-list (syntax->list #'(pat ...)))
     (define rhs-list (syntax->list #'(rhs ...)))
     (let loop ([pats pat-list] [rhss rhs-list])
       (cond
         [(null? pats) (parse-expr #'body)]
         [else
          (e:match (parse-expr (car rhss))
                   (list (clause (parse-pattern (car pats))
                                 #f
                                 (loop (cdr pats) (cdr rhss))
                                 (car pats)))
                   #t stx)]))]

    ;; (list e ...) — list-literal sugar.  Desugars to a Cons/Nil
    ;; chain; (list) is Nil.  Purely a parser rewrite, so the result is
    ;; an ordinary (List a).
    [(list elem ...)
     (build-list-ast
      (map parse-expr (syntax->list #'(elem ...))) stx)]

    ;; (describe NAME child ...) / (context NAME child ...) — the test
    ;; framework's grouping forms, made variadic so children need no
    ;; explicit list wrapper.  Desugars to a call to the library
    ;; function `group-of` (resolved in the user's env, like `do`
    ;; resolves `flatmap`); the children are gathered into a List via
    ;; Cons/Nil.  Both forms are aliases.
    [(describe name child ...)
     (e:app (e:var 'group-of stx)
            (list (parse-expr #'name)
                  (build-list-ast
                   (map parse-expr (syntax->list #'(child ...))) stx))
            stx)]
    [(context name child ...)
     (e:app (e:var 'group-of stx)
            (list (parse-expr #'name)
                  (build-list-ast
                   (map parse-expr (syntax->list #'(child ...))) stx))
            stx)]

    ;; (where ([n expr] ...) body) — sequential local bindings.
    ;; Each binding sees the ones before it.  Equivalent to a
    ;; nested chain of singleton lets.
    [(where ([x:id rhs] ...+) body)
     (define ids (syntax->list #'(x ...)))
     (define rhss (syntax->list #'(rhs ...)))
     (let loop ([ids ids] [rhss rhss])
       (cond
         [(null? ids) (parse-expr #'body)]
         [else
          (e:let (list (cons (syntax->datum (car ids))
                             (parse-expr (car rhss))))
                 (loop (cdr ids) (cdr rhss))
                 stx)]))]

    [(if c t e)
     (e:if (parse-expr #'c) (parse-expr #'t) (parse-expr #'e) stx)]

    ;; (cond [c1 b1] [c2 b2] ... [else bN])
    ;; Desugars to nested ifs.  The final clause MUST be `[else b]`
    ;; — without it there's no value to fall through to and the
    ;; resulting expression would be ill-typed.
    [(cond clause ...+)
     (parse-cond-clauses (syntax->list #'(clause ...)) stx)]

    [(ann e t)
     (e:ann (parse-expr #'e) (parse-type #'t) stx)]

    [(match scrut cl ...+)
     (e:match (parse-expr #'scrut)
              (for/list ([c-stx (in-list (syntax->list #'(cl ...)))])
                (parse-match-clause c-stx))
              #f stx)]

    ;; (racket τ (var ...) body ...+) — host-language escape.
    ;; Multiple body forms are wrapped in `begin` so users can write
    ;; sequences and inner `define`s naturally.
    [(racket τ (v:id ...) body ...+)
     (define body-list (syntax->list #'(body ...)))
     (define body-stx
       (cond
         [(= (length body-list) 1) (car body-list)]
         [else (datum->syntax stx (cons 'begin body-list) stx)]))
     (e:escape (parse-type #'τ)
               (map syntax->datum (syntax->list #'(v ...)))
               body-stx
               stx)]

    ;; (do [x <- m1] [y <- m2] ... body)  desugars to nested flatmap
    ;; calls.  A statement is `[var <- expr]`; each binds the un-wrapped
    ;; value for the rest of the chain.  The trailing `body` is the
    ;; final computation.
    [(do stmt ...+ body)
     (parse-do (syntax->list #'(stmt ...)) #'body stx)]

    ;; (handle EXPR [op (args ...) k -> body] ... [return v -> body])
    ;; EXPR is run in a context where the named ops
    ;; are dispatched to the listed clauses; if EXPR finishes
    ;; normally, its value flows through the return clause.
    [(handle expr cl ...+)
     (define cls (syntax->list #'(cl ...)))
     (define-values (op-clauses ret-clause)
       (parse-handle-clauses cls stx))
     (e:handle (parse-expr #'expr) op-clauses ret-clause stx)]

    ;; (update RECORD [field val] ...) — functional record update.
    ;; Each [field val] replaces the named field of the recordrm
    ;; result; the rest are preserved.  Field names must match the
    ;; struct's declared fields, checked at inference time.
    [(update record (~and upd [_ _]) ...+)
     (e:update (parse-expr #'record)
               (for/list ([u (in-list (syntax->list #'(upd ...)))])
                 (syntax-parse u
                   [[name:id v]
                    (cons (syntax->datum #'name) (parse-expr #'v))]))
               stx)]

    [x:id  (e:var (syntax->datum #'x) stx)]

    ;; Also accept zero-arg applications `(f)`.  These
    ;; are passed an implicit MkUnit so the typing of 0-arg ops
    ;; as `(-> Unit T)` lines up — saves users from spelling out
    ;; the dummy at every effect call site.
    [(head)
     (e:app (parse-expr #'head)
            (list (e:var 'MkUnit stx))
            stx)]
    [(head arg ...+)
     (e:app (parse-expr #'head)
            (for/list ([a (in-list (syntax->list #'(arg ...)))])
              (parse-expr a))
            stx)]))

;; Parse a sequence of cond clauses, desugaring to nested if forms.
;; The final clause is required to be `[else body]`.
(define (parse-cond-clauses clauses stx)
  (syntax-parse clauses
    #:datum-literals (else)
    [([else body]) (parse-expr #'body)]
    [([test body] more ...)
     (e:if (parse-expr #'test)
           (parse-expr #'body)
           (parse-cond-clauses (syntax->list #'(more ...)) stx)
           stx)]
    [()
     (raise-syntax-error 'parse-cond
       "cond must end with an [else …] clause"
       stx)]))

;; Parse one `match` clause.  Two shapes are accepted:
;;   [pat body]
;;   [pat #:when guard body]
;; The guard, when present, is a Boolean-typed expression evaluated
;; after the pattern's variable bindings are in scope.  If the guard
;; fails, the next clause is tried.
(define (parse-match-clause stx)
  (syntax-parse stx
    [[pat #:when guard body]
     (clause (parse-pattern #'pat)
             (parse-expr #'guard)
             (parse-expr #'body)
             stx)]
    [[pat body]
     (clause (parse-pattern #'pat)
             #f
             (parse-expr #'body)
             stx)]))

(define (parse-handle-clauses cls outer-stx)
  ;; Split clauses into op-clauses and a single return-clause.  The
  ;; return clause must be present and must be last (or anywhere —
  ;; we pick the unique one, regardless of position).
  (define parsed
    (for/list ([c (in-list cls)])
      (syntax-parse c
        #:datum-literals (return ->)
        [[return v:id -> body]
         (handle-return (syntax->datum #'v)
                        (parse-expr #'body) c)]
        [[op:id (p:id ...) k:id -> body]
         (handle-clause (syntax->datum #'op)
                        (map syntax->datum (syntax->list #'(p ...)))
                        (syntax->datum #'k)
                        (parse-expr #'body) c)])))
  (define-values (rets ops)
    (partition handle-return? parsed))
  (unless (= (length rets) 1)
    (raise-syntax-error 'parse-handle
      "expected exactly one [return v -> body] clause" outer-stx))
  (values ops (car rets)))

;; ----- monadic / applicative let desugarings -----------------------
;;
;; Parse parallel binding-clause syntax `([x rhs] ...)` into a list of
;; `(cons name-sym rhs-ast)` pairs.  RHS are parsed independently in the
;; enclosing scope; sequential/loop scoping is supplied by the builders.
(define (parse-binding-pairs ids-stx rhss-stx)
  (for/list ([id (in-list (syntax->list ids-stx))]
             [r  (in-list (syntax->list rhss-stx))])
    (cons (syntax->datum id) (parse-expr r))))

;; Gather independent binding RHS into one applicative value via
;; right-associated `product`, with the matching nested `MkPair`
;; destructuring pattern.  A single binding has no product: the value is
;; the lone RHS and the pattern is a plain variable.
;;
;;   [(a . m1) (b . m2) (c . m3)]
;;     value   = (product m1 (product m2 m3))
;;     pattern = (MkPair a (MkPair b c))
(define (gather-product binds stx)
  (define name (car (car binds)))
  (define rhs  (cdr (car binds)))
  (cond
    [(null? (cdr binds))
     (values rhs (p:var name stx))]
    [else
     (define-values (rest-ast rest-pat) (gather-product (cdr binds) stx))
     (values (e:app (e:var 'product stx) (list rhs rest-ast) stx)
             (p:ctor 'MkPair (list (p:var name stx) rest-pat) stx))]))

;; Build `(combiner (lambda (p) (match p [pattern body])) gathered)` for
;; let% (combiner = 'flatmap) and let+ (combiner = 'fmap).  A single
;; binding takes the shortcut `(combiner (lambda (name) body) rhs)`.
(define (build-gathered-let combiner binds body-ast stx)
  (cond
    [(null? (cdr binds))
     (define name (car (car binds)))
     (define rhs  (cdr (car binds)))
     (e:app (e:var combiner stx)
            (list (e:lam (list name) body-ast stx) rhs)
            stx)]
    [else
     (define-values (gathered pat) (gather-product binds stx))
     (define p (gensym '$let))
     (e:app (e:var combiner stx)
            (list (e:lam (list p)
                         (e:match (e:var p stx)
                                  (list (clause pat #f body-ast stx))
                                  #t stx)
                         stx)
                  gathered)
            stx)]))

;; let& — sequential nested flatmap.  Each later binding's RHS sits
;; inside the earlier bindings' lambdas, so it sees them in scope.
(define (build-sequential-let binds body-ast stx)
  (cond
    [(null? binds) body-ast]
    [else
     (define name (car (car binds)))
     (define rhs  (cdr (car binds)))
     (e:app (e:var 'flatmap stx)
            (list (e:lam (list name)
                         (build-sequential-let (cdr binds) body-ast stx)
                         stx)
                  rhs)
            stx)]))

;; Named pure let — `(letrec ([loop (lambda (x ...) body)]) (loop i ...))`.
(define (build-named-let loop-name binds body-ast stx)
  (e:letrec (list (cons loop-name
                        (e:lam (map car binds) body-ast stx)))
            (e:app (e:var loop-name stx) (map cdr binds) stx)
            stx))

;; Named monadic let% — the loop's parameters are the monadic values;
;; each entry combines them via the gathered-product engine (flatmap)
;; and binds the names; the body is monadic and may recurse with fresh
;; monadic values.  The initial RHS are the seeds.
(define (build-named-monadic-let loop-name binds body-ast stx)
  (define names  (map car binds))
  (define seeds  (map cdr binds))
  (define params (map (lambda (_) (gensym '$arg)) binds))
  (define param-binds
    (for/list ([n (in-list names)] [p (in-list params)])
      (cons n (e:var p stx))))
  (define loop-body (build-gathered-let 'flatmap param-binds body-ast stx))
  (e:letrec (list (cons loop-name (e:lam params loop-body stx)))
            (e:app (e:var loop-name stx) seeds stx)
            stx))

(define (parse-do stmts body-stx stx)
  (cond
    [(null? stmts) (parse-expr body-stx)]
    [else
     (define s (car stmts))
     (syntax-parse s
       #:datum-literals (<-)
       [[v:id <- expr]
        (e:app (e:var 'flatmap stx)
               (list (e:lam (list (syntax->datum #'v))
                            (parse-do (cdr stmts) body-stx stx)
                            stx)
                     (parse-expr #'expr))
               stx)]
       [expr
        ;; Bare-expression clause: sequence `expr` for its monadic
        ;; effect, discard the result, then continue.  Desugars to the
        ;; same shape as `[_fresh <- expr]` but with a fresh
        ;; identifier so the wildcard isn't a binder.
        (define fresh (gensym '_do))
        (e:app (e:var 'flatmap stx)
               (list (e:lam (list fresh)
                            (parse-do (cdr stmts) body-stx stx)
                            stx)
                     (parse-expr #'expr))
               stx)])]))

;; ----- patterns -----------------------------------------------------

(define (parse-pattern stx)
  (syntax-parse stx
    [n:number  (p:lit (syntax->datum #'n) stx)]
    [b:boolean (p:lit (syntax->datum #'b) stx)]
    [s:string  (p:lit (syntax->datum #'s) stx)]
    [x:id
     (define name (syntax->datum #'x))
     (cond
       [(wildcard-symbol? name) (p:wild stx)]
       [(lowercase-id? name)    (p:var name stx)]
       [else                    (p:ctor name '() stx)])]
    [(ctor:id arg ...)
     #:fail-unless (not (lowercase-id? (syntax->datum #'ctor)))
     "constructor pattern head must be a non-lowercase identifier"
     (p:ctor (syntax->datum #'ctor)
             (for/list ([a (in-list (syntax->list #'(arg ...)))])
               (parse-pattern a))
             stx)]))

;; Side channel: every `(define (f params…) body)` form parsed via
;; `parse-fn-params+body` deposits its original `(params-stx body-stx)`
;; here, keyed by the def's source stx.  The post-processor in
;; `parse-toplevel-list` consults this when grouping consecutive
;; same-name function defs into a single multi-clause `e:match*`,
;; reparsing the parameter list under the multi-clause rule (bare
;; uppercase identifiers become 0-arg ctor patterns rather than plain
;; param names).  Outside `parse-toplevel-list` the parameter is #f
;; and recording is skipped — `parse-top` then behaves exactly as
;; before for the surface tests.
(define current-fn-clauses-record (make-parameter #f))

;; Parse a function's parameter list together with its body.  Each
;; parameter is either a bare identifier (binds a plain parameter
;; — preserves the pre-feature behaviour for every existing
;; identifier shape, including `_x` and uppercase names) or a
;; parenthesized pattern like `(Point x y)` or `(Cons x xs)`.
;; Pattern parameters desugar to a synthetic identifier plus an
;; irrefutable `match` wrapping the body.  Returns
;; `(values param-names body-ast)`.
;;
;; Source order is preserved in the wrap: matching against the
;; first parameter is outermost in the resulting expression, so a
;; refutable pattern in the first parameter raises its mismatch
;; before later parameters are even examined.
;;
;; `irrefutable?` is flagged on every generated `e:match` so the
;; exhaustiveness checker skips it — the caller is explicitly
;; asserting the pattern fits.  Sum-type cases that need
;; fall-through across constructors should use the multi-clause
;; `define` mechanism instead of a single-clause refutable pattern.
(define (parse-fn-params+body params-stx body-stx ctx-stx)
  (define params-list (syntax->list params-stx))
  (define-values (names-rev wrappers-rev)
    (for/fold ([names '()] [wrappers '()])
              ([p (in-list params-list)])
      (cond
        [(identifier? p)
         ;; Bare identifier: use directly as the lambda's
         ;; parameter name.  No pattern parsing — `Nil` here is a
         ;; parameter literally named `Nil`, not a 0-arg ctor
         ;; pattern.  To pattern-match against a 0-arg ctor, wrap
         ;; in parens: `(Nil)`.
         (values (cons (syntax->datum p) names) wrappers)]
        [else
         (define pat (parse-pattern p))
         (define fresh (gensym '$arg))
         (values (cons fresh names)
                 (cons (cons fresh pat) wrappers))])))
  (define names    (reverse names-rev))
  (define wrappers (reverse wrappers-rev))
  (define body-ast (parse-expr body-stx))
  (define wrapped
    (foldr (lambda (w body)
             (e:match (e:var (car w) ctx-stx)
                      (list (clause (cdr w) #f body body-stx))
                      #t
                      ctx-stx))
           body-ast
           wrappers))
  ;; Stash the original params/body for the multi-clause combiner
  ;; that may run in `parse-toplevel-list`.  Outside that scope the
  ;; record parameter is #f and this is a no-op.
  (let ([rec (current-fn-clauses-record)])
    (when rec
      (hash-set! rec ctx-stx (cons params-stx body-stx))))
  (values names wrapped))

;; ----- types --------------------------------------------------------

(define (parse-type stx)
  (syntax-parse stx
    #:datum-literals (All => ->)
    [(All (v:id ...) body)
     (ty:forall (map syntax->datum (syntax->list #'(v ...)))
                (parse-type #'body)
                stx)]
    ;; Qualified type: (constraint ...+ => body)
    [(c ...+ => body)
     (ty:qual (for/list ([cstx (in-list (syntax->list #'(c ...)))])
                (parse-constraint cstx))
              (parse-type #'body)
              stx)]
    ;; Arrow type — variadic in the surface, binary in the core AST.
    ;;   `(-> T)`         → `(-> Unit T)` (0-arg fn returning T)
    ;;   `(-> A B)`       → standard binary arrow
    ;;   `(-> A B C …)`   → right-associates: `(-> A (-> B (-> C …)))`
    [(-> arg ...+)
     (build-arrow-type (syntax->list #'(arg ...)) stx)]
    [x:id
     (define name (syntax->datum #'x))
     (if (lowercase-id? name)
         (ty:var name stx)
         (ty:con name stx))]
    [(head arg ...+)
     (ty:app (parse-type #'head)
             (for/list ([a (in-list (syntax->list #'(arg ...)))])
               (parse-type a))
             stx)]))

;; Right-fold a variadic `->` form into binary arrow applications so the
;; core type AST stays binary (downstream stages only ever see `(-> A B)`).
(define (build-arrow-type arg-stxs stx)
  (cond
    [(null? (cdr arg-stxs))
     ;; `(-> T)` — 0-arg fn encoding.
     (ty:app (ty:con '-> stx)
             (list (ty:con 'Unit stx) (parse-type (car arg-stxs)))
             stx)]
    [(null? (cddr arg-stxs))
     ;; `(-> A B)` — terminal binary arrow.
     (ty:app (ty:con '-> stx)
             (list (parse-type (car arg-stxs)) (parse-type (cadr arg-stxs)))
             stx)]
    [else
     ;; `(-> A B C …)` — right-associate.
     (ty:app (ty:con '-> stx)
             (list (parse-type (car arg-stxs))
                   (build-arrow-type (cdr arg-stxs) stx))
             stx)]))

;; Parse a constraint expression like `(Eq a)` or `(Foo (Maybe a))`.
;; The head must be a non-lowercase identifier (a class name); every
;; argument is a plain type.  (Kind-annotated class parameters appear
;; only in a `protocol` head, which `parse-class-head` handles — not
;; through this entry point.)
(define (parse-constraint stx)
  (syntax-parse stx
    [(name:id arg ...+)
     #:fail-unless (not (lowercase-id? (syntax->datum #'name)))
     "class name in a constraint must be a non-lowercase identifier"
     (constraint (syntax->datum #'name)
                 (for/list ([a (in-list (syntax->list #'(arg ...)))])
                   (parse-type a))
                 stx)]))

;; Parse a kind expression: `*` or `(-> k1 k2 …)`.  Like the type arrow,
;; the kind arrow is variadic in surface syntax and right-associates into
;; binary `k:arr` nodes in the core kind AST.
(define (parse-kind-stx stx)
  (syntax-parse stx
    #:datum-literals (* ->)
    [* (k:star)]
    [(-> k1 k2 ks ...)
     (build-arrow-kind (cons #'k1 (cons #'k2 (syntax->list #'(ks ...)))))]))

;; Right-fold a variadic `->` kind form into binary `k:arr` nodes.  Expects
;; at least two kind syntax objects (enforced by the caller's pattern).
(define (build-arrow-kind kind-stxs)
  (cond
    [(null? (cdr kind-stxs))
     (parse-kind-stx (car kind-stxs))]
    [else
     (k:arr (parse-kind-stx (car kind-stxs))
            (build-arrow-kind (cdr kind-stxs)))]))

;; ----- foreign-c helpers --------------------------------------------

;; Split a C signature datum `(cty ... -> cty)` into (values arg-tags
;; result-tag) on the single `->`.  `(-> int)` yields no args.
(define (split-c-sig parts stx)
  (unless (list? parts)
    (raise-syntax-error 'foreign-c "#:sig must be a list (cty ... -> cty)" stx))
  (let loop ([xs parts] [args '()])
    (cond
      [(null? xs)
       (raise-syntax-error 'foreign-c "#:sig must contain ->" stx)]
      [(eq? (car xs) '->)
       (define after (cdr xs))
       (unless (and (pair? after) (null? (cdr after)))
         (raise-syntax-error 'foreign-c
                             "#:sig must have exactly one result type after ->" stx))
       (values (reverse args) (car after))]
      [else (loop (cdr xs) (cons (car xs) args))])))

;; Is the source type `t`, after peeling `n` argument arrows, headed by
;; IO?  Used to decide whether a foreign-c binding is a pure function or
;; an IO action.
(define (foreign-c-io-headed? t)
  (and (ty:app? t)
       (let ([h (ty:app-head t)])
         (and (ty:con? h) (eq? (ty:con-name h) 'IO)))))

(define (type-result-io? t n)
  (if (<= n 0)
      (foreign-c-io-headed? t)
      (and (ty:app? t)
           (let ([h (ty:app-head t)]
                 [args (ty:app-args t)])
             (and (ty:con? h) (eq? (ty:con-name h) '->) (= 2 (length args))
                  (type-result-io? (cadr args) (sub1 n)))))))

;; ----- top-level forms ----------------------------------------------

(define (parse-top stx)
  (syntax-parse stx
    #:datum-literals (define data newtype struct protocol instance define-alias define-effect require provide foreign foreign-c : =>)
    [(require spec ...)
     (top:require (syntax->list #'(spec ...)) stx)]

    ;; (foreign name τ #:from M)            — racket-id = name
    ;; (foreign name τ #:from M #:as rkt-id) — renamed
    ;; M is a module path (collection id like racket/string, or a
    ;; relative "file.rkt" string).
    [(foreign name:id ty #:from mod #:as rkt:id)
     (top:foreign (syntax->datum #'name) (parse-type #'ty)
                  (syntax->datum #'mod) (syntax->datum #'rkt) stx)]
    [(foreign name:id ty #:from mod)
     (top:foreign (syntax->datum #'name) (parse-type #'ty)
                  (syntax->datum #'mod) (syntax->datum #'name) stx)]

    ;; (foreign-c name τ #:lib L #:symbol S #:sig (cty ... -> cty))
    ;; Inline C-function import.  L is a string or #f; S a string; the
    ;; sig is C type keywords with a single `->` splitting args / result.
    [(foreign-c name:id ty #:lib lib #:symbol sym #:sig sig)
     (let*-values ([(t)             (parse-type #'ty)]
                   [(arg-tags res)  (split-c-sig (syntax->datum #'sig) #'sig)])
       (top:foreign-c (syntax->datum #'name) t
                      (syntax->datum #'lib) (syntax->datum #'sym)
                      arg-tags res
                      (type-result-io? t (length arg-tags))
                      stx))]

    [(provide spec ...)
     (top:provide (syntax->list #'(spec ...)) stx)]

    [(define-alias (aname:id aparam:id ...) target)
     #:fail-unless (not (lowercase-id? (syntax->datum #'aname)))
     "type alias name must be a non-lowercase identifier"
     (top:alias (syntax->datum #'aname)
                (map syntax->datum (syntax->list #'(aparam ...)))
                (parse-type #'target)
                stx)]
    [(define-alias aname:id target)
     #:fail-unless (not (lowercase-id? (syntax->datum #'aname)))
     "type alias name must be a non-lowercase identifier"
     (top:alias (syntax->datum #'aname) '() (parse-type #'target) stx)]
    [(: name:id ty)
     (top:dec (syntax->datum #'name) (parse-type #'ty) stx)]

    [(define (f:id arg ...) body)
     ;; A 0-arg define `(define (f) body)` desugars to a
     ;; lambda with one ignored Unit parameter, matching the 0-arg
     ;; call-site convention.  Without this `(f)` would call a
     ;; non-function value.  Each `arg` may be a plain identifier
     ;; (the historical case) or any pattern; pattern parameters
     ;; desugar via `parse-fn-params+body` into a synthetic
     ;; identifier plus an irrefutable match in the body.
     (define arg-list (syntax->list #'(arg ...)))
     (cond
       [(null? arg-list)
        (top:def (syntax->datum #'f)
                 (e:lam '($unit-arg) (parse-expr #'body) stx)
                 stx)]
       [else
        (define-values (names body-ast)
          (parse-fn-params+body #'(arg ...) #'body stx))
        (top:def (syntax->datum #'f)
                 (e:lam names body-ast stx)
                 stx)])]
    [(define x:id e)
     (top:def (syntax->datum #'x) (parse-expr #'e) stx)]

    [(data (tname:id tparam:id ...) item ...)
     #:fail-unless (not (lowercase-id? (syntax->datum #'tname)))
     "data type name must be a non-lowercase identifier"
     (parse-data-form (syntax->datum #'tname)
                      (map syntax->datum (syntax->list #'(tparam ...)))
                      (syntax->list #'(item ...))
                      stx
                      #'tname)]
    [(data tname:id item ...)
     #:fail-unless (not (lowercase-id? (syntax->datum #'tname)))
     "data type name must be a non-lowercase identifier"
     (parse-data-form (syntax->datum #'tname)
                      '()
                      (syntax->list #'(item ...))
                      stx
                      #'tname)]

    ;; (newtype Name (Wrap T) [#:deriving Cls ...])
    ;; (newtype (Name a ...) (Wrap T) [#:deriving Cls ...])
    ;; Sugar over data for the common "one ctor, one field"
    ;; case.  A nominal wrapper around an existing type.  At runtime
    ;; the wrapper is a plain ADT — the "zero-cost" of a newtype is
    ;; documentary, not a perf optimization.  A trailing
    ;; `#:deriving Cls ...` flows through to parse-data-form.
    [(newtype (tname:id tparam:id ...) (ctor:id ftype) rest ...)
     #:fail-unless (not (lowercase-id? (syntax->datum #'tname)))
     "newtype name must be a non-lowercase identifier"
     #:fail-unless (not (lowercase-id? (syntax->datum #'ctor)))
     "newtype constructor name must be a non-lowercase identifier"
     #:fail-unless (newtype-rest-ok? #'(rest ...))
     "newtype must declare exactly one constructor with one field — for multiple ctors or multiple fields use data"
     (parse-data-form (syntax->datum #'tname)
                      (map syntax->datum (syntax->list #'(tparam ...)))
                      (cons #'(ctor ftype) (syntax->list #'(rest ...)))
                      stx
                      #'tname)]
    [(newtype tname:id (ctor:id ftype) rest ...)
     #:fail-unless (not (lowercase-id? (syntax->datum #'tname)))
     "newtype name must be a non-lowercase identifier"
     #:fail-unless (not (lowercase-id? (syntax->datum #'ctor)))
     "newtype constructor name must be a non-lowercase identifier"
     #:fail-unless (newtype-rest-ok? #'(rest ...))
     "newtype must declare exactly one constructor with one field — for multiple ctors or multiple fields use data"
     (parse-data-form (syntax->datum #'tname)
                      '()
                      (cons #'(ctor ftype) (syntax->list #'(rest ...)))
                      stx
                      #'tname)]
    [(newtype _ _ ...)
     (raise-syntax-error
      'newtype
      "newtype must declare exactly one constructor with one field — for multiple ctors or multiple fields use data"
      stx)]

    ;; (struct (Name a b ...) [field : type] ...) and the bare
    ;; non-parameterised variant.  Desugars to a single-constructor
    ;; data plus one accessor function per field.
    [(struct (sname:id sparam:id ...) field ...)
     #:fail-unless (not (lowercase-id? (syntax->datum #'sname)))
     "struct name must be a non-lowercase identifier"
     (parse-struct-form (syntax->datum #'sname)
                        (map syntax->datum (syntax->list #'(sparam ...)))
                        (syntax->list #'(field ...))
                        stx
                        #'sname)]
    [(struct sname:id field ...)
     #:fail-unless (not (lowercase-id? (syntax->datum #'sname)))
     "struct name must be a non-lowercase identifier"
     (parse-struct-form (syntax->datum #'sname)
                        '()
                        (syntax->list #'(field ...))
                        stx
                        #'sname)]

    ;; A protocol: `(Name binder …)`, with superclasses expressed as
    ;; per-parameter `=>` bounds on the binders and/or `(#:requires …)`
    ;; clauses in the body.  The prefix form `((Super x) … => (Name …))`
    ;; has been retired.
    [(protocol head body ...)
     (define-values (head-constraint bound-supers)
       (parse-class-head #'head))
     (define-values (req-supers methods)
       (parse-class-body (syntax->list #'(body ...))))
     (top:class (append bound-supers req-supers)
                head-constraint
                methods
                stx)]

    ;; Instance with context: ((Eq a) ... => (Eq (Maybe a)))
    [(instance (ctx ...+ => head) body ...)
     (top:instance (for/list ([c (in-list (syntax->list #'(ctx ...)))])
                     (parse-constraint c))
                   (parse-constraint #'head)
                   (for/list ([m (in-list (syntax->list #'(body ...)))])
                     (parse-instance-method m))
                   stx)]
    [(instance head body ...)
     (top:instance '()
                   (parse-constraint #'head)
                   (for/list ([m (in-list (syntax->list #'(body ...)))])
                     (parse-instance-method m))
                   stx)]

    ;; (define-effect Name (op argType ... -> resultType) ...)
    [(define-effect name:id op ...)
     #:fail-unless (not (lowercase-id? (syntax->datum #'name)))
     "effect name must be a non-lowercase identifier"
     (top:effect (syntax->datum #'name)
                 (for/list ([o (in-list (syntax->list #'(op ...)))])
                   (parse-effect-op o))
                 stx)]))

(define (parse-effect-op stx)
  (syntax-parse stx
    #:datum-literals (->)
    [(name:id arg ... -> result)
     (effect-op (syntax->datum #'name)
                (for/list ([a (in-list (syntax->list #'(arg ...)))])
                  (parse-type a))
                (parse-type #'result)
                stx)]))

;; ----- protocol head -----------------------------------------------
;;
;; A `protocol` head is `(Name binder …)`.  Each binder is one of:
;;   v            — a plain parameter (kind inferred / defaults to *)
;;   [v :: k]     — an explicit kind annotation, no superclass
;;   [v => B …]   — one or more superclass BOUNDS on `v`
;; A bound `B` is a class head missing its final argument; `v` supplies
;; it.  So `[v => Functor]` ⇒ `(Functor v)` and `[v => (Pairing f)]` ⇒
;; `(Pairing f v)`.  The head desugars to a plain class constraint plus
;; a list of superclass constraints, matching the representation the old
;; prefix form `((Super x) … => (Name …))` produced.

;; Desugar one bound entry `B` for parameter `var` into a superclass
;; constraint, appending `var` as the final argument.
(define (bound->constraint stx var)
  (syntax-parse stx
    [c:id
     #:fail-unless (not (lowercase-id? (syntax->datum #'c)))
     "superclass in a bound must be a non-lowercase class name"
     (constraint (syntax->datum #'c) (list var) stx)]
    [(c:id arg ...+)
     #:fail-unless (not (lowercase-id? (syntax->datum #'c)))
     "superclass head in a bound must be a non-lowercase class name"
     (constraint (syntax->datum #'c)
                 (append (for/list ([a (in-list (syntax->list #'(arg ...)))])
                           (parse-type a))
                         (list var))
                 stx)]))

;; Parse one head binder.  Returns (values head-tyvar bound-constraints).
;; The head-tyvar carries an explicit kind on its stx as 'rackton:kind
;; when written `[v :: k]`; otherwise the kind is left to inference.
(define (parse-class-param stx)
  (syntax-parse stx
    #:datum-literals (:: =>)
    [v:id
     #:fail-unless (lowercase-id? (syntax->datum #'v))
     "class parameter must be a lowercase identifier"
     (values (ty:var (syntax->datum #'v) #'v) '())]
    [(v:id :: k)
     #:fail-unless (lowercase-id? (syntax->datum #'v))
     "class parameter must be a lowercase identifier"
     (values (ty:var (syntax->datum #'v)
                     (syntax-property #'v 'rackton:kind (parse-kind-stx #'k)))
             '())]
    [(v:id => b ...+)
     #:fail-unless (lowercase-id? (syntax->datum #'v))
     "class parameter must be a lowercase identifier"
     (define var (ty:var (syntax->datum #'v) #'v))
     (values var
             (for/list ([b (in-list (syntax->list #'(b ...)))])
               (bound->constraint b var)))]))

;; Parse a protocol head `(Name binder …)` into (values head-constraint
;; bound-supers), where bound-supers are the superclass constraints
;; contributed by `=>` bounds on the binders.
(define (parse-class-head stx)
  (syntax-parse stx
    [(name:id binder ...+)
     #:fail-unless (not (lowercase-id? (syntax->datum #'name)))
     "class name must be a non-lowercase identifier"
     (define-values (vars super-lists)
       (for/lists (vs ss)
                  ([b (in-list (syntax->list #'(binder ...)))])
         (parse-class-param b)))
     (values (constraint (syntax->datum #'name) vars stx)
             (append* super-lists))]))

;; Split a protocol body into superclass constraints contributed by
;; `(#:requires c …)` clauses and the remaining method items.  Keyword
;; clauses self-quote in syntax-parse, like `#:fundep` / `#:type`.
(define (parse-class-body items)
  (for/fold ([supers '()] [methods '()]
             #:result (values (reverse supers) (reverse methods)))
            ([item (in-list items)])
    (syntax-parse item
      [(#:requires c ...+)
       (values (append (reverse (for/list ([cs (in-list (syntax->list #'(c ...)))])
                                  (parse-constraint cs)))
                       supers)
               methods)]
      [_ (values supers (cons (parse-class-method item) methods))])))

;; A method form inside `protocol`: either a `(: name type)` signature,
;; a `(define ...)` providing a default implementation, or a functional
;; dependency `(#:fundep lhs … -> rhs …)`.
(define (parse-class-method stx)
  (syntax-parse stx
    #:datum-literals (: define ->)
    [(: name:id ty)
     (method-sig (syntax->datum #'name) (parse-type #'ty) stx)]
    [(define (f:id arg ...) body)
     (define arg-list (syntax->list #'(arg ...)))
     (cond
       [(null? arg-list)
        (method-default (syntax->datum #'f)
                        (e:lam '($unit-arg) (parse-expr #'body) stx)
                        stx)]
       [else
        (define-values (names body-ast)
          (parse-fn-params+body #'(arg ...) #'body stx))
        (method-default (syntax->datum #'f)
                        (e:lam names body-ast stx)
                        stx)])]
    [(define x:id e)
     (method-default (syntax->datum #'x) (parse-expr #'e) stx)]
    [(#:fundep lhs:id ...+ -> rhs:id ...+)
     (class-fundep (map syntax->datum (syntax->list #'(lhs ...)))
                   (map syntax->datum (syntax->list #'(rhs ...)))
                   stx)]
    [(#:type name:id)
     #:fail-unless (not (lowercase-id? (syntax->datum #'name)))
     "associated-type name must be a non-lowercase identifier"
     (class-type-fam (syntax->datum #'name) stx)]))

;; An instance method must be a `define`, or a `#:type` binding for
;; an associated type declared by the class.
(define (parse-instance-method stx)
  (syntax-parse stx
    #:datum-literals (define =)
    [(#:type (name:id = ty))
     #:fail-unless (not (lowercase-id? (syntax->datum #'name)))
     "associated-type name must be a non-lowercase identifier"
     (inst-type-fam (syntax->datum #'name) (parse-type #'ty) stx)]
    [(define (f:id arg ...) body)
     (define arg-list (syntax->list #'(arg ...)))
     (cond
       [(null? arg-list)
        (top:def (syntax->datum #'f)
                 (e:lam '($unit-arg) (parse-expr #'body) stx)
                 stx)]
       [else
        (define-values (names body-ast)
          (parse-fn-params+body #'(arg ...) #'body stx))
        (top:def (syntax->datum #'f)
                 (e:lam names body-ast stx)
                 stx)])]
    [(define x:id e)
     (top:def (syntax->datum #'x) (parse-expr #'e) stx)]))

;; Split a data body into constructor specs and an optional
;; `#:deriving Cls ...` tail.  Returns a list of top-level forms:
;; the top:data and any synthesized top:instance entries.
;;
;; For the synthesized instances we use the syntax handle of the *first
;; constructor spec* as the lexical-context anchor — that handle is an
;; actual identifier from the user's source and so carries the same
;; scope set as anything else the user wrote.  Using the whole-form's
;; syntax instead leaves the synthesised identifiers missing scopes
;; that show up only on individual identifier-leaf syntax objects.
(define (parse-data-form tname tparams items stx [tname-stx #f])
  ;; Peel off the `#:abstract` flag (it may appear
  ;; alongside `#:deriving` in any order before the ctor list ends).
  (define-values (items-1 abstract?) (split-abstract items))
  (define-values (items-2 runtime-tag) (split-runtime-tag items-1))
  (define-values (ctor-stxs deriving-classes)
    (split-deriving items-2))
  (define ctors
    (for/list ([c (in-list ctor-stxs)]) (parse-data-ctor c)))
  (define data-form (top:data tname tparams ctors stx abstract? runtime-tag))
  (cond
    [(null? deriving-classes) data-form]
    [else
     (define ctx-stx
       (cond
         [tname-stx tname-stx]
         [(pair? ctor-stxs) (car ctor-stxs)]
         [else stx]))
     (cons data-form
           (synthesize-deriving deriving-classes
                                tname tparams ctors ctx-stx stx
                                'data))]))

;; A newtype's `rest` after `(ctor ftype)` is permitted only if it
;; is either empty or starts with the `#:deriving` keyword.  Reject
;; anything else (extra ctor specs, stray items) at parse time.
(define (newtype-rest-ok? rest-stx)
  (define rest (syntax->list rest-stx))
  (or (null? rest)
      (eq? (syntax->datum (car rest)) '#:deriving)))

;; Peel off a `#:abstract` flag anywhere it appears in a
;; data/struct body items list.  Returns (values rest abstract?)
;; where abstract? is #t iff the keyword was found.
(define (split-abstract items)
  (let loop ([rem items] [acc '()] [abs? #f])
    (cond
      [(null? rem) (values (reverse acc) abs?)]
      [(eq? (syntax->datum (car rem)) '#:abstract)
       (loop (cdr rem) acc #t)]
      [else (loop (cdr rem) (cons (car rem) acc) abs?)])))

;; Peel off a `#:runtime-tag tag` pair from a data body items list.
;; Returns (values rest tag-symbol-or-#f).  The tag names the dispatch
;; tag the type's opaque runtime values carry (see tcon-info runtime-tag
;; in env.rkt); used for foreign-backed opaque types with instances.
(define (split-runtime-tag items)
  (let loop ([rem items] [acc '()])
    (cond
      [(null? rem) (values (reverse acc) #f)]
      [(eq? (syntax->datum (car rem)) '#:runtime-tag)
       (when (null? (cdr rem))
         (raise-syntax-error 'data "#:runtime-tag must be followed by a tag"
                             (car rem)))
       (values (append (reverse acc) (cddr rem))
               (syntax->datum (cadr rem)))]
      [else (loop (cdr rem) (cons (car rem) acc))])))

;; Split the trailing `#:deriving Cls ...` clause off a list of body
;; items.  Returns (values items-before deriving-classes).  Shared
;; by data, newtype, and struct so all three
;; honor the same deriving menu.
(define (split-deriving items)
  (let loop ([rem items] [acc '()])
    (cond
      [(null? rem) (values (reverse acc) '())]
      [(eq? (syntax->datum (car rem)) '#:deriving)
       (values (reverse acc)
               (for/list ([c (in-list (cdr rem))])
                 (syntax->datum c)))]
      [else (loop (cdr rem) (cons (car rem) acc))])))

;; Synthesize the per-class instance forms for a data type's
;; deriving list.  `kind-tag` is the surface keyword raising the
;; error so messages stay accurate ('data / 'struct).
(define (synthesize-deriving classes tname tparams ctors ctx-stx err-stx kind-tag)
  ;; Deriving Ord implies deriving Eq (Ord has Eq as a superclass
  ;; and our `<` calls `==`); same for Foldable's required Functor
  ;; superclass when the user asks for Foldable without Functor.
  (define classes-needing-eq
    (cond
      [(and (member 'Ord classes) (not (member 'Eq classes)))
       (cons 'Eq classes)]
      [else classes]))
  ;; Deriving Monoid implies deriving Semigroup since
  ;; Monoid's class declaration lists Semigroup as a superclass.
  (define classes-needing-semigroup
    (cond
      [(and (member 'Monoid classes-needing-eq)
            (not (member 'Semigroup classes-needing-eq)))
       (cons 'Semigroup classes-needing-eq)]
      [else classes-needing-eq]))
  (apply append
         (for/list ([cls (in-list classes-needing-semigroup)])
           (case cls
             [(Eq)   (list (synthesize-eq-instance      tname tparams ctors ctx-stx))]
             [(Show) (list (synthesize-show-instance    tname tparams ctors ctx-stx))]
             [(Ord)  (list (synthesize-ord-instance     tname tparams ctors ctx-stx))]
             [(Functor)
              (cond
                [(null? tparams)
                 (raise-syntax-error kind-tag
                   "cannot derive Functor for a type with no type parameters"
                   err-stx)]
                [else
                 (list (synthesize-functor-instance tname tparams ctors ctx-stx))])]
             [(Foldable)
              (cond
                [(null? tparams)
                 (raise-syntax-error kind-tag
                   "cannot derive Foldable for a type with no type parameters"
                   err-stx)]
                [else
                 (list (synthesize-foldable-instance tname tparams ctors ctx-stx))])]
             [(Traversable)
              (list (synthesize-traversable-instance tname tparams ctors ctx-stx))]
             [(Bifunctor)
              (list (synthesize-bifunctor-instance tname tparams ctors ctx-stx))]
             [(Semigroup)
              (list (synthesize-semigroup-instance tname tparams ctors ctx-stx))]
             [(Monoid)
              (list (synthesize-monoid-instance tname tparams ctors ctx-stx))]
             [(Prism)
              (cond
                [(eq? kind-tag 'struct)
                 (raise-syntax-error kind-tag
                   "cannot derive Prism for struct (single-ctor record) — use Lens instead"
                   err-stx)]
                [else
                 (synthesize-prism-defs tname tparams ctors ctx-stx)])]
             [else
              (raise-syntax-error kind-tag
                (format "cannot derive ~s — supported: Eq, Ord, Show, Functor, Foldable, Traversable, Bifunctor, Semigroup, Monoid, Prism" cls)
                err-stx)]))))

;; ----- records: struct ---------------------------------------

(define (parse-struct-form name tparams field-stxs stx tname-stx)
  ;; Split off a trailing `#:deriving Cls ...` clause before
  ;; parsing field specs — the deriving classes are routed through the
  ;; shared synthesize-deriving helper.
  ;; Also peel off `#:abstract` from anywhere in the body.
  (define-values (field-stxs-1 abstract?) (split-abstract field-stxs))
  (define-values (field-only-stxs deriving-classes)
    (split-deriving field-stxs-1))
  (define field-pairs
    (for/list ([fs (in-list field-only-stxs)]) (parse-field-spec fs)))
  (define field-names (map car field-pairs))
  (define field-types (map cdr field-pairs))
  (define ctor (data-ctor-plain name field-types stx))
  (define data-form (top:data name tparams (list ctor) stx abstract? #f))
  (define accessor-defs
    (for/list ([fname (in-list field-names)]
               [i (in-naturals)])
      (synthesize-accessor name (length field-names) fname i tname-stx)))
  ;; Lens-deriving needs field-NAMES (to name the lenses
  ;; and generate the accessor / re-builder), which the generic
  ;; synthesize-deriving doesn't have.  Peel `Lens` out and handle
  ;; it here; pass the rest through to synthesize-deriving normally.
  (define-values (lens? other-deriving-classes)
    (partition-by-eq 'Lens deriving-classes))
  (define lens-defs
    (cond
      [lens? (synthesize-lens-defs name tparams field-names tname-stx)]
      [else '()]))
  (define derived
    (cond
      [(null? other-deriving-classes) '()]
      [else
       (synthesize-deriving other-deriving-classes
                            name tparams (list ctor)
                            tname-stx stx 'struct)]))
  (append (list data-form
                (top:struct-fields name field-names stx))
          accessor-defs derived lens-defs))

;; Peel a class symbol out of the deriving list.  Returns
;; (values present? rest).
(define (partition-by-eq sym xs)
  (cond
    [(member sym xs) (values #t (filter (lambda (x) (not (eq? x sym))) xs))]
    [else            (values #f xs)]))

;; Emit per-field lens defs `Tname-fname-lens` for a
;; single-ctor struct.  Each lens reuses the existing
;; accessor `Tname-fname` as the getter and rebuilds the struct
;; with `(Tname ...)` for the setter.
(define (synthesize-lens-defs tname tparams field-names ctx-stx)
  (define arity (length field-names))
  (for/list ([fname (in-list field-names)] [idx (in-naturals)])
    (define lens-name
      (string->symbol (format "~a-~a-lens" tname fname)))
    (define accessor-name
      (string->symbol (format "~a-~a" tname fname)))
    ;; Getter:  (lambda (p) (Tname-fname p))
    (define getter
      (e:lam '(p)
             (e:app (e:var accessor-name (fresh-stx ctx-stx))
                    (list (e:var 'p (fresh-stx ctx-stx)))
                    (fresh-stx ctx-stx))
             (fresh-stx ctx-stx)))
    ;; Setter:  (lambda (p) (lambda (v) (Tname f0 ... v ... fn)))
    ;; where f_j = (Tname-f_j p) for j != idx, and v at slot idx.
    (define ctor-args
      (for/list ([f-other (in-list field-names)] [j (in-naturals)])
        (cond
          [(= j idx)
           (e:var 'v (fresh-stx ctx-stx))]
          [else
           (define other-accessor
             (string->symbol (format "~a-~a" tname f-other)))
           (e:app (e:var other-accessor (fresh-stx ctx-stx))
                  (list (e:var 'p (fresh-stx ctx-stx)))
                  (fresh-stx ctx-stx))])))
    (define setter
      (e:lam '(p)
             (e:lam '(v)
                    (e:app (e:var tname (fresh-stx ctx-stx))
                           ctor-args
                           (fresh-stx ctx-stx))
                    (fresh-stx ctx-stx))
             (fresh-stx ctx-stx)))
    (define lens-body
      (e:app (e:var 'MkLens (fresh-stx ctx-stx))
             (list getter setter)
             (fresh-stx ctx-stx)))
    (top:def lens-name lens-body ctx-stx)))

(define (parse-field-spec stx)
  (syntax-parse stx
    #:datum-literals (:)
    [(fname:id : t)
     #:fail-unless (lowercase-id? (syntax->datum #'fname))
     "struct field name must be a lowercase identifier"
     (cons (syntax->datum #'fname) (parse-type #'t))]))

;; Build `(define (Name-fname r) (match r [(Name _ … v … _) v]))`.
(define (synthesize-accessor struct-name arity field-name idx ctx-stx)
  (define accessor-name
    (string->symbol (format "~a-~a" struct-name field-name)))
  (define pat-vars
    (for/list ([j (in-range arity)])
      (if (= j idx) (p:var 'v ctx-stx) (p:wild ctx-stx))))
  (define pat (p:ctor struct-name pat-vars ctx-stx))
  (define body
    (e:match (e:var 'r ctx-stx)
             (list (clause pat #f (e:var 'v ctx-stx) ctx-stx))
             #f ctx-stx))
  (top:def accessor-name (e:lam '(r) body ctx-stx) ctx-stx))

;; Split a GADT constructor's `: SIG` type into (values field-stxs result-stx).
;; An arrow `(-> a₁ … aₙ)` with n ≥ 2 yields fields a₁…aₙ₋₁ and result aₙ.
;; A single-element arrow `(-> r)` (a 0-arg function) and any non-arrow type
;; both yield no fields and result = the type itself (a nullary constructor).
(define (split-ctor-signature sig)
  (syntax-parse sig
    #:datum-literals (->)
    [(-> a ...+)
     (define args (syntax->list #'(a ...)))
     (cond
       [(null? (cdr args)) (values '() (car args))]
       [else
        (define rev (reverse args))
        (values (reverse (cdr rev)) (car rev))])]
    [_ (values '() sig)]))

(define (parse-data-ctor stx)
  (syntax-parse stx
    [name:id
     #:fail-unless (not (lowercase-id? (syntax->datum #'name)))
     "data constructor name must be a non-lowercase identifier"
     (data-ctor-plain (syntax->datum #'name) '() stx)]
    ;; Existential ctor with #:forall and #:where clauses.
    [(name:id (~datum #:forall) (tv:id ...+)
              (~datum #:where) ctx ...
              ft ...+)
     #:fail-unless (not (lowercase-id? (syntax->datum #'name)))
     "data constructor name must be a non-lowercase identifier"
     (data-ctor (syntax->datum #'name)
                (for/list ([t (in-list (syntax->list #'(ft ...)))])
                  (parse-type t))
                stx
                (map syntax->datum (syntax->list #'(tv ...)))
                (for/list ([c (in-list (syntax->list #'(ctx ...)))])
                  (parse-constraint c))
                #f)]
    ;; GADT ctor with `: SIG` clause giving the ctor's full type
    ;; signature.  When SIG is an arrow `(-> ft … RT)` the leading
    ;; types are the fields and the final type is the refined result;
    ;; a non-arrow SIG is a nullary ctor whose result type is SIG.
    [(name:id (~datum :) sig)
     #:fail-unless (not (lowercase-id? (syntax->datum #'name)))
     "data constructor name must be a non-lowercase identifier"
     (define-values (field-stxs result-stx) (split-ctor-signature #'sig))
     (data-ctor (syntax->datum #'name)
                (for/list ([t (in-list field-stxs)]) (parse-type t))
                stx
                '()
                '()
                (parse-type result-stx))]
    [(name:id ft ...+)
     #:fail-unless (not (lowercase-id? (syntax->datum #'name)))
     "data constructor name must be a non-lowercase identifier"
     (data-ctor-plain (syntax->datum #'name)
                      (for/list ([t (in-list (syntax->list #'(ft ...)))])
                        (parse-type t))
                      stx)]))

(define (parse-toplevel-list stx-or-list)
  (define forms
    (cond
      [(syntax? stx-or-list) (syntax->list stx-or-list)]
      [(list? stx-or-list)   stx-or-list]
      [else (raise-argument-error 'parse-toplevel-list
                                  "syntax or list" stx-or-list)]))
  (define record (make-hasheq))
  (define raw
    (parameterize ([current-fn-clauses-record record])
      ;; A single surface form may parse to multiple AST entries (e.g.
      ;; `(data … #:deriving Eq Show)` desugars to the data plus
      ;; the two synthesized instances).  Flatten if so.
      (apply append
             (for/list ([f (in-list forms)])
               (define result (parse-top f))
               (if (list? result) result (list result))))))
  (combine-multi-clause-defs raw record))

;; Walk the parsed top-form list.  Group every top:def whose stx
;; recorded a function-form entry in `record` by name.  Singletons
;; pass through unchanged (already piece-1 desugared by parse-top).
;; A name with multiple clauses becomes one top:def whose body is
;; an `e:match*` over fresh argument identifiers, with each clause
;; carrying its parsed-as-pattern parameter list (so bare uppercase
;; identifiers dispatch as 0-arg ctor patterns rather than naming a
;; plain parameter).
(define (combine-multi-clause-defs parsed record)
  (define groups (make-hasheq))     ; name → (list-of top:def in reverse src order)
  (define non-fn (make-hasheq))     ; name → stx of conflicting non-function form
  (for ([f (in-list parsed)])
    (cond
      [(and (top:def? f) (hash-has-key? record (top:def-stx f)))
       (hash-update! groups (top:def-name f) (lambda (xs) (cons f xs)) '())]
      [(top:def? f)
       (hash-set! non-fn (top:def-name f) (top:def-stx f))]
      [else (void)]))
  ;; Validate: every name must be EITHER a single value def OR a
  ;; group of function clauses with matching arity.
  (for ([(name top-defs) (in-hash groups)])
    (define arities
      (for/seteq ([d (in-list top-defs)])
        (length (e:lam-params (top:def-expr d)))))
    (when (> (set-count arities) 1)
      (raise-syntax-error 'rackton
        (format "definition ~s has clauses with different arities: ~s"
                name (sort (set->list arities) <))
        (top:def-stx (car (reverse top-defs)))))
    (when (hash-has-key? non-fn name)
      (raise-syntax-error 'rackton
        (format "definition ~s mixes function-form (define (~s …)) and value-form (define ~s …)"
                name name name)
        (top:def-stx (car (reverse top-defs))))))
  ;; Emit: replace each first-occurrence clause-bearing top:def with
  ;; the combined form; skip subsequent occurrences for the same
  ;; name.  All other forms pass through.
  (define emitted (mutable-seteq))
  (apply append
         (for/list ([f (in-list parsed)])
           (cond
             [(and (top:def? f) (hash-has-key? record (top:def-stx f)))
              (define name (top:def-name f))
              (cond
                [(set-member? emitted name) '()]
                [else
                 (set-add! emitted name)
                 (define clauses (reverse (hash-ref groups name)))
                 (list (combine-fn-clauses name clauses record))])]
             [else (list f)]))))

;; If `top-defs` is a single clause, return it unchanged — parse-top
;; already produced the desugared singleton form.  Otherwise
;; synthesize one top:def whose body is an `e:match*` over fresh
;; arg names, with each clause's parameter list reparsed under the
;; multi-clause rule via `parse-clause-params`.
(define (combine-fn-clauses name top-defs record)
  (cond
    [(null? (cdr top-defs)) (car top-defs)]
    [else
     (define first (car top-defs))
     (define stx (top:def-stx first))
     (define arity (length (e:lam-params (top:def-expr first))))
     (define fresh-names
       (for/list ([_ (in-range arity)]) (gensym '$arg)))
     (define clauses
       (for/list ([d (in-list top-defs)])
         (define rec (hash-ref record (top:def-stx d)))
         (define params-stx (car rec))
         (define body-stx   (cdr rec))
         (define pats
           (for/list ([p-stx (in-list (syntax->list params-stx))])
             (parse-pattern p-stx)))
         (clause* pats #f
                  (parse-expr body-stx)
                  (top:def-stx d))))
     (define scrutinees
       (for/list ([n (in-list fresh-names)]) (e:var n stx)))
     (top:def name
              (e:lam fresh-names
                     (e:match* scrutinees clauses #f stx)
                     stx)
              stx)]))

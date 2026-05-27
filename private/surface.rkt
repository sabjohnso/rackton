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
         (struct-out e:escape)
         (struct-out e:letrec)
         (struct-out clause)

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
         racket/list)

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
(struct top:data     (name params ctors stx abstract?) #:transparent)
;; A data ctor may carry its own existential quantifier
;; via `#:forall (a) #:where (Cls a) ...` keywords between the ctor
;; name and the field types.  `extra-tvars` lists the existentially
;; quantified tvars; `extra-context` lists the constraints over them.
;; Existing (non-existential) ctors use empty lists for both.
;; `result-type` is either #f (default — the data type's
;; `(T tparams)` shape) or a ty-AST giving the ctor's specific
;; result type for GADTs (declared via `#:returns RT`).
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
;; Items inside a `define-class` body:
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
(define (chained-eq arity stx)
  (cond
    [(zero? arity) (e:literal #t stx)]
    [else
     (foldr
      (lambda (i acc)
        (e:if (e:app (e:var '== stx)
                     (list (e:var (a-name i) stx) (e:var (b-name i) stx))
                     stx)
              acc
              (e:literal #f stx)
              stx))
      (e:literal #t stx)
      (build-list arity values))]))

(define (synthesize-eq-instance tname tparams ctors stx)
  (define head-ty (data-head-type-ast tname tparams stx))
  (define ctx (for/list ([p (in-list tparams)])
                (constraint 'Eq (list (ty:var p stx)) stx)))
  (define head (constraint 'Eq (list head-ty) stx))
  ;; Outer match on x; for each ctor, inner match on y.
  (define eq-body
    (e:match
     (e:var 'x stx)
     (for/list ([c (in-list ctors)])
       (define name (data-ctor-name c))
       (define arity (length (data-ctor-field-types c)))
       (clause (ctor-x-pattern name arity stx) #f
               (e:match (e:var 'y stx)
                        (list (clause (ctor-y-pattern name arity stx) #f
                                      (chained-eq arity stx)
                                      stx)
                              (clause (p:wild stx) #f
                                      (e:literal #f stx)
                                      stx))
                        #f stx)
               stx))
     #f stx))
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
     (e:var 'x stx)
     (for/list ([c (in-list ctors)] [i (in-naturals)])
       (clause (ctor-x-pattern (data-ctor-name c)
                               (length (data-ctor-field-types c))
                               stx) #f
               (synthesize-ord-inner-match ctors c i stx)
               stx))
     #f stx))
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
     (list (clause (p:wild stx) #f (e:literal #t stx) stx))))
  (e:match (e:var 'y stx) clauses #f stx))

(define (chained-lex-less arity stx)
  (cond
    [(zero? arity) (e:literal #f stx)]
    [else
     (foldr
      (lambda (i acc)
        ;; (if (< ai bi) #t (if (== ai bi) <recurse> #f))
        (e:if (e:app (e:var '< stx)
                     (list (e:var (a-name i) stx) (e:var (b-name i) stx))
                     stx)
              (e:literal #t stx)
              (e:if (e:app (e:var '== stx)
                           (list (e:var (a-name i) stx) (e:var (b-name i) stx))
                           stx)
                    acc
                    (e:literal #f stx)
                    stx)
              stx))
      (e:literal #f stx)
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
     (e:var 'x stx)
     (for/list ([c (in-list ctors)])
       (define name (data-ctor-name c))
       (define field-types (data-ctor-field-types c))
       (define arity (length field-types))
       (clause (ctor-x-pattern name arity stx) #f
               (synthesize-functor-rebuild name field-types fparam tname stx)
               stx))
     #f stx))
  (top:instance '() head
                (list (top:def 'fmap
                               (e:lam '(f x) fmap-body stx)
                               stx))
                stx))

(define (synthesize-functor-rebuild ctor-name field-types fparam tname stx)
  (cond
    [(null? field-types)
     ;; Nullary constructors are values — emit the bare reference.
     (e:var ctor-name stx)]
    [else
     (define transformed
       (for/list ([ft (in-list field-types)] [i (in-naturals)])
         (transform-functor-field ft (a-name i) fparam tname stx)))
     (e:app (e:var ctor-name stx) transformed stx)]))

(define (transform-functor-field ft arg-name fparam tname stx)
  (define arg-var (e:var arg-name stx))
  (match ft
    ;; field is exactly the functor parameter: apply `f`
    [(ty:var n _) #:when (eq? n fparam)
     (e:app (e:var 'f stx) (list arg-var) stx)]
    ;; field is a recursive use of the same data type — recurse via fmap
    [(ty:app (ty:con t _) _ _) #:when (eq? t tname)
     (e:app (e:var 'fmap stx) (list (e:var 'f stx) arg-var) stx)]
    [(ty:con t _) #:when (eq? t tname)
     (e:app (e:var 'fmap stx) (list (e:var 'f stx) arg-var) stx)]
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
     (e:var 'x stx)
     (for/list ([c (in-list ctors)])
       (define name (data-ctor-name c))
       (define field-types (data-ctor-field-types c))
       (define arity (length field-types))
       (clause (ctor-x-pattern name arity stx) #f
               (synthesize-foldable-combine field-types fparam tname stx)
               stx))
     #f stx))
  (top:instance '() head
                (list (top:def 'foldr
                               (e:lam '(f z x) foldr-body stx)
                               stx))
                stx))

;; Build right-to-left combine of `f` over the ctor's fields, starting
;; from `z`.  Returns the body expression (`z` if no fields contribute).
(define (synthesize-foldable-combine field-types fparam tname stx)
  (for/foldr ([acc (e:var 'z stx)])
             ([ft (in-list field-types)]
              [i  (in-naturals)])
    (define arg (e:var (a-name i) stx))
    (cond
      ;; field is exactly the foldable parameter — combine directly.
      [(and (ty:var? ft) (eq? (ty:var-name ft) fparam))
       (e:app (e:var 'f stx) (list arg acc) stx)]
      ;; recursive use of the same data type — recurse via foldr.
      [(or (and (ty:app? ft)
                (ty:con? (ty:app-head ft))
                (eq? (ty:con-name (ty:app-head ft)) tname))
           (and (ty:con? ft)
                (eq? (ty:con-name ft) tname)))
       (e:app (e:var 'foldr stx)
              (list (e:var 'f stx) acc arg)
              stx)]
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
;;   N≥3      → `(<*> (<*> … (liftA2 Ctor lift0 lift1) … lift_{N-2}) lift_{N-1})`
(define (synthesize-traversable-instance tname tparams ctors stx)
  (when (null? tparams)
    (raise-syntax-error 'define-data
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
     (e:var 'x stx)
     (for/list ([c (in-list ctors)])
       (define name (data-ctor-name c))
       (define field-types (data-ctor-field-types c))
       (define arity (length field-types))
       (clause (ctor-x-pattern name arity stx) #f
               (synthesize-traverse-rebuild name field-types fparam tname stx)
               stx))
     #f stx))
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
     (e:app (e:var 'pure stx) (list (e:var ctor-name stx)) stx)]
    [(= arity 1)
     (e:app (e:var 'fmap stx)
            (list (e:var ctor-name stx) (car lifted-fields))
            stx)]
    [else
     ;; (liftA2 Ctor lift0 lift1) then chain (<*> acc liftN) for N≥2.
     (for/fold ([acc (e:app (e:var 'liftA2 stx)
                            (list (e:var ctor-name stx)
                                  (car lifted-fields)
                                  (cadr lifted-fields))
                            stx)])
               ([lf (in-list (cddr lifted-fields))])
       (e:app (e:var '<*> stx) (list acc lf) stx))]))

(define (transform-traverse-field ft arg-name fparam tname stx)
  (define arg (e:var arg-name stx))
  (cond
    [(and (ty:var? ft) (eq? (ty:var-name ft) fparam))
     (e:app (e:var 'f stx) (list arg) stx)]
    [(or (and (ty:app? ft)
              (ty:con? (ty:app-head ft))
              (eq? (ty:con-name (ty:app-head ft)) tname))
         (and (ty:con? ft)
              (eq? (ty:con-name ft) tname)))
     (e:app (e:var 'traverse stx) (list (e:var 'f stx) arg) stx)]
    [else
     (e:app (e:var 'pure stx) (list arg) stx)]))

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
    (raise-syntax-error 'define-data
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
     (e:var 'x stx)
     (for/list ([c (in-list ctors)])
       (define name (data-ctor-name c))
       (define field-types (data-ctor-field-types c))
       (define arity (length field-types))
       (clause (ctor-x-pattern name arity stx) #f
               (synthesize-bifunctor-rebuild name field-types
                                             f1 f2 tname stx)
               stx))
     #f stx))
  (top:instance '() head
                (list (top:def 'bimap
                               (e:lam '(f g x) bimap-body stx)
                               stx))
                stx))

(define (synthesize-bifunctor-rebuild ctor-name field-types f1 f2 tname stx)
  (cond
    [(null? field-types) (e:var ctor-name stx)]
    [else
     (define transformed
       (for/list ([ft (in-list field-types)] [i (in-naturals)])
         (transform-bifunctor-field ft (a-name i) f1 f2 tname stx)))
     (e:app (e:var ctor-name stx) transformed stx)]))

(define (transform-bifunctor-field ft arg-name f1 f2 tname stx)
  (define arg (e:var arg-name stx))
  (cond
    [(and (ty:var? ft) (eq? (ty:var-name ft) f1))
     (e:app (e:var 'f stx) (list arg) stx)]
    [(and (ty:var? ft) (eq? (ty:var-name ft) f2))
     (e:app (e:var 'g stx) (list arg) stx)]
    [(or (and (ty:app? ft)
              (ty:con? (ty:app-head ft))
              (eq? (ty:con-name (ty:app-head ft)) tname))
         (and (ty:con? ft)
              (eq? (ty:con-name ft) tname)))
     (e:app (e:var 'bimap stx) (list (e:var 'f stx) (e:var 'g stx) arg) stx)]
    [else arg]))

;; ----- Semigroup deriving ----------------------------
;; Single-ctor ADTs only.  Combine fields pairwise via `<>`.  Qual
;; context carries `(Semigroup ft)` for each unique field type.
;; Concrete field types (e.g. `String`) get discharged immediately
;; by reduce-context; tvar field types stay in the qual.
(define (synthesize-semigroup-instance tname tparams ctors stx)
  (unless (= (length ctors) 1)
    (raise-syntax-error 'define-data
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
    (raise-syntax-error 'define-data
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
  (apply append
         (for/list ([c (in-list ctors)])
           (define name (data-ctor-name c))
           (define arity (length (data-ctor-field-types c)))
           (cond
             [(= arity 0) (list (synth-prism-0-arg tname name ctors ctx-stx))]
             [(= arity 1) (list (synth-prism-1-arg tname name ctors ctx-stx))]
             [else '()]))))

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

(define (parse-expr stx)
  (syntax-parse stx
    #:datum-literals (lambda λ let letrec match-let where if cond else ann match racket do <- update handle return ->)
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

    [(let ([x:id rhs] ...) body)
     (e:let (for/list ([id (in-list (syntax->list #'(x ...)))]
                       [r  (in-list (syntax->list #'(rhs ...)))])
              (cons (syntax->datum id) (parse-expr r)))
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

    ;; (do [x <- m1] [y <- m2] ... body)  desugars to nested >>= calls.
    ;; A statement is `[var <- expr]`; each binds the un-wrapped value
    ;; for the rest of the chain.  The trailing `body` is the final
    ;; computation.
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

(define (parse-do stmts body-stx stx)
  (cond
    [(null? stmts) (parse-expr body-stx)]
    [else
     (define s (car stmts))
     (syntax-parse s
       #:datum-literals (<-)
       [[v:id <- expr]
        (e:app (e:var '>>= stx)
               (list (parse-expr #'expr)
                     (e:lam (list (syntax->datum #'v))
                            (parse-do (cdr stmts) body-stx stx)
                            stx))
               stx)]
       [expr
        ;; Bare-expression clause: sequence `expr` for its monadic
        ;; effect, discard the result, then continue.  Desugars to the
        ;; same shape as `[_fresh <- expr]` but with a fresh
        ;; identifier so the wildcard isn't a binder.
        (define fresh (gensym '_do))
        (e:app (e:var '>>= stx)
               (list (parse-expr #'expr)
                     (e:lam (list fresh)
                            (parse-do (cdr stmts) body-stx stx)
                            stx))
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
;; The head must be a non-lowercase identifier (a class name).  The
;; constraint args may be plain types OR — when this constraint appears
;; as a class head — kind-annotated type vars `(var :: kind)`, in which
;; case the kind annotation is stripped and the resulting type-var
;; remembers its kind via the syntax property 'rackton:kind on its stx.
(define (parse-constraint stx)
  (syntax-parse stx
    [(name:id arg ...+)
     #:fail-unless (not (lowercase-id? (syntax->datum #'name)))
     "class name in a constraint must be a non-lowercase identifier"
     (constraint (syntax->datum #'name)
                 (for/list ([a (in-list (syntax->list #'(arg ...)))])
                   (parse-constraint-arg a))
                 stx)]))

;; Constraint args may be either plain types or kind-annotated type
;; variables `(var :: kind)`.  We parse the annotated form as a plain
;; ty:var whose stx carries the kind as a property; the caller (the
;; class-form handler) reads it back when computing param kinds.
(define (parse-constraint-arg stx)
  (syntax-parse stx
    #:datum-literals (::)
    [(v:id :: k)
     #:fail-unless (lowercase-id? (syntax->datum #'v))
     "kind-annotated class parameter must be a lowercase identifier"
     (define kind (parse-kind-stx #'k))
     (define annotated (syntax-property #'v 'rackton:kind kind))
     (ty:var (syntax->datum #'v) annotated)]
    [_ (parse-type stx)]))

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

;; ----- top-level forms ----------------------------------------------

(define (parse-top stx)
  (syntax-parse stx
    #:datum-literals (define define-data define-newtype define-struct define-class define-instance define-alias define-effect require provide : =>)
    [(require spec ...)
     (top:require (syntax->list #'(spec ...)) stx)]

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

    [(define (f:id arg:id ...) body)
     ;; A 0-arg define `(define (f) body)` desugars to a
     ;; lambda with one ignored Unit parameter, matching the 0-arg
     ;; call-site convention.  Without this `(f)` would call a
     ;; non-function value.
     (define args (map syntax->datum (syntax->list #'(arg ...))))
     (top:def (syntax->datum #'f)
              (e:lam (cond
                       [(null? args) '($unit-arg)]
                       [else args])
                     (parse-expr #'body)
                     stx)
              stx)]
    [(define x:id e)
     (top:def (syntax->datum #'x) (parse-expr #'e) stx)]

    [(define-data (tname:id tparam:id ...) item ...)
     #:fail-unless (not (lowercase-id? (syntax->datum #'tname)))
     "data type name must be a non-lowercase identifier"
     (parse-data-form (syntax->datum #'tname)
                      (map syntax->datum (syntax->list #'(tparam ...)))
                      (syntax->list #'(item ...))
                      stx
                      #'tname)]
    [(define-data tname:id item ...)
     #:fail-unless (not (lowercase-id? (syntax->datum #'tname)))
     "data type name must be a non-lowercase identifier"
     (parse-data-form (syntax->datum #'tname)
                      '()
                      (syntax->list #'(item ...))
                      stx
                      #'tname)]

    ;; (define-newtype Name (Wrap T) [#:deriving Cls ...])
    ;; (define-newtype (Name a ...) (Wrap T) [#:deriving Cls ...])
    ;; Sugar over define-data for the common "one ctor, one field"
    ;; case.  A nominal wrapper around an existing type.  At runtime
    ;; the wrapper is a plain ADT — the "zero-cost" of a newtype is
    ;; documentary, not a perf optimization.  A trailing
    ;; `#:deriving Cls ...` flows through to parse-data-form.
    [(define-newtype (tname:id tparam:id ...) (ctor:id ftype) rest ...)
     #:fail-unless (not (lowercase-id? (syntax->datum #'tname)))
     "newtype name must be a non-lowercase identifier"
     #:fail-unless (not (lowercase-id? (syntax->datum #'ctor)))
     "newtype constructor name must be a non-lowercase identifier"
     #:fail-unless (newtype-rest-ok? #'(rest ...))
     "newtype must declare exactly one constructor with one field — for multiple ctors or multiple fields use define-data"
     (parse-data-form (syntax->datum #'tname)
                      (map syntax->datum (syntax->list #'(tparam ...)))
                      (cons #'(ctor ftype) (syntax->list #'(rest ...)))
                      stx
                      #'tname)]
    [(define-newtype tname:id (ctor:id ftype) rest ...)
     #:fail-unless (not (lowercase-id? (syntax->datum #'tname)))
     "newtype name must be a non-lowercase identifier"
     #:fail-unless (not (lowercase-id? (syntax->datum #'ctor)))
     "newtype constructor name must be a non-lowercase identifier"
     #:fail-unless (newtype-rest-ok? #'(rest ...))
     "newtype must declare exactly one constructor with one field — for multiple ctors or multiple fields use define-data"
     (parse-data-form (syntax->datum #'tname)
                      '()
                      (cons #'(ctor ftype) (syntax->list #'(rest ...)))
                      stx
                      #'tname)]
    [(define-newtype _ _ ...)
     (raise-syntax-error
      'define-newtype
      "newtype must declare exactly one constructor with one field — for multiple ctors or multiple fields use define-data"
      stx)]

    ;; (define-struct (Name a b ...) [field : type] ...) and the bare
    ;; non-parameterised variant.  Desugars to a single-constructor
    ;; define-data plus one accessor function per field.
    [(define-struct (sname:id sparam:id ...) field ...)
     #:fail-unless (not (lowercase-id? (syntax->datum #'sname)))
     "struct name must be a non-lowercase identifier"
     (parse-struct-form (syntax->datum #'sname)
                        (map syntax->datum (syntax->list #'(sparam ...)))
                        (syntax->list #'(field ...))
                        stx
                        #'sname)]
    [(define-struct sname:id field ...)
     #:fail-unless (not (lowercase-id? (syntax->datum #'sname)))
     "struct name must be a non-lowercase identifier"
     (parse-struct-form (syntax->datum #'sname)
                        '()
                        (syntax->list #'(field ...))
                        stx
                        #'sname)]

    ;; A class with a superclass list: ((C1 a) ... => (D a))
    [(define-class (sup ...+ => head) body ...)
     (top:class (for/list ([s (in-list (syntax->list #'(sup ...)))])
                  (parse-constraint s))
                (parse-constraint #'head)
                (for/list ([m (in-list (syntax->list #'(body ...)))])
                  (parse-class-method m))
                stx)]
    ;; A class with no superclasses: (D a)
    [(define-class head body ...)
     (top:class '()
                (parse-constraint #'head)
                (for/list ([m (in-list (syntax->list #'(body ...)))])
                  (parse-class-method m))
                stx)]

    ;; Instance with context: ((Eq a) ... => (Eq (Maybe a)))
    [(define-instance (ctx ...+ => head) body ...)
     (top:instance (for/list ([c (in-list (syntax->list #'(ctx ...)))])
                     (parse-constraint c))
                   (parse-constraint #'head)
                   (for/list ([m (in-list (syntax->list #'(body ...)))])
                     (parse-instance-method m))
                   stx)]
    [(define-instance head body ...)
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

;; A method form inside `define-class`: either a `(: name type)` signature,
;; a `(define ...)` providing a default implementation, or a functional
;; dependency `(#:fundep lhs … -> rhs …)`.
(define (parse-class-method stx)
  (syntax-parse stx
    #:datum-literals (: define ->)
    [(: name:id ty)
     (method-sig (syntax->datum #'name) (parse-type #'ty) stx)]
    [(define (f:id arg:id ...) body)
     (method-default (syntax->datum #'f)
                     (e:lam (map syntax->datum (syntax->list #'(arg ...)))
                            (parse-expr #'body)
                            stx)
                     stx)]
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
    [(define (f:id arg:id ...) body)
     (top:def (syntax->datum #'f)
              (e:lam (map syntax->datum (syntax->list #'(arg ...)))
                     (parse-expr #'body) stx)
              stx)]
    [(define x:id e)
     (top:def (syntax->datum #'x) (parse-expr #'e) stx)]))

;; Split a define-data body into constructor specs and an optional
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
  (define-values (ctor-stxs deriving-classes)
    (split-deriving items-1))
  (define ctors
    (for/list ([c (in-list ctor-stxs)]) (parse-data-ctor c)))
  (define data-form (top:data tname tparams ctors stx abstract?))
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
                                'define-data))]))

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

;; Split the trailing `#:deriving Cls ...` clause off a list of body
;; items.  Returns (values items-before deriving-classes).  Shared
;; by define-data, define-newtype, and define-struct so all three
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
;; error so messages stay accurate ('define-data / 'define-struct).
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
                [(eq? kind-tag 'define-struct)
                 (raise-syntax-error kind-tag
                   "cannot derive Prism for define-struct (single-ctor record) — use Lens instead"
                   err-stx)]
                [else
                 (synthesize-prism-defs tname tparams ctors ctx-stx)])]
             [else
              (raise-syntax-error kind-tag
                (format "cannot derive ~s — supported: Eq, Ord, Show, Functor, Foldable, Traversable, Bifunctor, Semigroup, Monoid, Prism" cls)
                err-stx)]))))

;; ----- records: define-struct ---------------------------------------

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
  (define data-form (top:data name tparams (list ctor) stx abstract?))
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
                            tname-stx stx 'define-struct)]))
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
;; single-ctor define-struct.  Each lens reuses the existing
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
    ;; GADT ctor with `#:returns RT` clause that gives
    ;; the ctor's specific result type instead of the default
    ;; `(T tparams)` shape.
    [(name:id (~datum #:returns) rt ft ...)
     #:fail-unless (not (lowercase-id? (syntax->datum #'name)))
     "data constructor name must be a non-lowercase identifier"
     (data-ctor (syntax->datum #'name)
                (for/list ([t (in-list (syntax->list #'(ft ...)))])
                  (parse-type t))
                stx
                '()
                '()
                (parse-type #'rt))]
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
  ;; A single surface form may parse to multiple AST entries (e.g.
  ;; `(define-data … #:deriving Eq Show)` desugars to the data plus
  ;; the two synthesized instances).  Flatten if so.
  (apply append
         (for/list ([f (in-list forms)])
           (define result (parse-top f))
           (if (list? result) result (list result)))))

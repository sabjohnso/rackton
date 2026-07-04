#lang racket/base

;; Rackton — `:deriving` instance synthesis.
;;
;; Given a data type's name, parameters, and constructors, build the
;; typed-core AST for a derived class instance (Eq, Ord, Show, Functor,
;; Foldable, Traversable, Bifunctor, Semigroup, Monoid) or the focusing
;; defs for Prism.  Pure AST construction over ast.rkt — no parsing — so
;; "add a derivable class" is one self-contained reason to change, isolated
;; from the surface parser that dispatches into it (surface.rkt's
;; synthesize-deriving).

(require "ast.rkt"
         racket/list
         racket/match)

(provide synthesize-eq-instance
         synthesize-ord-instance
         synthesize-show-instance
         synthesize-functor-instance
         synthesize-foldable-instance
         synthesize-traversable-instance
         synthesize-bifunctor-instance
         synthesize-semigroup-instance
         synthesize-monoid-instance
         synthesize-prism-defs)

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
     (e:app (e:var 'fmap (mark-sugar-ref (fresh-stx stx))) (list (e:var 'f (fresh-stx stx)) arg-var) (fresh-stx stx))]
    [(ty:con t _) #:when (eq? t tname)
     (e:app (e:var 'fmap (mark-sugar-ref (fresh-stx stx))) (list (e:var 'f (fresh-stx stx)) arg-var) (fresh-stx stx))]
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
       (e:app (e:var 'foldr (mark-sugar-ref (fresh-stx stx)))
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
     (e:app (e:var 'pure (mark-sugar-ref (fresh-stx stx))) (list (e:var ctor-name (fresh-stx stx))) (fresh-stx stx))]
    [(= arity 1)
     (e:app (e:var 'fmap (mark-sugar-ref (fresh-stx stx)))
            (list (e:var ctor-name (fresh-stx stx)) (car lifted-fields))
            (fresh-stx stx))]
    [else
     ;; arity ≥ 3: Ctor <$> l0 <*> l1 <*> … <*> l_{N-1}, with Ctor
     ;; curried so each fmap/fapply supplies exactly one field (a bare
     ;; n-ary Ctor would arity-mismatch — liftA2 only reaches arity 2).
     (for/fold ([acc (e:app (e:var 'fmap (mark-sugar-ref (fresh-stx stx)))
                            (list (curried-ctor-lambda ctor-name arity stx)
                                  (car lifted-fields))
                            (fresh-stx stx))])
               ([lf (in-list (cdr lifted-fields))])
       (e:app (e:var 'fapply (mark-sugar-ref (fresh-stx stx))) (list acc lf) (fresh-stx stx)))]))

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
     (e:app (e:var 'traverse (mark-sugar-ref (fresh-stx stx))) (list (e:var 'f (fresh-stx stx)) arg) (fresh-stx stx))]
    [else
     (e:app (e:var 'pure (mark-sugar-ref (fresh-stx stx))) (list arg) (fresh-stx stx))]))

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
     (e:app (e:var 'bimap (mark-sugar-ref (fresh-stx stx)))
            (list (e:var 'f (fresh-stx stx)) (e:var 'g (fresh-stx stx)) arg)
            (fresh-stx stx))]
    [else arg]))

;; ----- Semigroup deriving ----------------------------
;; Single-ctor ADTs only.  Combine fields pairwise via `mappend`.  Qual
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
                (e:app (e:var 'mappend (mark-sugar-ref (fresh-stx stx)))
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
                (list (top:def 'mappend (e:lam '(x y) body stx) stx))
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
                (e:var 'mempty (mark-sugar-ref (fresh-stx stx))))
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
                          (e:app (e:var 'Some (mark-sugar-ref (fresh-stx ctx-stx)))
                                 (list (e:var 'Unit (fresh-stx ctx-stx)))
                                 (fresh-stx ctx-stx))
                          ctx-stx)
                  ;; Always emit a wildcard fallback even when the
                  ;; type has just one ctor — keeps the match
                  ;; well-formed without consulting exhaustiveness.
                  (clause (p:wild ctx-stx) #f
                          (e:var 'None (mark-sugar-ref (fresh-stx ctx-stx)))
                          ctx-stx))
            #f ctx-stx)
           (fresh-stx ctx-stx)))
  (define builder
    (e:lam '(_)
           (e:var ctor-name (fresh-stx ctx-stx))
           (fresh-stx ctx-stx)))
  (top:def lens-name
           (e:app (e:var 'Prism (mark-sugar-ref (fresh-stx ctx-stx)))
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
                          (e:app (e:var 'Some (mark-sugar-ref (fresh-stx ctx-stx)))
                                 (list (e:var 'x (fresh-stx ctx-stx)))
                                 (fresh-stx ctx-stx))
                          ctx-stx)
                  (clause (p:wild ctx-stx) #f
                          (e:var 'None (mark-sugar-ref (fresh-stx ctx-stx)))
                          ctx-stx))
            #f ctx-stx)
           (fresh-stx ctx-stx)))
  ;; Builder is just a reference to the ctor — auto-curry makes
  ;; `(Foo x)` and `Foo` interchangeable for arity-1 ctors.
  (define builder (e:var ctor-name (fresh-stx ctx-stx)))
  (top:def lens-name
           (e:app (e:var 'Prism (mark-sugar-ref (fresh-stx ctx-stx)))
                  (list extractor builder)
                  (fresh-stx ctx-stx))
           ctx-stx))

;; Prism for a multi-field (arity ≥ 2) ctor.  The focus is the FLAT
;; tuple of the fields — a variadic `(tuple …)`, so there is no arity
;; limit (a 2-field focus is a `Pair`, the binary tuple):
;;   preview: (lambda (s) (match s [(C x0 … xn) (Some (tuple x0 … xn))] [_ None]))
;;   review:  (lambda (p) (match p [(tuple x0 … xn) (C x0 … xn)]))
;; review's match is irrefutable — a tuple value always matches its one
;; shape — so exhaustiveness is not consulted.
(define (synth-prism-n-arg tname ctor-name arity all-ctors ctx-stx)
  (define lens-name (string->symbol (format "~a-~a-prism" tname ctor-name)))
  (define vars (for/list ([i (in-range arity)]) (a-name i)))
  (define extractor
    (e:lam '(s)
           (e:match
            (e:var 's (fresh-stx ctx-stx))
            (list (clause (p:ctor ctor-name
                                  (for/list ([v (in-list vars)]) (p:var v ctx-stx))
                                  ctx-stx) #f
                          (e:app (e:var 'Some (mark-sugar-ref (fresh-stx ctx-stx)))
                                 (list (e:tuple
                                        (for/list ([v (in-list vars)]) (e:var v (fresh-stx ctx-stx)))
                                        (fresh-stx ctx-stx)))
                                 (fresh-stx ctx-stx))
                          ctx-stx)
                  (clause (p:wild ctx-stx) #f
                          (e:var 'None (mark-sugar-ref (fresh-stx ctx-stx)))
                          ctx-stx))
            #f ctx-stx)
           (fresh-stx ctx-stx)))
  (define builder
    (e:lam '(p)
           (e:match
            (e:var 'p (fresh-stx ctx-stx))
            (list (clause (p:tuple
                           (for/list ([v (in-list vars)]) (p:var v ctx-stx))
                           ctx-stx) #f
                          (e:app (e:var ctor-name (fresh-stx ctx-stx))
                                 (for/list ([v (in-list vars)]) (e:var v (fresh-stx ctx-stx)))
                                 (fresh-stx ctx-stx))
                          ctx-stx))
            #t ctx-stx)
           (fresh-stx ctx-stx)))
  (top:def lens-name
           (e:app (e:var 'Prism (mark-sugar-ref (fresh-stx ctx-stx)))
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

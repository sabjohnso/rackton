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
         generalize
         instantiate
         current-method-uses
         current-method-resolutions
         current-method-dict-resolutions
         current-dict-skolems
         current-needs-dict-defs
         resolve-method-uses!)

(require racket/match
         racket/set
         racket/list
         racket/string
         "types.rkt"
         "env.rkt"
         "unify.rkt"
         "surface.rkt"
         "entail.rkt"
         "scheme-codec.rkt")

;; ----- fresh type variables -----------------------------------------

(define current-fresh-state    (make-parameter #f))
(define current-pending-preds  (make-parameter #f))
;; Map of alias-name → (cons params target-ty-ast), consulted by
;; `resolve-type` to expand alias references at the type level.
(define current-aliases        (make-parameter (hasheq)))
;; Set of alias names currently being expanded — used to detect
;; recursive aliases.
(define current-expanding      (make-parameter (seteq)))
;; Return-typed-method use sites accumulated during inference of one
;; top-level definition.  A hashtable from the e:var's syntax object →
;; (list class-name method-name body-type).  After constraint
;; reduction completes, each entry's body-type is run through the
;; final substitution to determine the concrete instance and the
;; entry is graduated into `current-method-resolutions`.
(define current-method-uses        (make-parameter #f))
;; Resolved return-typed-method calls.  A hashtable from stx → impl
;; name symbol (e.g. '$pure:Maybe).  Consumed by codegen.
(define current-method-resolutions (make-parameter #f))
;; Resolved dict-method calls.  A hashtable from stx → (Listof
;; impl-name-symbol) — the codegen prepends these to the e:app args
;; when compiling the call site (Phase 20).
(define current-method-dict-resolutions (make-parameter #f))
;; Phase 29: per-needs-dict-def skolem map and dict-arg name table.
;;   current-dict-skolems    : hasheq from skolem-tcon-name → local
;;                              dict-arg-name; set during body
;;                              inference of a needs-dict-body def.
;;   current-needs-dict-defs : hasheq from top-def name → list of
;;                              dict-arg-names; consumed by codegen
;;                              when prepending lambda params.
(define current-dict-skolems    (make-parameter (hasheq)))
(define current-needs-dict-defs (make-parameter #f))

(define (fresh-tvar [prefix 'a])
  (define box (current-fresh-state))
  (unless box
    (error 'fresh-tvar "called outside of inference"))
  (define n (unbox box))
  (set-box! box (add1 n))
  (tvar (string->symbol (format "~a~a" prefix n))))

(define-syntax-rule (with-fresh body ...)
  (parameterize ([current-fresh-state   (box 0)]
                 [current-pending-preds (box '())])
    body ...))

;; ----- constraint accumulator ---------------------------------------

(define (add-preds! ps)
  (define b (current-pending-preds))
  (set-box! b (append ps (unbox b))))

(define (apply-subst-to-preds! s)
  (define b (current-pending-preds))
  (set-box! b (for/list ([p (in-list (unbox b))]) (apply-subst s p))))

(define (snapshot-preds) (unbox (current-pending-preds)))

(define (restore-preds! ps)
  (set-box! (current-pending-preds) ps))

;; Pull every pred whose free type vars share any var with `quantified-set`.
;; Returns the pulled preds; the remaining preds stay in the box.
(define (take-relevant-preds! quantified-set)
  (define b (current-pending-preds))
  (define-values (taken kept)
    (partition (lambda (p)
                 (not (set-empty? (set-intersect (type-vars p) quantified-set))))
               (unbox b)))
  (set-box! b kept)
  taken)

;; ----- generalize / instantiate -------------------------------------

;; Instantiate a scheme.  If the body is a qualified type, the constraints
;; are added to the running pred-box and the bare type is returned.
(define (instantiate sch)
  (define raw
    (match sch
      [(scheme '() body) body]
      [(scheme vs body)
       (define s
         (for/fold ([s empty-subst]) ([v (in-list vs)])
           (subst-extend s v (fresh-tvar v))))
       (apply-subst s body)]))
  (cond
    [(qual? raw)
     (add-preds! (qual-constraints raw))
     (qual-body raw)]
    [else raw]))

;; Like `instantiate` but also returns the substitution it built so
;; callers can recover the fresh tvar that replaced any specific
;; scheme-bound variable.  Used to record both return-typed-method
;; sites (Phase 18) and dict-requiring-method sites (Phase 20).  The
;; scheme body may carry nested quals when a method declared its own
;; qualifying context on top of the class head — both layers of
;; constraints are pulled out into the pending-preds box.
(define (instantiate/subst sch)
  (match sch
    [(scheme vs body)
     (define s
       (for/fold ([s empty-subst]) ([v (in-list vs)])
         (subst-extend s v (fresh-tvar v))))
     (define raw (apply-subst s body))
     (add-preds! (qual-constraints-of raw))
     (values (qual-body-deep raw) s)]))

(define (qual-body-deep t)
  (cond [(qual? t) (qual-body-deep (qual-body t))]
        [else t]))

;; Phase 29 helper: given a list of `(class-name . param-name-list)`
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
  ;;   (cons class-name (list tvar-name ...))     — Phase 29/30 legacy
  ;;   (cons class-name (list pred-arg-type ...)) — Phase 31, multi-param
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
;; var — needed by Phase 29 to map return-typed-method references in
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

(define (improve-by-fds env)
  (let loop ([s empty-subst])
    (define preds (snapshot-preds))
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
      [(equal? s′ s) s]                  ; fixpoint
      [else
       (apply-subst-to-preds! s′)
       (loop s′)])))

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
(define (generalize env ty [hypotheses '()])
  (define fd-sub (improve-by-fds env))
  (define ty* (apply-subst fd-sub ty))
  (define env-fv (env-vars-free-vars env))
  (define ty-fv  (type-vars ty*))
  (define q-set  (set-subtract ty-fv env-fv))
  (define preds  (take-relevant-preds! q-set))
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
  (scheme nice (mqual (for/list ([p (in-list reduced)]) (apply-subst σ p))
                      (apply-subst σ ty*))))

;; For user-facing diagnostic output: rename a type's free tvars to
;; nice sequential names (a, b, c, …) before converting to a datum.
;; Without this, error messages show internal fresh names like `a12`.
(define (pretty-type t)
  (define fv (type-vars t))
  (define vs (sort (set->list fv) symbol<?))
  (define nice (nice-tvar-names (length vs) (seteq)))
  (define σ
    (for/fold ([s empty-subst]) ([old (in-list vs)] [new (in-list nice)])
      (subst-extend s old (tvar new))))
  (type->datum (apply-subst σ t)))

(define (pretty-pred p)
  (define fv (type-vars p))
  (define vs (sort (set->list fv) symbol<?))
  (define nice (nice-tvar-names (length vs) (seteq)))
  (define σ
    (for/fold ([s empty-subst]) ([old (in-list vs)] [new (in-list nice)])
      (subst-extend s old (tvar new))))
  (pred->datum (apply-subst σ p)))

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
    (format "type mismatch\n  expected: ~a\n  got:      ~a"
            (pretty-type expected)
            (pretty-type got))
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
(define (resolve-type ty-ast)
  (match ty-ast
    [(ty:var n _)        (tvar n)]
    [(ty:con n stx)
     (cond
       [(hash-ref (current-aliases) n #f)
        => (lambda (info) (expand-alias n info '() stx))]
       [else (tcon n)])]
    [(ty:app (and h (ty:con n _)) args stx)
     (cond
       [(hash-ref (current-aliases) n #f)
        => (lambda (info) (expand-alias n info args stx))]
       [else (make-tapp (tcon n)
                        (for/list ([a (in-list args)]) (resolve-type a)))])]
    [(ty:app h args _)
     (make-tapp (resolve-type h)
                (for/list ([a (in-list args)]) (resolve-type a)))]
    [(ty:forall _ body _) (resolve-type body)]
    [(ty:qual cs body _)
     (mqual (for/list ([c (in-list cs)]) (resolve-constraint c))
            (resolve-type body))]))

(define (expand-alias name info args stx)
  (when (set-member? (current-expanding) name)
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
  (parameterize ([current-expanding (set-add (current-expanding) name)])
    (resolve-type (substitute-tyvars sub target))))

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

(define (resolve-constraint c)
  (match c
    [(constraint class args _)
     (pred class (for/list ([a (in-list args)]) (resolve-type a)))]))

;; Resolve a parsed type AST as a scheme (for declarations).  A bare
;; `(All ...)` wraps the explicit quantifier; otherwise we generalize
;; over every type variable that appears.
(define (resolve-scheme ty-ast)
  (match ty-ast
    [(ty:forall vs body _)
     (scheme vs (resolve-type body))]
    [_
     (define t (resolve-type ty-ast))
     (define vs (sort (set->list (type-vars t)) symbol<?))
     (scheme vs t)]))

;; ----- literals -----------------------------------------------------

(define (literal-type v)
  (cond
    [(exact-integer? v) t-int]
    [(inexact-real? v)  t-float]
    [(boolean? v)       t-bool]
    [(string? v)        t-string]
    [(char? v)          t-char]
    [(bytes? v)         t-bytes]
    [else (error 'literal-type "unsupported literal: ~e" v)]))

;; ----- core inference ----------------------------------------------

(define (infer-expr/fresh e [env initial-env])
  (with-fresh (infer-expr e env)))

(define (infer-expr e env)
  (match e

    [(e:literal v _) (values empty-subst (literal-type v))]

    [(e:var x stx)
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
           (define-values (t sub) (instantiate/subst sch))
           (define dispatchpos (hash-ref (class-info-dispatchpos cinfo) x #f))
           (cond
             [(eq? dispatchpos 'return)
              (define class-param-tvars
                (for/list ([p (in-list (class-info-params cinfo))])
                  (hash-ref sub p)))
              (record-method-use! x stx env class-param-tvars)]
             [(and (integer? dispatchpos)
                   (or (class-has-needs-dict-instances? env owner-class)
                       (env-class-has-overlap? env owner-class)))
              ;; Phase 27: positional class-method call on a class that
              ;; has at least one needs-dict instance.  Record so the
              ;; resolver can route to a per-instance impl after the
              ;; dispatch arg's type is settled.
              ;; Phase 37 extends this to classes with overlapping
              ;; instances — runtime dispatch can't tell apart
              ;; same-outer-ctor instances, so we resolve at compile
              ;; time when the call site is monomorphic.
              (define class-param-tvars
                (for/list ([p (in-list (class-info-params cinfo))])
                  (hash-ref sub p)))
              (record-inst-dispatch-use! x stx class-param-tvars)]
             [else (void)])
           (define reqs (hash-ref (class-info-dictreqs cinfo) x '()))
           (unless (null? reqs)
             (record-dict-use! x stx reqs sub))
           (values empty-subst t)]
          [else
           ;; A free function may itself be needs-dict: if its scheme's
           ;; qual context includes a constraint over a class with
           ;; return-typed methods (e.g. `mconcat :: (Monoid a) => …`),
           ;; the elaborator inserts the resolved impls at the call
           ;; site, mirroring Phase 20's path for class methods.
           (define free-reqs (var-dict-requirements env sch))
           (cond
             [(null? free-reqs)
              (values empty-subst (instantiate sch))]
             [else
              (define-values (t sub) (instantiate/subst sch))
              (record-dict-use! x stx free-reqs sub)
              (values empty-subst t)])])]
       [else
        (raise-syntax-error 'infer
                            (format "unbound identifier: ~s~a"
                                    x (suggest-similar x env))
                            stx)])]

    [(e:lam params body _)
     (define param-tvars
       (for/list ([_ (in-list params)]) (fresh-tvar)))
     (define env*
       (for/fold ([e env]) ([p (in-list params)] [t (in-list param-tvars)])
         (env-extend-var e p (scheme '() t))))
     (define-values (s body-type) (infer-expr body env*))
     (values s
             (foldr make-arrow body-type
                    (for/list ([t (in-list param-tvars)])
                      (apply-subst s t))))]

    [(e:app head args stx)
     (define-values (s-head t-head) (infer-expr head env))
     ;; Sequentially walk args.  At each step, after inferring the
     ;; arg's type, unify the current head-type's domain with the
     ;; arg's type.  On failure, blame the SPECIFIC arg so the error
     ;; points at the bad token, not the whole call form.
     (define-values (s-final result-type)
       (let loop ([args args]
                  [s s-head]
                  [head-ty t-head]
                  [env (apply-subst/env s-head env)])
         (cond
           [(null? args) (values s head-ty)]
           [else
            (define this-arg (car args))
            (define-values (s-arg t-arg) (infer-expr this-arg env))
            (define s-now (subst-compose s-arg s))
            (define head-ty-now (apply-subst s-now head-ty))
            (define β (fresh-tvar))
            (define expected-arrow (make-arrow t-arg β))
            (define s-u
              (with-handlers
               ([exn:fail:unify?
                 (lambda (_)
                   (cond
                     ;; The head's type is concretely an arrow but the
                     ;; argument type doesn't match — blame the arg.
                     [(arrow? head-ty-now)
                      (raise-type-mismatch!
                        (expr-stx this-arg)
                        (apply-subst s-now (arrow-dom head-ty-now))
                        (apply-subst s-now t-arg))]
                     ;; Otherwise the head itself isn't applicable —
                     ;; blame the head.
                     [else
                      (raise-type-mismatch!
                        (expr-stx head)
                        expected-arrow
                        head-ty-now)]))])
               (unify head-ty-now expected-arrow)))
            (loop (cdr args)
                  (subst-compose s-u s-now)
                  (apply-subst s-u β)
                  (apply-subst/env s-arg env))])))
     (values s-final result-type)]

    [(e:let bindings body _)
     ;; Parallel let: each rhs is typed in the env at let-entry (with
     ;; substitutions threaded), generalized, then made available.
     (define-values (s-acc env-after)
       (for/fold ([s empty-subst] [env-after env])
                 ([b (in-list bindings)])
         (define-values (s′ t)
           (infer-expr (cdr b) (apply-subst/env s env)))
         (define s-combined (subst-compose s′ s))
         (apply-subst-to-preds! s-combined)
         (define env-for-gen (apply-subst/env s-combined env))
         (define sch (generalize env-for-gen (apply-subst s-combined t)))
         (values s-combined
                 (env-extend-var (apply-subst/env s-combined env-after)
                                 (car b) sch))))
     (define-values (s-body t-body)
       (infer-expr body env-after))
     (values (subst-compose s-body s-acc) t-body)]

    [(e:letrec bindings body _)
     ;; Mutual recursion: pre-bind each name with a fresh monomorphic
     ;; tvar so each rhs can reference every other binding (and itself).
     ;; After inferring all rhs's, unify each tvar with the inferred
     ;; type and generalize against the OUTER env's free-var set.
     (define pre-bindings
       (for/list ([b (in-list bindings)]) (cons (car b) (fresh-tvar))))
     (define env-with-pre
       (for/fold ([e env]) ([pb (in-list pre-bindings)])
         (env-extend-var e (car pb) (scheme '() (cdr pb)))))
     (define-values (s-final ts)
       (for/fold ([s empty-subst] [ts '()])
                 ([b  (in-list bindings)]
                  [pb (in-list pre-bindings)])
         (define-values (s′ t)
           (infer-expr (cdr b) (apply-subst/env s env-with-pre)))
         (define s-combined (subst-compose s′ s))
         (define s-u
           (unify (apply-subst s-combined (cdr pb))
                  (apply-subst s-combined t)))
         (define s-after (subst-compose s-u s-combined))
         (values s-after (cons (apply-subst s-after (cdr pb)) ts))))
     (apply-subst-to-preds! s-final)
     (define env-after
       (for/fold ([e (apply-subst/env s-final env)])
                 ([b  (in-list bindings)]
                  [t  (in-list (reverse ts))])
         (env-extend-var e (car b)
                         (generalize (apply-subst/env s-final env) t))))
     (define-values (s-body t-body) (infer-expr body env-after))
     (values (subst-compose s-body s-final) t-body)]

    [(e:if c t e stx)
     (define-values (s-c t-c) (infer-expr c env))
     (define s-cb
       (with-handlers
        ([exn:fail:unify?
          (lambda (_)
            (raise-type-mismatch!
              (expr-stx c)
              t-bool
              (apply-subst s-c t-c)))])
        (unify (apply-subst s-c t-c) t-bool)))
     (define s1 (subst-compose s-cb s-c))
     (define-values (s-then t-then)
       (infer-expr t (apply-subst/env s1 env)))
     (define s2 (subst-compose s-then s1))
     (define-values (s-else t-else)
       (infer-expr e (apply-subst/env s2 env)))
     (define s3 (subst-compose s-else s2))
     (define s-branches
       (with-handlers
        ([exn:fail:unify?
          (lambda (_)
            (raise-type-mismatch!
              (expr-stx e)
              (apply-subst s3 t-then)
              (apply-subst s3 t-else)))])
        (unify (apply-subst s3 t-then) (apply-subst s3 t-else))))
     (define s-final (subst-compose s-branches s3))
     (values s-final (apply-subst s-final t-then))]

    [(e:ann expr ty-ast stx)
     (define-values (s-e t-e) (infer-expr expr env))
     (define declared (qual-body-type (resolve-type ty-ast)))
     (define s-u
       (with-handlers
        ([exn:fail:unify?
          (lambda (_)
            (raise-type-mismatch!
              (expr-stx expr)
              declared
              (apply-subst s-e t-e)))])
        (unify (apply-subst s-e t-e) declared)))
     (values (subst-compose s-u s-e) (apply-subst s-u declared))]

    [(e:escape ty-ast vars _ stx)
     (define expected (qual-body-type (resolve-type ty-ast)))
     (for ([v (in-list vars)])
       (unless (env-ref-var env v)
         (raise-syntax-error 'infer
           (format "(racket …) escape references unbound name: ~s" v)
           stx)))
     (values empty-subst expected)]

    [(e:match scrut clauses irrefutable? stx)
     (define-values (s-scrut t-scrut) (infer-expr scrut env))
     (define result-tv (fresh-tvar))
     (define-values (s-final _)
       (for/fold ([s s-scrut] [_ignored result-tv])
                 ([cl (in-list clauses)])
         (define-values (s-cl _t)
           (infer-clause cl
                         (apply-subst s t-scrut)
                         (apply-subst s result-tv)
                         (apply-subst/env s env)))
         (values (subst-compose s-cl s) _t)))
     ;; A match-let-style irrefutable destructure skips the
     ;; exhaustiveness check — the user has asserted the pattern fits.
     (unless irrefutable?
       (check-exhaustive! (apply-subst s-final t-scrut) clauses stx env))
     (values s-final (apply-subst s-final result-tv))]))

;; Type a single match clause.  Pattern bindings extend the env for the
;; clause body.  The body type is unified with the running result type
;; so every arm yields the same type.
(define (infer-clause cl scrut-type result-type env)
  (define-values (bindings pat-type) (infer-pattern (clause-pattern cl) env))
  (define s-pat
    (with-handlers
     ([exn:fail:unify?
       (lambda (_)
         (raise-syntax-error 'infer
           (format "pattern type ~a does not match scrutinee type ~a"
                   (pretty-type pat-type) (pretty-type scrut-type))
           (clause-stx cl)))])
     (unify pat-type scrut-type)))
  (define env*
    (for/fold ([e (apply-subst/env s-pat env)])
              ([b (in-list bindings)])
      (env-extend-var e (car b)
                      (scheme '() (apply-subst s-pat (cdr b))))))
  ;; A pattern guard, when present, is typechecked under the pattern
  ;; bindings and must produce a Boolean.  Thread its substitution
  ;; into the running chain so any tvars it pins (e.g. via uses of
  ;; class methods) are visible to the body and the surrounding
  ;; constraint-reduction pass.
  (define-values (s-pre-body env-pre-body)
    (cond
      [(clause-guard cl)
       (define-values (s-g t-g) (infer-expr (clause-guard cl) env*))
       (define s-u
         (with-handlers
          ([exn:fail:unify?
            (lambda (_)
              (raise-syntax-error 'infer
                (format "pattern guard must be Boolean, got ~a"
                        (pretty-type (apply-subst s-g t-g)))
                (clause-stx cl)))])
          (unify (apply-subst s-g t-g) t-bool)))
       (define s* (subst-compose s-u (subst-compose s-g s-pat)))
       (values s* (apply-subst/env s* env*))]
      [else (values s-pat env*)]))
  (define-values (s-body t-body) (infer-expr (clause-body cl) env-pre-body))
  (define s-acc (subst-compose s-body s-pre-body))
  (define s-u
    (with-handlers
     ([exn:fail:unify?
       (lambda (_)
         (raise-syntax-error 'infer
           (format "match clause body has type ~a but earlier arms have ~a"
                   (pretty-type (apply-subst s-acc t-body))
                   (pretty-type (apply-subst s-acc result-type)))
           (clause-stx cl)))])
     (unify (apply-subst s-acc t-body)
            (apply-subst s-acc result-type))))
  (values (subst-compose s-u s-acc)
          (apply-subst s-u t-body)))

;; Type a pattern.  Returns (bindings, pattern-type).
(define (infer-pattern pat env)
  (match pat
    [(p:wild _)   (values '() (fresh-tvar))]
    [(p:lit v _)  (values '() (literal-type v))]
    [(p:var x _)
     (define α (fresh-tvar))
     (values (list (cons x α)) α)]
    [(p:ctor name args stx)
     (define info (env-ref-data env name))
     (cond
       [(not info)
        (raise-syntax-error 'infer
          (format "unknown data constructor: ~s~a"
                  name (suggest-similar name env))
          stx)]
       [(not (= (length args) (data-info-arity info)))
        (raise-syntax-error 'infer
          (format "constructor ~s expects ~a arg(s), pattern has ~a"
                  name (data-info-arity info) (length args))
          stx)]
       [else
        (define ctor-type (instantiate (data-info-scheme info)))
        (define-values (arg-tys result-ty)
          (unfold-arrow ctor-type (length args)))
        (define-values (all-bindings s-acc)
          (for/fold ([acc '()] [s empty-subst])
                    ([arg-pat (in-list args)]
                     [exp-ty (in-list arg-tys)])
            (define-values (bs t) (infer-pattern arg-pat env))
            (define s-u (unify (apply-subst s t)
                               (apply-subst s exp-ty)))
            (values (append acc bs) (subst-compose s-u s))))
        (values (for/list ([b (in-list all-bindings)])
                  (cons (car b) (apply-subst s-acc (cdr b))))
                (apply-subst s-acc result-ty))])]))

;; Compile-time exhaustiveness check for `match`.  Tabular cases:
;;   - any wildcard or variable pattern is a universal catchall.
;;   - on a known ADT, every declared constructor must appear (unless
;;     a catchall is present).
;;   - on Boolean, both #t and #f literals must appear.
;;   - on other primitive scrutinee types (Integer, String, …) and on
;;     scrutinees whose type is still polymorphic, a catchall is required.
(define (check-exhaustive! scrut-type clauses stx env)
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
    [has-catchall? (void)]
    [(eq? head 'Boolean)
     (define hits
       (for/fold ([acc '()]) ([c (in-list clauses)] #:unless (clause-guard c))
         (match (clause-pattern c)
           [(p:lit v _) (cons v acc)]
           [_ acc])))
     (unless (and (member #t hits) (member #f hits))
       (raise-syntax-error 'infer
         "non-exhaustive match on Boolean — both #t and #f must be covered"
         stx))]
    [head
     (define ti (env-ref-tcon env head))
     (cond
       [(not ti)
        (raise-syntax-error 'infer
          "non-exhaustive match: needs a wildcard or variable pattern"
          stx)]
       [else
        (define needed (tcon-info-ctors ti))
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
            stx))])]
    [else
     (raise-syntax-error 'infer
       "non-exhaustive match: needs a wildcard or variable pattern"
       stx)]))

(define (unfold-arrow t n)
  (let loop ([t t] [n n] [acc '()])
    (cond
      [(zero? n) (values (reverse acc) t)]
      [(arrow? t) (loop (arrow-cod t) (sub1 n) (cons (arrow-dom t) acc))]
      [else (error 'unfold-arrow
                   "expected ~a more arrow(s) in ~v but ran out" n t)])))

;; ----- top-level forms ----------------------------------------------

(define (infer-program forms [env initial-env])
  (with-fresh
   (let loop ([forms forms] [env env] [declared (hasheq)])
     (cond
       [(null? forms) env]
       [else
        (define-values (env* declared*)
          (handle-top-form (car forms) env declared))
        (loop (cdr forms) env* declared*)]))))

(define (handle-top-form form env declared)
  (parameterize ([current-aliases (env-aliases env)])
   (match form

    [(top:alias name params target-ast stx)
     (values (env-extend-alias env name params target-ast) declared)]

    [(top:dec name ty-ast _)
     (define sch (resolve-scheme ty-ast))
     ;; Pre-register the name in env with its declared scheme so that
     ;; subsequent top-level forms can forward-reference it.  When the
     ;; matching define is processed later, the binding is replaced.
     (values (env-extend-var env name sch)
             (hash-set declared name sch))]

    [(top:def name expr stx)
     (cond
       [(hash-has-key? declared name)
        (define decl-scheme (hash-ref declared name))
        ;; Phase 29: detect needs-dict-body — a def whose qual context
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
        (when (and (current-needs-dict-defs) (not (null? dict-arg-names)))
          (hash-set! (current-needs-dict-defs) name dict-arg-names))
        ;; The dict-skolem map must be visible to BOTH the body
        ;; inference AND the post-reduction `resolve-method-uses!`
        ;; pass that follows below; mutate the parameter directly
        ;; for the whole branch instead of wrapping just infer-expr.
        (define saved-skolems (current-dict-skolems))
        (current-dict-skolems dict-skolems)
        (define-values (s t) (infer-expr expr env-rec))
        (define s-u
          (with-handlers
           ([exn:fail:unify?
             (lambda (_)
               (raise-syntax-error 'infer
                 (format "definition of ~s has type ~a, declared as ~a"
                         name
                         (pretty-type (apply-subst s t))
                         (scheme->datum decl-scheme))
                 stx))])
           (unify (apply-subst s t) decl-ty)))
        ;; Discharge any constraints raised inside the body against the
        ;; declaration's preds (hypotheses).
        (define final-subst (subst-compose s-u s))
        (apply-subst-to-preds! final-subst)
        (define remaining-preds
          (reduce-context env decl-preds (snapshot-preds)))
        (cond
          [(not (null? remaining-preds))
           (raise-syntax-error 'infer
             (format "unsolved constraints in ~s: ~s"
                     name
                     (map pretty-pred remaining-preds))
             stx)]
          [else (restore-preds! '())])
        (resolve-method-uses! final-subst env)
        (current-dict-skolems saved-skolems)
        (values (env-extend-var env name decl-scheme)
                (hash-remove declared name))]
       [else
        ;; No declaration: pre-register fresh tvar for recursive use,
        ;; infer, unify, generalize.
        (define α (fresh-tvar))
        (define env-rec (env-extend-var env name (scheme '() α)))
        (define-values (s t) (infer-expr expr env-rec))
        (define s-rec (unify (apply-subst s α) (apply-subst s t)))
        (define s* (subst-compose s-rec s))
        (apply-subst-to-preds! s*)
        (define final-ty (apply-subst s* t))
        (define final-env (apply-subst/env s* env))
        (define generalized (generalize final-env final-ty))
        (resolve-method-uses! s* env)
        (values (env-extend-var final-env name generalized)
                declared)])]

    [(top:class supers head methods stx)
     (handle-class-form supers head methods stx env declared)]

    [(top:instance ctx head methods stx)
     (handle-instance-form ctx head methods stx env declared)]

    [(top:require specs stx)
     (handle-require-form specs stx env declared)]

    [(top:data tname tparams ctors stx)
     (define result-type
       (make-tapp (tcon tname)
                  (for/list ([p (in-list tparams)]) (tvar p))))
     (define env*
       (env-extend-tcon env tname
                        (tcon-info tname (length tparams)
                                   (for/list ([c (in-list ctors)])
                                     (data-ctor-name c)))))
     (define env**
       (for/fold ([e env*]) ([c (in-list ctors)])
         (define field-tys
           (for/list ([t (in-list (data-ctor-field-types c))])
             (resolve-type t)))
         (define ctor-fn-type
           (foldr make-arrow result-type field-tys))
         (define sch (scheme tparams ctor-fn-type))
         (env-extend-data e (data-ctor-name c)
                          (data-info tname (data-ctor-name c)
                                     (length field-tys) sch))))
     (values env** declared)])))

;; ----- class / instance elaboration --------------------------------

(define (handle-class-form supers head methods stx env declared)
  (define class-name (constraint-class head))
  ;; The head's args may be plain ty:var nodes or kind-annotated
  ;; ty:vars (parser stashes the kind on the stx as 'rackton:kind).
  (define class-args
    (for/list ([a (in-list (constraint-args head))]) (resolve-type a)))
  (define class-params
    (for/list ([a (in-list class-args)])
      (match a
        [(tvar n) n]
        [_ (raise-syntax-error 'infer
              "class head arguments must be (kind-annotated) type variables"
              stx)])))
  (define class-kinds
    (for/fold ([acc (hasheq)])
              ([raw (in-list (constraint-args head))]
               [name (in-list class-params)])
      (match raw
        [(ty:var _ var-stx)
         (define surface-kind
           (and (syntax? var-stx)
                (syntax-property var-stx 'rackton:kind)))
         (hash-set acc name (surface-kind->core
                             (or surface-kind (k:star))))]
        [_ (hash-set acc name (kind-star))])))
  (define super-preds
    (for/list ([s (in-list supers)]) (resolve-constraint s)))
  (define head-pred (pred class-name class-args))
  (define method-schemes
    (for/fold ([acc (hasheq)]) ([m (in-list methods)])
      (cond
        [(method-sig? m)
         (define raw (resolve-type (method-sig-type m)))
         (define body (mqual (list head-pred) raw))
         ;; Quantify over EVERY type variable that appears in the
         ;; method type (including method-local ones like `a`/`b`
         ;; in `(>>= : (m a) -> (a -> m b) -> m b)`).  Class params
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
      (define body-type (qual-body-type (scheme-body sch)))
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
  ;; the docstring on class-info in env.rkt and Phase 20's plan.
  (define dictreqs
    (for/fold ([acc (hasheq)])
              ([(method-name sch) (in-hash method-schemes)])
      (define reqs (method-dict-requirements sch class-params))
      (cond
        [(null? reqs) acc]
        [else (hash-set acc method-name reqs)])))
  (define info (class-info class-name class-params class-kinds
                           super-preds method-schemes defaults
                           dispatchpos fundeps dictreqs))
  ;; Phase 37: when a class is redeclared, its previously-registered
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
         (with-handlers ([exn:fail? (lambda (_) e)])
           (define bindings
             (dynamic-require submod-spec 'rackton-bindings))
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
             (env-extend-instance acc (car decoded) (cdr decoded))))])))
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
(define (find-dispatch-pos t class-params [fundeps '()])
  (define determined
    (for/fold ([acc (seteq)]) ([fd (in-list fundeps)])
      (set-union acc (list->seteq (cdr fd)))))
  (let loop ([t t] [pos 0])
    (cond
      [(arrow? t)
       (define dom (arrow-dom t))
       (define mentions
         (filter (lambda (p) (set-member? (type-vars dom) p)) class-params))
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

;; Stash a Phase-18 return-typed-method entry under `stx`.  The
;; entry shape is `(list 'return method-name class-param-tvars)`.
(define (record-method-use! method-name stx env class-param-tvars)
  (define table (current-method-uses))
  (when table
    (hash-set! table stx (list 'return
                               method-name
                               class-param-tvars))))

;; Phase 27: a positional class-method call on a class that has at
;; least one needs-dict instance.  Entry shape:
;;   (list 'inst-dispatch method-name class-param-tvars)
;; After the enclosing def's constraints reduce, the resolver applies
;; the final substitution, looks up the matching instance, and — if
;; that instance has dict-bearing constraints in its qual — writes
;; the per-instance impl name into current-method-resolutions and the
;; dict impls into current-method-dict-resolutions.
(define (record-inst-dispatch-use! method-name stx class-param-tvars)
  (define table (current-method-uses))
  (when table
    (hash-set! table stx (list 'inst-dispatch method-name class-param-tvars))))

;; Does this class have at least one instance whose qual context
;; mentions a return-typed-bearing class?  Used to decide whether to
;; record positional class-method call sites for Phase 27 dispatch.
(define (class-has-needs-dict-instances? env class-name)
  (for/or ([inst (in-list (env-instances env class-name))])
    (instance-needs-dict? env inst)))

(define (instance-needs-dict? env inst)
  (for/or ([c (in-list (instance-info-context inst))])
    (class-has-return-typed-methods? env (pred-class c))))

;; Stash a Phase-20 dict-requiring-method entry under `stx`.  The
;; entry shape is `(list 'dict method-name dict-entries)` where each
;; dict-entry is `(cons class-name tvars)` — the class to look up and
;; the fresh tvars whose final resolution names the impl.
(define (record-dict-use! method-name stx reqs sub)
  (define table (current-method-uses))
  (when table
    (define dict-entries
      (for/list ([req (in-list reqs)])
        (define cls (car req))
        (define arg-types (cdr req))
        (cons cls (for/list ([a (in-list arg-types)])
                    (apply-subst sub a)))))
    (hash-set! table stx (list 'dict method-name dict-entries))))

;; After a top-level def's constraints have been reduced, walk every
;; recorded method use, apply `final-subst` to each entry's tvars,
;; extract resulting type-constructor names, and graduate the entry
;; into the appropriate resolution table:
;;   * `'return` entries land in `current-method-resolutions` as a
;;     single impl-name symbol the codegen substitutes for the e:var.
;;   * `'dict`   entries land in `current-method-dict-resolutions` as
;;     a list of impl-name symbols the codegen prepends to e:app args.
(define (resolve-method-uses! final-subst env)
  (define uses (current-method-uses))
  (define resolutions (current-method-resolutions))
  (define dict-resolutions (current-method-dict-resolutions))
  (when (and uses resolutions)
    (for ([(stx entry) (in-hash uses)])
      (match entry
        [(list 'return method-name class-param-tvars)
         (define impl (resolve-return-impl method-name class-param-tvars
                                           final-subst stx env))
         (hash-set! resolutions stx impl)
         ;; Phase 25.3: when the matching instance carries a qual
         ;; context with return-typed-method-bearing constraints, the
         ;; impl needs those impls as dict args.  Compute and store
         ;; alongside (codegen prepends them at e:app).
         (define inst-dict-impls
           (instance-qual-return-impls env method-name
                                       class-param-tvars final-subst stx))
         (unless (null? inst-dict-impls)
           (hash-set! dict-resolutions stx inst-dict-impls))]
        [(list 'dict method-name dict-entries)
         ;; For each constraint, expand into (method . arg-types) pairs
         ;; following the class's superclass hierarchy so inherited
         ;; methods (e.g. `pure` reached from MonadState via Monad) see
         ;; only their own class's arg slots.  Each impl is wrapped with
         ;; its instance-qual dicts (Phase 31) so needs-dict transformer
         ;; instances pre-apply their inner-monad dicts at the call site.
         ;; Pairs are deduped across constraints (e.g. MonadState +
         ;; MonadEnv both reaching Monad.pure) — matches
         ;; build-dict-skolems' (skolem.method) dedup used to size the
         ;; needs-dict lambda's params.  Pairs whose class-params are
         ;; fully concrete (after fundep filter) at this call site are
         ;; dropped: the function's body resolves those references to
         ;; per-type globals (e.g. `$mempty:String`) directly, with no
         ;; corresponding dict-arg slot.
         (define all-pairs
           (apply append
                  (for/list ([entry (in-list dict-entries)])
                    (collect-dict-method-args (car entry) (cdr entry) env))))
         (define dedup-pairs
           (let loop ([ps all-pairs] [seen (seteq)] [acc '()])
             (cond
               [(null? ps) (reverse acc)]
               [(set-member? seen (car (car ps)))
                (loop (cdr ps) seen acc)]
               [else
                (loop (cdr ps)
                      (set-add seen (car (car ps)))
                      (cons (car ps) acc))])))
         (define active-pairs
           (filter (lambda (pair)
                     (pair-has-tvar-at-undetermined-position? pair env))
                   dedup-pairs))
         (define impls
           (for/list ([pair (in-list active-pairs)])
             (resolve-impl-with-quals (car pair) (cdr pair)
                                      final-subst stx env)))
         (hash-set! dict-resolutions stx impls)]
        [(list 'inst-dispatch method-name class-param-tvars)
         ;; Phase 27: route a class-method call to a per-instance
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
           ;; Phase 32: filter tcon-names by fundep-determined params
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
                (string->symbol
                 (format "$~a:~a"
                         method-name
                         (string-join (map symbol->string keep-tcon-names)
                                      "-"))))
              (hash-set! resolutions stx impl)
              (define inst-dict-impls
                (instance-qual-return-impls env method-name
                                            class-param-tvars final-subst stx))
              (unless (null? inst-dict-impls)
                (hash-set! dict-resolutions stx inst-dict-impls))]
             ;; Phase 37: for overlap-group classes, emit a deep-
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
              (hash-set! resolutions stx impl)]))]))
    (hash-clear! uses)))

;; Phase 37: must agree byte-for-byte with overlap-impl-symbol in
;; codegen.rkt — encodes nested ctors deeply so two same-outer-ctor
;; overlap-group instances get distinct impl names.
(define (overlap-impl-symbol method-name head-arg-types)
  (string->symbol
   (format "$~a:~a"
           method-name
           (apply string-append
                  (let loop ([ts head-arg-types])
                    (cond
                      [(null? ts) '()]
                      [(null? (cdr ts)) (list (head-fingerprint (car ts)))]
                      [else (cons (head-fingerprint (car ts))
                                  (cons "-" (loop (cdr ts))))]))))))

(define (head-fingerprint t)
  (match t
    [(tcon n) (symbol->string n)]
    [(tvar _) "*"]
    [(tapp h args)
     (string-append (head-fingerprint h)
                    (apply string-append
                           (for/list ([a (in-list args)])
                             (string-append "_" (head-fingerprint a)))))]
    [_ "*"]))

;; Phase 32: predicate matching build-dict-skolems' filter-skolems —
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

;; Phase 31: resolve a method-name + arg-types to either a bare impl
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
                   [else '()])))])]))

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
     ;; Phase 29: if a class-param resolves to a tracked skolem,
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
        (string->symbol
         (format "$~a:~a"
                 method-name
                 (string-join (map symbol->string keep-tcon-names) "-")))])]
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

;; Phase 37: a pre-existing instance is a "true duplicate" only if
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

(define (handle-instance-form ctx head methods stx env declared)
  (define head-pred-raw (resolve-constraint head))
  (define class-name (pred-class head-pred-raw))
  (define cinfo (env-ref-class env class-name))
  (unless cinfo
    (raise-syntax-error 'infer
      (format "unknown class: ~s~a"
              class-name (suggest-similar class-name env 'class))
      stx))
  ;; Phase 37: reject duplicate instance registrations (heads
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
      (when (instance-matches-class-shape? existing cinfo)
        (raise-syntax-error 'infer
          (format "duplicate instance: ~s already declared"
                  (pretty-pred head-pred-raw))
          stx))))
  (define inst-args-raw (pred-args head-pred-raw))
  (define ctx-preds-raw (for/list ([c (in-list ctx)]) (resolve-constraint c)))
  ;; Phase 30: if the qual context introduces tvars that pin a
  ;; return-typed-bearing class (e.g. `(HasUnit m) =>` on a lifted
  ;; instance), skolemize those tvars and build a map from each
  ;; skolem to a local dict-arg name.  The instance body's polymorphic
  ;; class-method references will resolve against this map.
  (define inst-needs-dict-reqs
    (for/list ([c (in-list ctx-preds-raw)]
               #:when (class-has-return-typed-methods? env (pred-class c)))
      (cons (pred-class c) (pred-args c))))
  (define-values (sk-subst dict-skolems dict-arg-names)
    (cond
      [(null? inst-needs-dict-reqs) (values empty-subst (hasheq) '())]
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
  ;; Apply the instance-qual skolem substitution into the parts that
  ;; flow into body inference.
  ;; The skolemized versions are used only for body-checking
  ;; hypotheses; the env entry uses the original (un-skolemized) head
  ;; and ctx so other constraints can match against this instance.
  (define inst-args-sk (map (lambda (a) (apply-subst sk-subst a)) inst-args-raw))
  (define head-pred-sk (apply-subst sk-subst head-pred-raw))
  (define ctx-preds-sk (map (lambda (p) (apply-subst sk-subst p)) ctx-preds-raw))
  ;; Stash the dict-arg names so compile-instance can find them when
  ;; emitting the per-instance impl defs.  Key by (class-name, head
  ;; tcon, method-name) — see compile-instance for the lookup.
  (when (and (current-needs-dict-defs) (not (null? dict-arg-names)))
    (define head-tcon (type-head-tcon (car inst-args-raw)))
    (for ([m (in-hash-keys (class-info-methods cinfo))])
      (hash-set! (current-needs-dict-defs)
                 (list class-name head-tcon m)
                 dict-arg-names)))
  (define user-impls
    (for/fold ([acc (hasheq)]) ([m (in-list methods)])
      (match m
        [(top:def name expr _) (hash-set acc name expr)])))
  (define checked-bodies
    (for/fold ([acc (hasheq)])
              ([(method-name method-sch)
                (in-hash (class-info-methods cinfo))])
      (define body
        (cond
          [(hash-ref user-impls method-name #f)]
          [(hash-ref (class-info-defaults cinfo) method-name #f)]
          [else
           (raise-syntax-error 'infer
             (format "instance ~s missing method ~s with no default"
                     (pred->datum head-pred-raw) method-name)
             stx)]))
      ;; Substitute class params → instance args (skolemized version)
      ;; in the method's body type.  Using the skolemized inst args is
      ;; what allows the body's polymorphic class-method references to
      ;; resolve to local dict-arg names via current-dict-skolems.
      (define σ
        (for/fold ([s empty-subst])
                  ([p (in-list (class-info-params cinfo))]
                   [a (in-list inst-args-sk)])
          (subst-extend s p a)))
      (define inst-method-qual (apply-subst σ (scheme-body method-sch)))
      ;; Strip ALL qual layers — a method type may carry its own
      ;; qualifying context on top of the class head (e.g.
      ;; `traverse :: (Applicative f) => ...`), which leaves nested
      ;; quals after substitution.  We need the bare body for
      ;; unification and the full constraint list as hypotheses.
      (define expected-type (qual-body-deep inst-method-qual))
      (define method-extra-preds
        ;; Constraints from the method's own qualifying context only —
        ;; drop the class's head pred since it appears separately as
        ;; `head-pred` below.
        (filter (lambda (p) (not (equal? p head-pred-sk)))
                (qual-constraints-of inst-method-qual)))
      (parameterize ([current-pending-preds (box '())])
        ;; Make the instance-qual skolem map visible while inferring
        ;; the body AND while resolve-method-uses! runs afterward —
        ;; mirroring Phase 29's mutate-then-restore pattern for
        ;; top-defs.
        (define saved-skolems (current-dict-skolems))
        (current-dict-skolems dict-skolems)
        (define-values (s t) (infer-expr body env))
        (define s-u
          (with-handlers
           ([exn:fail:unify?
             (lambda (_)
               (raise-syntax-error 'infer
                 (format "method ~s body has type ~a, expected ~a"
                         method-name
                         (type->datum (apply-subst s t))
                         (type->datum expected-type))
                 stx))])
           (unify (apply-subst s t) expected-type)))
        (define final-subst (subst-compose s-u s))
        (apply-subst-to-preds! final-subst)
        ;; The instance head itself is a hypothesis during method
        ;; checking — plus any constraints from the method's own
        ;; qualifying context (e.g. `Applicative f` for traverse).
        (define leftovers
          (reduce-context env
                          (append (cons head-pred-sk ctx-preds-sk)
                                  method-extra-preds)
                          (snapshot-preds)))
        (unless (null? leftovers)
          (raise-syntax-error 'infer
            (format "instance ~s method ~s leaves unsolved constraints: ~s"
                    (pretty-pred head-pred-raw) method-name
                    (map pretty-pred leftovers))
            stx))
        (resolve-method-uses! final-subst env)
        (current-dict-skolems saved-skolems))
      (hash-set acc method-name body)))
  (define info (instance-info head-pred-raw ctx-preds-raw checked-bodies))
  (values (env-extend-instance env class-name info) declared))

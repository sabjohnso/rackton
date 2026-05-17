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
         instantiate)

(require racket/match
         racket/set
         racket/list
         "types.rkt"
         "env.rkt"
         "unify.rkt"
         "surface.rkt"
         "entail.rkt"
         "scheme-codec.rkt")

;; ----- fresh type variables -----------------------------------------

(define current-fresh-state    (make-parameter #f))
(define current-pending-preds  (make-parameter #f))

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

;; Generalize: take the type's quantifiable tvars, pull the constraints
;; that mention them out of the pred-box, reduce them against the env,
;; and wrap into a `(scheme vs (qual cs ty))`.  Bound tvars are renamed
;; to nice sequential names (a, b, c, …) for readability.
(define (generalize env ty [hypotheses '()])
  (define env-fv (env-vars-free-vars env))
  (define ty-fv  (type-vars ty))
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
                      (apply-subst σ ty))))

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
;; preserved only by `resolve-scheme`.
(define (resolve-type ty-ast)
  (match ty-ast
    [(ty:var n _)        (tvar n)]
    [(ty:con n _)        (tcon n)]
    [(ty:app h args _)
     (make-tapp (resolve-type h)
                (for/list ([a (in-list args)]) (resolve-type a)))]
    [(ty:forall _ body _) (resolve-type body)]
    [(ty:qual cs body _)
     (mqual (for/list ([c (in-list cs)]) (resolve-constraint c))
            (resolve-type body))]))

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
    [(boolean? v)       t-bool]
    [(string? v)        t-string]
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
       [sch (values empty-subst (instantiate sch))]
       [else
        (raise-syntax-error 'infer
                            (format "unbound identifier: ~s" x)
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
     (define-values (s-args ts-args)
       (let loop ([args args] [s empty-subst] [env (apply-subst/env s-head env)]
                  [acc '()])
         (cond
           [(null? args) (values s (reverse acc))]
           [else
            (define-values (s′ t) (infer-expr (car args) env))
            (loop (cdr args)
                  (subst-compose s′ s)
                  (apply-subst/env s′ env)
                  (cons t acc))])))
     (define s-pre (subst-compose s-args s-head))
     (define α (fresh-tvar))
     (define expected
       (foldr make-arrow α
              (for/list ([t (in-list ts-args)])
                (apply-subst s-args t))))
     (define actual (apply-subst s-args t-head))
     (define s-u
       (with-handlers
        ([exn:fail:unify?
          (lambda (e)
            (raise-syntax-error 'infer
              (format "type mismatch in application: cannot apply ~a to ~a"
                      (pretty-type actual)
                      (for/list ([t ts-args]) (pretty-type (apply-subst s-args t))))
              stx))])
        (unify actual expected)))
     (values (subst-compose s-u s-pre)
             (apply-subst s-u α))]

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

    [(e:if c t e stx)
     (define-values (s-c t-c) (infer-expr c env))
     (define s-cb
       (with-handlers
        ([exn:fail:unify?
          (lambda (_)
            (raise-syntax-error 'infer
              (format "if-condition must be Boolean, got ~a"
                      (pretty-type (apply-subst s-c t-c)))
              stx))])
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
            (raise-syntax-error 'infer
              (format "if branches disagree: then ~a vs else ~a"
                      (pretty-type (apply-subst s3 t-then))
                      (pretty-type (apply-subst s3 t-else)))
              stx))])
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
            (raise-syntax-error 'infer
              (format "expression has type ~a but ascription says ~a"
                      (pretty-type (apply-subst s-e t-e))
                      (pretty-type declared))
              stx))])
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

    [(e:match scrut clauses stx)
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
     (check-exhaustive! (apply-subst s-final t-scrut) clauses stx env)
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
  (define-values (s-body t-body) (infer-expr (clause-body cl) env*))
  (define s-acc (subst-compose s-body s-pat))
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
          (format "unknown data constructor: ~s" name) stx)]
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
    (or (p:wild? (clause-pattern c))
        (p:var?  (clause-pattern c))))
  (define has-catchall? (for/or ([c (in-list clauses)]) (catchall? c)))
  (cond
    [has-catchall? (void)]
    [(eq? head 'Boolean)
     (define hits
       (for/fold ([acc '()]) ([c (in-list clauses)])
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
          (for/fold ([acc '()]) ([c (in-list clauses)])
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
  (match form

    [(top:dec name ty-ast _)
     (values env (hash-set declared name (resolve-scheme ty-ast)))]

    [(top:def name expr stx)
     (cond
       [(hash-has-key? declared name)
        (define decl-scheme (hash-ref declared name))
        ;; Skolemize bound vars so the body cannot covertly specialize them.
        (define-values (decl-ty decl-preds) (skolemize decl-scheme))
        (define env-rec (env-extend-var env name (scheme '() decl-ty)))
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
        (apply-subst-to-preds! (subst-compose s-u s))
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
        (values (env-extend-var final-env name (generalize final-env final-ty))
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
     (values env** declared)]))

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
         (define sch (scheme class-params body))
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
  ;; parameter.
  (define dispatchpos
    (for/fold ([acc (hasheq)])
              ([(method-name sch) (in-hash method-schemes)])
      (define body-type (qual-body-type (scheme-body sch)))
      (define pos (find-dispatch-pos body-type class-params))
      (cond
        [pos (hash-set acc method-name pos)]
        [else
         (raise-syntax-error 'infer
           (format "class method ~s does not have any argument whose type mentions a class parameter — single dispatch cannot resolve it"
                   method-name)
           stx)])))
  (define info (class-info class-name class-params class-kinds
                           super-preds method-schemes defaults
                           dispatchpos))
  (values (env-extend-class env class-name info) declared))

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

;; Walk the arrow chain of a method type and return the position of the
;; first argument whose type mentions any of `class-params`, or #f.
(define (find-dispatch-pos t class-params)
  (let loop ([t t] [pos 0])
    (cond
      [(arrow? t)
       (define dom (arrow-dom t))
       (cond
         [(ormap (lambda (p) (set-member? (type-vars dom) p)) class-params)
          pos]
         [else (loop (arrow-cod t) (add1 pos))])]
      [else #f])))

(define (handle-instance-form ctx head methods stx env declared)
  (define head-pred (resolve-constraint head))
  (define class-name (pred-class head-pred))
  (define cinfo (env-ref-class env class-name))
  (unless cinfo
    (raise-syntax-error 'infer
      (format "unknown class: ~s" class-name) stx))
  (define inst-args (pred-args head-pred))
  (define ctx-preds (for/list ([c (in-list ctx)]) (resolve-constraint c)))
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
                     (pred->datum head-pred) method-name)
             stx)]))
      ;; Substitute class params → instance args in the method's body.
      (define σ
        (for/fold ([s empty-subst])
                  ([p (in-list (class-info-params cinfo))]
                   [a (in-list inst-args)])
          (subst-extend s p a)))
      (define inst-method-qual (apply-subst σ (scheme-body method-sch)))
      (define expected-type (qual-body-type inst-method-qual))
      (parameterize ([current-pending-preds (box '())])
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
        (apply-subst-to-preds! (subst-compose s-u s))
        ;; The instance head itself is a hypothesis during method checking
        ;; (so a method body can recursively use its own class methods).
        (define leftovers
          (reduce-context env (cons head-pred ctx-preds)
                          (snapshot-preds)))
        (unless (null? leftovers)
          (raise-syntax-error 'infer
            (format "instance ~s method ~s leaves unsolved constraints: ~s"
                    (pretty-pred head-pred) method-name
                    (map pretty-pred leftovers))
            stx)))
      (hash-set acc method-name body)))
  (define info (instance-info head-pred ctx-preds checked-bodies))
  (values (env-extend-instance env class-name info) declared))

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
         "surface.rkt")

;; ----- fresh type variables -----------------------------------------

(define current-fresh-state (make-parameter #f))

(define (fresh-tvar [prefix 'a])
  (define box (current-fresh-state))
  (unless box
    (error 'fresh-tvar "called outside of inference"))
  (define n (unbox box))
  (set-box! box (add1 n))
  (tvar (string->symbol (format "~a~a" prefix n))))

(define-syntax-rule (with-fresh body ...)
  (parameterize ([current-fresh-state (box 0)]) body ...))

;; ----- generalize / instantiate -------------------------------------

(define (instantiate sch)
  (match sch
    [(scheme '() body) body]
    [(scheme vs body)
     (define s
       (for/fold ([s empty-subst]) ([v (in-list vs)])
         (subst-extend s v (fresh-tvar v))))
     (apply-subst s body)]))

;; Skolemize: replace each bound type variable with a fresh tcon, so the
;; declared signature acts rigidly — the body can't sneak a more specific
;; type past a polymorphic declaration.
(define (skolemize sch)
  (match sch
    [(scheme '() body) body]
    [(scheme vs body)
     (define s
       (for/fold ([s empty-subst]) ([v (in-list vs)])
         (subst-extend s v (tcon (gensym (format "$skolem.~a." v))))))
     (apply-subst s body)]))

(define (generalize env ty)
  (define env-fv (env-vars-free-vars env))
  (define ty-fv  (type-vars ty))
  (define quantified (sort (set->list (set-subtract ty-fv env-fv)) symbol<?))
  (scheme quantified ty))

;; ----- type expression → type ---------------------------------------

;; Resolve a parsed type AST to a raw type.  `(All ...)` wrappers are
;; stripped at this level; for declarations we use `resolve-scheme`
;; below, which preserves the quantifier.
(define (resolve-type ty-ast)
  (match ty-ast
    [(ty:var n _)        (tvar n)]
    [(ty:con n _)        (tcon n)]
    [(ty:app h args _)
     (make-tapp (resolve-type h)
                (for/list ([a (in-list args)]) (resolve-type a)))]
    [(ty:forall _ body _) (resolve-type body)]))

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
                      (type->datum actual)
                      (for/list ([t ts-args]) (type->datum (apply-subst s-args t))))
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
                      (type->datum (apply-subst s-c t-c)))
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
                      (type->datum (apply-subst s3 t-then))
                      (type->datum (apply-subst s3 t-else)))
              stx))])
        (unify (apply-subst s3 t-then) (apply-subst s3 t-else))))
     (define s-final (subst-compose s-branches s3))
     (values s-final (apply-subst s-final t-then))]

    [(e:ann expr ty-ast stx)
     (define-values (s-e t-e) (infer-expr expr env))
     (define declared (resolve-type ty-ast))
     (define s-u
       (with-handlers
        ([exn:fail:unify?
          (lambda (_)
            (raise-syntax-error 'infer
              (format "expression has type ~a but ascription says ~a"
                      (type->datum (apply-subst s-e t-e))
                      (type->datum declared))
              stx))])
        (unify (apply-subst s-e t-e) declared)))
     (values (subst-compose s-u s-e) (apply-subst s-u declared))]

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
                   (type->datum pat-type) (type->datum scrut-type))
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
                   (type->datum (apply-subst s-acc t-body))
                   (type->datum (apply-subst s-acc result-type)))
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
        (define decl-ty (skolemize decl-scheme))
        (define env-rec (env-extend-var env name (scheme '() decl-ty)))
        (define-values (s t) (infer-expr expr env-rec))
        (with-handlers
         ([exn:fail:unify?
           (lambda (_)
             (raise-syntax-error 'infer
               (format "definition of ~s has type ~a, declared as ~a"
                       name
                       (type->datum (apply-subst s t))
                       (scheme->datum decl-scheme))
               stx))])
         (unify (apply-subst s t) decl-ty))
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
        (define final-ty (apply-subst s* t))
        (define final-env (apply-subst/env s* env))
        (values (env-extend-var final-env name (generalize final-env final-ty))
                declared)])]

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

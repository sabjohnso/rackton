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

         empty-env
         env-extend-var
         env-ref-var
         env-extend-data
         env-ref-data
         env-extend-tcon
         env-ref-tcon
         env-extend-class
         env-ref-class
         env-extend-instance
         env-instances
         env-ref-method-class
         env-extend-alias
         env-ref-alias
         env-vars-free-vars
         apply-subst/env

         initial-env)

(require racket/set
         "types.rkt")

;; ----- structures ----------------------------------------------------

(struct env (vars data-ctors tcons classes instance-table method-owners aliases)
  #:transparent)

;; A data-constructor's typing information.  `scheme` is the polymorphic
;; type assigned to the constructor when used as a value.  `arity` is
;; the number of arguments the constructor takes.
(struct data-info (type-name ctor-name arity scheme) #:transparent)

;; A type-constructor's kinding information.  `ctors` lists every
;; data constructor that produces this type — used for exhaustiveness.
(struct tcon-info (name arity ctors) #:transparent)

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
(struct class-info (name params kinds supers methods defaults dispatchpos
                    fundeps dictreqs)
  #:transparent)

;; An instance's information.
;;   head    : pred — the instance head, e.g. (Eq Integer) or (Eq (Maybe a))
;;   context : (Listof pred) — qualifying preds for this instance
;;   methods : (HashEq method-name → surface-expr) — method bodies
(struct instance-info (head context methods) #:transparent)

(define empty-env (env (hasheq) (hasheq) (hasheq) (hasheq) (hasheq) (hasheq) (hasheq)))

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

;; ----- classes & instances ------------------------------------------

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

(define (env-extend-instance e class-name inst)
  (struct-copy env e
               [instance-table
                (hash-update (env-instance-table e) class-name
                             (lambda (cur) (append cur (list inst)))
                             (lambda () '()))]))

(define (env-instances e class-name)
  (hash-ref (env-instance-table e) class-name '()))

(define (env-ref-method-class e method [default #f])
  (hash-ref (env-method-owners e) method default))

;; ----- type aliases -----------------------------------------------
;; `aliases` maps an alias name to (cons param-list target-ty-ast).
;; The target-ty-ast is the surface ty:* AST so that resolution can
;; substitute and recurse uniformly.

(define (env-extend-alias e name params target)
  (struct-copy env e
               [aliases (hash-set (env-aliases e) name (cons params target))]))

(define (env-ref-alias e name [default #f])
  (hash-ref (env-aliases e) name default))

;; Free type variables across every value binding's scheme — needed
;; for `generalize` at let bindings.
(define (env-vars-free-vars e)
  (for/fold ([acc (seteq)]) ([(_ sch) (in-hash (env-vars e))])
    (set-union acc (scheme-free-vars sch))))

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
;; any rackton program.  Each is monomorphic at Phase 1; arithmetic
;; ops fix Integer because we have no Num class yet.

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

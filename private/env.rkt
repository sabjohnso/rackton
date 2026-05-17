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

         empty-env
         env-extend-var
         env-ref-var
         env-extend-data
         env-ref-data
         env-extend-tcon
         env-ref-tcon
         env-vars-free-vars
         apply-subst/env

         initial-env)

(require racket/set
         "types.rkt")

;; ----- structures ----------------------------------------------------

(struct env       (vars data-ctors tcons) #:transparent)

;; A data-constructor's typing information.  `scheme` is the polymorphic
;; type assigned to the constructor when used as a value.  `arity` is
;; the number of arguments the constructor takes.
(struct data-info (type-name ctor-name arity scheme) #:transparent)

;; A type-constructor's kinding information.  `ctors` lists every
;; data constructor that produces this type — used for exhaustiveness.
(struct tcon-info (name arity ctors) #:transparent)

(define empty-env (env (hasheq) (hasheq) (hasheq)))

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

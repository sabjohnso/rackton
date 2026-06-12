#lang racket/base

;; Rackton — type-directed search for the REPL's ,accepts command.
;;
;; `,accepts TYPE` lists the functions in scope that take an argument
;; of TYPE — Hoogle-style search restricted to argument positions.  A
;; candidate matches when TYPE unifies with one of the argument
;; positions along its (curried) arrow spine.  Argument positions that
;; are *unconstrained* type variables are excluded: `(-> a a)` accepts
;; everything, so listing it for every query would bury the
;; informative matches.  A *constrained* variable participates: the
;; `a` in `(Num a) => (-> a (-> a a))` matches `Integer` exactly when
;; the constraints remain satisfiable under the match — so `,accepts
;; Integer` lists `+`, but not the method of a class with no Integer
;; instance.
;;
;; Public API:
;;   accepts-search — env × type datum →
;;                    'bare-query | (listof (cons name scheme)), sorted
;;                    (raises exn:fail on an unparsable type)

(provide accepts-search)

(require racket/match
         racket/set
         "types.rkt"
         "unify.rkt"
         "env.rkt"
         (only-in "surface.rkt" parse-type)
         (only-in "infer.rkt" resolve-scheme))

(define (accepts-search env type-datum)
  ;; A query's own constraints are dropped: `,accepts ((Num a) => a)`
  ;; searches for the bare `a` (and is then rejected as bare).
  (define query
    (qual-body-type
     (instantiate (resolve-scheme (parse-type (datum->syntax #f type-datum))
                                  env))))
  (cond
    [(tvar? query) 'bare-query]
    [else
     (sort (append (var-matches env query)
                   (ctor-matches env query))
           symbol<? #:key car)]))

;; ----- candidate enumeration ----------------------------------------

;; Session and prelude values.  `$`-prefixed names are the REPL's own
;; synthetic expression bindings — not part of the user's vocabulary.
(define (var-matches env query)
  (for/list ([(name sch) (in-hash (env-vars env))]
             #:unless (synthetic-name? name)
             #:when (scheme-accepts? env sch query))
    (cons name sch)))

;; Data constructors are functions too: `,accepts Integer` should find
;; a ctor with an Integer field.
(define (ctor-matches env query)
  (for/list ([(name di) (in-hash (env-data-ctors env))]
             #:when (scheme-accepts? env (data-info-scheme di) query))
    (cons name (data-info-scheme di))))

(define (synthetic-name? name)
  (char=? (string-ref (symbol->string name) 0) #\$))

;; ----- matching ------------------------------------------------------

;; A candidate accepts the query when the query unifies with one of
;; its argument positions AND, under that unification, each of the
;; candidate's class constraints can still be satisfied by some
;; instance.  The second check is what keeps `fmap` (whose `(f a)`
;; argument matches `(List Integer)` with `(Functor List)` on hand)
;; while dropping, say, `censor` (whose `(m a)` also unifies, but no
;; `(MonadWriter w List)` instance can ever apply).
;;
;; A variable argument position is skipped only when no constraint
;; mentions it — a constrained one (the `a` of `(Num a) => …`) binds
;; to the query and stands or falls with its constraints.
(define (scheme-accepts? env sch query)
  (define body (instantiate sch))
  (define t (qual-body-type body))
  (define preds (if (qual? body) (qual-constraints body) '()))
  (define constrained
    (for*/seteq ([p (in-list preds)] [v (in-set (pred-vars p))]) v))
  (for/or ([arg (in-list (arrow-arguments t))]
           #:unless (unconstrained-tvar? arg constrained))
    (define σ (try-unify arg query))
    (and σ
         (for/and ([p (in-list preds)])
           (pred-possibly-satisfiable? env (apply-subst σ p))))))

(define (unconstrained-tvar? t constrained)
  (and (tvar? t) (not (set-member? constrained (tvar-name t)))))

;; Conservative satisfiability: refute a predicate only when it
;; mentions a concrete constructor and no instance head of its class
;; unifies with it.  A fully variable predicate (e.g. `(Eq a)` with
;; `a` free) might always be satisfied later, so it passes.  Instance
;; contexts are not chased — a false positive merely lists an extra
;; candidate, while a false refutation would hide a real one.
(define (pred-possibly-satisfiable? env p)
  (cond
    [(not (ormap mentions-tcon? (pred-args p))) #t]
    [else
     (for/or ([inst (in-list (env-instances env (pred-class p)))])
       (preds-unify? (freshen-pred (instance-info-head inst)) p))]))

(define (mentions-tcon? t)
  (match t
    [(tcon _)      #t]
    [(tapp h args) (or (mentions-tcon? h) (ormap mentions-tcon? args))]
    [_             #f]))

;; Unify two same-class predicates argument by argument, threading the
;; substitution so a variable bound by one argument constrains the next.
(define (preds-unify? p q)
  (with-handlers ([exn:fail:unify? (lambda (_) #f)])
    (for/fold ([σ empty-subst] #:result #t)
              ([x (in-list (pred-args p))]
               [y (in-list (pred-args q))])
      (subst-compose (unify (apply-subst σ x) (apply-subst σ y)) σ))))

;; An instance head's variables come from its defining module and can
;; collide with the (gensym-fresh) query/candidate variables only by
;; staying as-written — rename them apart before unifying.
(define (freshen-pred p)
  (define σ
    (for/fold ([σ empty-subst]) ([v (in-set (pred-vars p))])
      (subst-extend σ v (tvar (gensym v)))))
  (pred (pred-class p) (map (lambda (t) (apply-subst σ t)) (pred-args p))))

;; The argument positions along a curried arrow spine:
;; (-> A (-> B R)) → (A B).
(define (arrow-arguments t)
  (match t
    [(tapp (tcon '->) (list a r)) (cons a (arrow-arguments r))]
    [_ '()]))

(define (try-unify a b)
  (with-handlers ([exn:fail:unify? (lambda (_) #f)])
    (unify a b)))

;; Replace a scheme's quantified variables with fresh ones, so two
;; candidates (or candidate and query) can never collide on a
;; variable name during unification.
(define (instantiate sch)
  (match sch
    [(scheme vs body)
     (define s
       (for/fold ([s empty-subst]) ([v (in-list vs)])
         (subst-extend s v (tvar (gensym v)))))
     (apply-subst s body)]))

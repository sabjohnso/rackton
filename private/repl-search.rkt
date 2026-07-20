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
;; instance.  The same holds of the *query*: `((Num a) => a)` is not a
;; bare variable but "some type having a `Num` instance", and its
;; constraints filter the candidates the same way.
;;
;; Public API:
;;   accepts-search — env × type datum →
;;                    'bare-query | (listof (cons name scheme)), sorted
;;                    (raises exn:fail on an unparsable type)
;;   search-entries — (listof (cons name scheme)) × query →
;;                    'bare-query | (listof (cons name scheme)), best
;;                    rank first.  The query is a type datum searched
;;                    under #:kind ('signature, 'returns, or 'accepts),
;;                    or a string searched against names.  See the
;;                    comment on the definition for the per-kind rules.
;;   env-search-entries — env → (listof (cons name scheme)), the
;;                    candidates an env contributes (values and data
;;                    constructors, minus the REPL's synthetic names).
;;
;; 'bare-query means the query was a type variable carrying no
;; constraints, which every candidate would match.

(provide accepts-search
         search-entries
         env-search-entries)

(require racket/match
         racket/set
         racket/list
         racket/string
         "types.rkt"
         "unify.rkt"
         "env.rkt"
         "prelude.rkt"
         (only-in "surface.rkt" parse-type)
         (only-in "infer.rkt" resolve-scheme))

(define (accepts-search env type-datum)
  (define q (parse-query env type-datum))
  (cond
    [(bare-query? q) 'bare-query]
    [else
     (sort (append (var-matches env q) (ctor-matches env q))
           symbol<? #:key car)]))

;; ----- query parsing --------------------------------------------------
;;
;; A query keeps its own constraints: `((Num a) => a)` is not the bare
;; variable `a` but "some type having a `Num` instance".  The
;; constraints travel with the body type and are re-checked against
;; each candidate's match substitution.  They ride in one struct so a
;; further query attribute does not re-thread every match rule.
(struct search-query (type preds) #:transparent)

(define (parse-query env type-datum)
  (define-values (t preds)
    (split-quals
     (instantiate (resolve-scheme (parse-type (datum->syntax #f type-datum))
                                  env))))
  ;; Synonyms expand once here rather than per candidate: expansion
  ;; substitutes the synonym's parameters by the predicate's arguments,
  ;; so it commutes with the match substitution applied later.
  (search-query t (expand-synonyms env preds)))

(define (expand-synonyms env preds)
  (append*
   (for/list ([p (in-list preds)])
     (cond
       [(expand-constraint-syn env p)
        => (lambda (comps) (expand-synonyms env comps))]
       [else (list p)]))))

;; A variable with no constraints matches everything, so it is not a
;; useful query; a constrained one narrows the candidates.
(define (bare-query? q)
  (and (tvar? (search-query-type q)) (null? (search-query-preds q))))

;; A qualified type can nest — `(MonadTrans t) => ((Monad m) => (-> …))`
;; — so peel every layer, not just the outermost, and gather all the
;; constraints.  Stopping at the first layer would leave a `qual` struct
;; where the arrow is, hiding the argument positions from every match
;; rule below.
(define (split-quals t)
  (let loop ([t t] [preds '()])
    (cond
      [(qual? t) (loop (qual-body t) (append preds (qual-constraints t)))]
      [else (values t preds)])))

;; `split-quals` is internal, but its flattening law is what the nested
;; `=>` fix rests on, so the property tests in repl-search-test.rkt
;; reach it here rather than through the module's interface.
(module+ private-for-test
  (provide split-quals))

;; The query's constraints and the candidate's, under the substitution
;; that matched them, must hold together of some type the environment
;; knows.  A search with no session env judges the query against the
;; prelude's instances, since a query names classes the user typed and
;; expects to mean something.
(define (constraints-satisfiable? env query-preds cand-preds σ)
  (define e (or env prelude-env))
  (define (under-σ ps) (for/list ([p (in-list ps)]) (apply-subst σ p)))
  (define cs (expand-synonyms e (under-σ cand-preds)))
  (and
   ;; A candidate's class the env cannot see has no instance the search
   ;; can reach.  A query's is passed instead: the user may name a class
   ;; the session has not imported, and refuting it would silently empty
   ;; the result list rather than report the unfamiliar name.
   (for/and ([p (in-list cs)]) (and (env-ref-class e (pred-class p)) #t))
   (let ([ps (append (for/list ([p (in-list (under-σ query-preds))]
                                #:when (env-ref-class e (pred-class p)))
                       p)
                     cs)])
     (and (for/and ([p (in-list ps)]) (pred-has-instance? e p))
          (variables-have-a-common-type? e ps)))))

;; ----- candidate enumeration ----------------------------------------

;; Session and prelude values.  `$`-prefixed names are the REPL's own
;; synthetic expression bindings — not part of the user's vocabulary.
(define (var-matches env q)
  (for/list ([(name sch) (in-hash (env-vars env))]
             #:unless (synthetic-name? name)
             #:when (scheme-accepts? env sch q))
    (cons name sch)))

;; Data constructors are functions too: `,accepts Integer` should find
;; a ctor with an Integer field.
(define (ctor-matches env q)
  (for/list ([(name di) (in-hash (env-data-ctors env))]
             #:when (scheme-accepts? env (data-info-scheme di) q))
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
(define (scheme-accepts? env sch q)
  (define-values (t preds constrained) (candidate-shape sch))
  (for/or ([arg (in-list (arrow-arguments t))]
           #:unless (unconstrained-tvar? arg constrained))
    (define σ (try-unify arg (search-query-type q)))
    (and σ (constraints-satisfiable? env (search-query-preds q) preds σ))))

;; A candidate's type as the match rules need it: the arrow under every
;; qualifier layer, the constraints gathered from those layers, and the
;; variables those constraints mention.
(define (candidate-shape sch)
  (define-values (t preds) (split-quals (instantiate sch)))
  (values t preds
          (for*/seteq ([p (in-list preds)] [v (in-set (pred-vars p))]) v)))

(define (unconstrained-tvar? t constrained)
  (and (tvar? t) (not (set-member? constrained (tvar-name t)))))

;; One predicate holds of some instance the env carries.  Instance
;; contexts are not chased: `(Eq (List a))` counts as satisfiable on the
;; strength of an `(Eq (List a))` head, without asking whether `(Eq a)`
;; follows.
(define (pred-has-instance? env p)
  (for/or ([inst (in-list (env-instances env (pred-class p)))])
    (preds-unify? (freshen-pred (instance-info-head inst)) p)))

;; Predicates over a type *variable* are each satisfiable alone — a
;; variable unifies with every instance head — so they must be judged
;; together.  `mempty :: (Monoid a) => a` answers a search for
;; `((Additive-Magma a) (Multiplicative-Magma a) => a)` only if some one
;; type carries all three classes, and none does.  Search reports what
;; the environment holds now, so a variable no known type can satisfy is
;; refuted rather than left open against instances that may be written
;; later.
(define (variables-have-a-common-type? env preds)
  (for/and ([(_v allowed) (in-hash (allowed-types-per-variable env preds))])
    (or (eq? allowed 'any) (not (set-empty? allowed)))))

;; variable name → the type constructors every class constraining it
;; admits at that argument position; 'any when no class restricts it.
(define (allowed-types-per-variable env preds)
  (for*/fold ([acc (hasheq)])
             ([p (in-list preds)]
              [(arg idx) (in-indexed (in-list (pred-args p)))]
              #:when (tvar? arg))
    (hash-update acc (tvar-name arg)
                 (lambda (cur) (meet-allowed cur (allowed-heads env (pred-class p) idx)))
                 'any)))

;; The type constructors a class's instances admit at argument `idx`.
;; 'any when some instance head has a variable there, since that head
;; matches every type.
(define (allowed-heads env cls idx)
  (for/fold ([acc (seteq)]) ([inst (in-list (env-instances env cls))])
    (cond
      [(eq? acc 'any) 'any]
      [else
       (define args (pred-args (instance-info-head inst)))
       (cond
         [(>= idx (length args)) acc]
         [(head-tcon-name (list-ref args idx))
          => (lambda (n) (set-add acc n))]
         [else 'any])])))

(define (meet-allowed a b)
  (cond
    [(eq? a 'any) b]
    [(eq? b 'any) a]
    [else (set-intersect a b)]))

;; The type constructor at the head of a type, or #f when a variable
;; heads it (`(m a)`) — such a head admits any constructor.
(define (head-tcon-name t)
  (match t
    [(tcon n)          n]
    [(tapp h _args)    (head-tcon-name h)]
    [_                 #f]))

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

;; ----- Hoogle-style search ---------------------------------------------

;; The (name . scheme) pairs a session env contributes as candidates —
;; values and data constructors, minus the REPL's synthetic names.
(define (env-search-entries env)
  (append
   (for/list ([(name sch) (in-hash (env-vars env))]
              #:unless (synthetic-name? name))
     (cons name sch))
   (for/list ([(name di) (in-hash (env-data-ctors env))])
     (cons name (data-info-scheme di)))))

;; Search candidates by `query` under `kind`:
;;   'signature — query is a type datum; an arrow matches a whole
;;       signature (same arity) by unification, exactly in order
;;       (rank 0) or with the arguments permuted (rank 1); a
;;       non-arrow query matches values of that type.  A string query
;;       searches names instead.
;;   'returns — query is a type datum matching the (curried) result.
;;   'accepts — query matches some argument position (the ,accepts rule).
;; `env` supplies alias resolution and the constraint-satisfiability
;; filter; #f falls back to the prelude for parsing and skips the
;; filter (index entries' instances live outside any env).
;; Returns matched (name . scheme) pairs, best rank first, then by
;; name; 'bare-query when the query is a variable carrying no
;; constraints (a constrained one narrows the candidates and searches
;; normally).
(define (search-entries entries query
                        #:kind [kind 'signature]
                        #:env [env #f])
  (cond
    [(string? query)
     (sort (for/list ([e (in-list entries)]
                      #:when (string-contains? (symbol->string (car e)) query))
             e)
           symbol<? #:key car)]
    [else
     (define parse-env (or env prelude-env))
     (define q (parse-query parse-env query))
     (cond
       [(bare-query? q) 'bare-query]
       [else
        (define ranked
          (for*/list ([e (in-list entries)]
                      [r (in-value (match-rank env kind q (cdr e)))]
                      #:when r)
            (cons r e)))
        (map cdr (sort ranked
                       (lambda (a b)
                         (or (< (car a) (car b))
                             (and (= (car a) (car b))
                                  (symbol<? (cadr a) (cadr b)))))))])]))

;; The rank of one candidate against the query type, or #f.
(define (match-rank env kind q sch)
  (define qtype (search-query-type q))
  (define-values (t preds constrained) (candidate-shape sch))
  ;; The query's own constraints are judged even with no env (the
  ;; prelude stands in); a candidate's are judged only against a real
  ;; session env, since an index entry's instances live outside it.
  ;; An index entry's own constraints are judged only against a real
  ;; session env, since the instances discharging them live outside it;
  ;; the query's are judged either way (the prelude stands in).
  (define (preds-ok? σ)
    (constraints-satisfiable? env (search-query-preds q)
                              (if env preds '()) σ))
  (case kind
    [(signature)
     (define-values (qargs qres) (arrow-spine qtype))
     (define-values (cargs cres) (arrow-spine t))
     (and (= (length qargs) (length cargs))
          (or (let ([σ (unify-pairs (cons (cons cres qres)
                                          (map cons cargs qargs)))])
                (and σ (preds-ok? σ) 0))
              ;; the same arguments in another order — capped so the
              ;; permutation count stays trivial
              (and (pair? cargs) (<= (length cargs) 6)
                   (for/or ([perm (in-list (permutations cargs))]
                            #:unless (equal? perm cargs))
                     (let ([σ (unify-pairs (cons (cons cres qres)
                                                 (map cons perm qargs)))])
                       (and σ (preds-ok? σ) 1))))))]
    [(returns)
     (define-values (_args cres) (arrow-spine t))
     (and (not (unconstrained-tvar? cres constrained))
          (let ([σ (try-unify cres qtype)])
            (and σ (preds-ok? σ) 0)))]
    [(accepts)
     (and (for/or ([arg (in-list (arrow-arguments t))]
                   #:unless (unconstrained-tvar? arg constrained))
            (let ([σ (try-unify arg qtype)])
              (and σ (preds-ok? σ))))
          0)]))

;; The full curried spine: (-> A (-> B R)) → (values (A B) R).
(define (arrow-spine t)
  (match t
    [(tapp (tcon '->) (list a r))
     (define-values (args res) (arrow-spine r))
     (values (cons a args) res)]
    [_ (values '() t)]))

;; Unify each (left . right) pair, threading the substitution; #f when
;; any pair fails.
(define (unify-pairs pairs)
  (with-handlers ([exn:fail:unify? (lambda (_) #f)])
    (for/fold ([σ empty-subst]) ([p (in-list pairs)])
      (subst-compose (unify (apply-subst σ (car p))
                            (apply-subst σ (cdr p)))
                     σ))))

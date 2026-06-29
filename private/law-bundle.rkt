#lang racket/base

;; Rackton — runnable law bundles (Feature 9).
;;
;; Synthesize, from each protocol that declares `:laws`, a `<Class>-laws`
;; function value that turns generators into a `Test` group of properties
;; — one per RUNNABLE law — driving each already-type-checked law body
;; over generated inputs and labeling every binder by source name on
;; failure.  This is a pure surface-AST → surface-AST transformation: the
;; generated `top:def` is spliced into the program and runs through the
;; normal infer + codegen, so inference computes the bundle's constraints
;; (e.g. `((Eq a) (Show a) => (-> (Gen a) Test))`, or for a higher-kinded
;; class `((Box2 f) (Eq (f a)) (Show (f a)) => (-> (Gen (f a)) Test))`) on
;; its own.
;;
;; GATE: emission is gated on the module importing `rackton/unit`, whose
;; names the bundle references (`group-of`, `it-prop`, `for-all-gen`,
;; `gen-pair`, `Gen`, `Test`).  A module that declares laws but does not
;; import unit keeps today's behaviour: the laws stay compile-time-only.
;;
;; SCOPE: first-order AND higher-kinded classes, best-effort PER LAW.  A
;; law is RUNNABLE iff
;;   (a) no binder has a function type — we have no function generator
;;       (so a functor composition law, quantified over `g`/`h`, is
;;       skipped); and
;;   (b) its body uses no return-typed method — `pure`/`mempty` and any
;;       user method whose dispatch is by result type cannot resolve from
;;       a passed dictionary (so a monoid identity law over `mempty` is
;;       skipped).
;; A class with no runnable law emits no bundle.  The return-typed names
;; come from the prelude (passed in) unioned with the surface signatures
;; of every protocol in this block; a return-typed method imported from
;; another user module (not the prelude) is not detected here — its
;; dispatch position is not known until inference.
;;
;; The generated bundle takes one generator per DISTINCT binder type
;; across its runnable laws (binders sharing a type share a generator,
;; combined with nested `gen-pair` + a `Pair` `match`).  For a first-order
;; single-parameter class every binder is the parameter, so this is one
;; `(Gen a)` argument.

(require racket/list
         racket/match
         racket/set
         "ast.rkt")

(provide synthesize-law-bundles)

;; The parsed top-form list and the prelude's return-typed method names →
;; a list of generated `top:def`s (empty when the module does not import
;; rackton/unit, or declares no runnable laws).
(define (synthesize-law-bundles parsed prelude-return-typed)
  (cond
    [(not (requires-unit? parsed)) '()]
    [else
     (define rt-names
       (set-union prelude-return-typed (user-class-return-typed-names parsed)))
     (for*/list ([f (in-list parsed)]
                 #:when (top:class? f)
                 [bundle (in-value (build-class-bundle f rt-names))]
                 #:when bundle)
       bundle)]))

;; ----- the unit-import gate -----

;; Does any `(require …)` in the block pull in rackton/unit?  We accept
;; the collection path `rackton/unit` and any relative path resolving to
;; the unit tree (unit.rkt or a unit/ submodule), recurring into
;; sub-forms like `(prefix-in u: …)` / `(only-in … )`.
(define (requires-unit? parsed)
  (for/or ([f (in-list parsed)] #:when (top:require? f))
    (for/or ([spec (in-list (top:require-specs f))])
      (spec-mentions-unit? (syntax->datum spec)))))

(define (spec-mentions-unit? d)
  (cond
    [(string? d)
     (regexp-match? #rx"(^|/)unit(/(laws|tree|gen|property))?[.]rkt$" d)]
    [(symbol? d) (eq? d 'rackton/unit)]
    [(pair? d) (or (spec-mentions-unit? (car d))
                   (spec-mentions-unit? (cdr d)))]
    [else #f]))

;; ----- return-typed method detection (surface) -----

;; Names of methods declared in this block's protocols whose dispatch is
;; by result type: a method is return-typed iff none of its argument
;; types mentions a class parameter (so no runtime value reveals the
;; instance — exactly the `pure`/`mempty` shape).
(define (user-class-return-typed-names parsed)
  (for*/fold ([acc (seteq)])
             ([f (in-list parsed)] #:when (top:class? f)
              [m (in-list (top:class-methods f))] #:when (method-sig? m))
    (if (method-sig-return-typed? m (class-param-names f))
        (set-add acc (method-sig-name m))
        acc)))

(define (class-param-names f)
  (for/list ([a (in-list (constraint-args (top:class-head f)))]
             #:when (ty:var? a))
    (ty:var-name a)))

(define (method-sig-return-typed? m params)
  (define args (surface-arg-types (method-sig-type m)))
  (not (for/or ([dom (in-list args)]) (type-mentions-any? dom params))))

;; The domain types of a (possibly qualified / quantified) arrow chain.
(define (surface-arg-types t)
  (let loop ([t (strip-quantifiers t)])
    (cond
      [(arrow-type? t) (cons (arrow-dom t) (loop (arrow-cod t)))]
      [else '()])))

(define (strip-quantifiers t)
  (cond
    [(ty:qual? t)   (strip-quantifiers (ty:qual-body t))]
    [(ty:forall? t) (strip-quantifiers (ty:forall-body t))]
    [else t]))

;; ----- surface-type predicates -----

(define (arrow-type? t)
  (and (ty:app? t)
       (ty:con? (ty:app-head t))
       (eq? (ty:con-name (ty:app-head t)) '->)
       (= 2 (length (ty:app-args t)))))
(define (arrow-dom t) (car (ty:app-args t)))
(define (arrow-cod t) (cadr (ty:app-args t)))

(define (type-mentions-arrow? t)
  (let loop ([t t])
    (cond
      [(ty:con? t)    (eq? (ty:con-name t) '->)]
      [(ty:app? t)    (or (loop (ty:app-head t)) (ormap loop (ty:app-args t)))]
      [(ty:forall? t) (loop (ty:forall-body t))]
      [(ty:exists? t) (loop (ty:exists-body t))]
      [(ty:qual? t)   (or (ormap (lambda (c) (ormap loop (constraint-args c)))
                                 (ty:qual-constraints t))
                          (loop (ty:qual-body t)))]
      [else #f])))

(define (type-mentions-any? t names)
  (let loop ([t t])
    (cond
      [(ty:var? t)    (and (memq (ty:var-name t) names) #t)]
      [(ty:app? t)    (or (loop (ty:app-head t)) (ormap loop (ty:app-args t)))]
      [(ty:forall? t) (loop (ty:forall-body t))]
      [(ty:exists? t) (loop (ty:exists-body t))]
      [(ty:qual? t)   (or (ormap (lambda (c) (ormap loop (constraint-args c)))
                                 (ty:qual-constraints t))
                          (loop (ty:qual-body t)))]
      [else #f])))

;; A stx-free key identifying a surface type up to alpha-equal structure,
;; so binders of the same type share one generator parameter.
(define (type-key t)
  (cond
    [(ty:var? t)    (list 'v (ty:var-name t))]
    [(ty:con? t)    (list 'c (ty:con-name t))]
    [(ty:nat? t)    (list 'n (ty:nat-value t))]
    [(ty:app? t)    (list 'a (type-key (ty:app-head t)) (map type-key (ty:app-args t)))]
    [(ty:forall? t) (list 'all (ty:forall-vars t) (type-key (ty:forall-body t)))]
    [(ty:exists? t) (list 'ex (ty:exists-vars t) (type-key (ty:exists-body t)))]
    [(ty:qual? t)   (list 'q
                          (map (lambda (c) (cons (constraint-class c)
                                                 (map type-key (constraint-args c))))
                               (ty:qual-constraints t))
                          (type-key (ty:qual-body t)))]
    [else (list 'other)]))

;; ----- which value names a law body uses (for return-typed skipping) -----

(define (collect-var-names body)
  (define seen (mutable-seteq))
  (let walk ([e body])
    (match e
      [(e:var n _)             (set-add! seen n)]
      [(e:literal _ _)         (void)]
      [(e:lam _ b _)           (walk b)]
      [(e:app h args _)        (walk h) (for-each walk args)]
      [(e:let bs b _)          (for ([bd (in-list bs)]) (walk (cdr bd))) (walk b)]
      [(e:letrec bs b _)       (for ([bd (in-list bs)]) (walk (cdr bd))) (walk b)]
      [(e:if a b c _)          (walk a) (walk b) (walk c)]
      [(e:ann e2 _ _)          (walk e2)]
      [(e:match s cs _ _)      (walk s)
                               (for ([c (in-list cs)])
                                 (when (clause-guard c) (walk (clause-guard c)))
                                 (walk (clause-body c)))]
      [(e:match* ss cs _ _)    (for-each walk ss)
                               (for ([c (in-list cs)])
                                 (when (clause*-guard c) (walk (clause*-guard c)))
                                 (walk (clause*-body c)))]
      [(e:tuple es _)          (for-each walk es)]
      [(e:tref t _ _)          (walk t)]
      [(e:update r us _)       (walk r) (for ([u (in-list us)]) (walk (cdr u)))]
      [(e:open e2 _ _ b _)     (walk e2) (walk b)]
      [(e:array es _)          (for-each walk es)]
      [(e:build-array _ p _)   (walk p)]
      [(e:aref a _ _)          (walk a)]
      [(e:array-slice _ _ a _) (walk a)]
      [(e:handle e2 cls ret _) (walk e2)
                               (for ([c (in-list cls)]) (walk (handle-clause-body c)))
                               (when ret (walk (handle-return-body ret)))]
      [_ (void)]))
  seen)

;; ----- eligibility -----

(define (class-laws f) (filter class-law? (top:class-methods f)))

(define (law-runnable? law rt-names)
  (and (for/and ([b (in-list (class-law-binders law))])
         (not (type-mentions-arrow? (law-binder-type b))))
       (let ([used (collect-var-names (class-law-body law))])
         (for/and ([n (in-set rt-names)]) (not (set-member? used n))))))

;; ----- synthesis -----

;; #f when the class has no runnable law, else its `<Class>-laws` def.
(define (build-class-bundle f rt-names)
  (define runnable (filter (lambda (law) (law-runnable? law rt-names)) (class-laws f)))
  (cond
    [(null? runnable) #f]
    [else
     (define base       (top:class-stx f))
     (define class-name (constraint-class (top:class-head f)))
     (define bundle-name (string->symbol (format "~a-laws" class-name)))
     (define-values (key->sym gen-syms) (assign-gens runnable))
     (define prop-tests
       (for/list ([law (in-list runnable)])
         (build-it-prop class-name law base key->sym)))
     (top:def bundle-name
              (lam base gen-syms
                   (app base (var base 'group-of)
                        (list (lit base (format "~a laws" class-name))
                              (list->ast base prop-tests))))
              base)]))

;; One gensym'd generator parameter per distinct binder type, in order of
;; first appearance across the runnable laws.  gensym'd so it never
;; collides with a law binder of the same source name.
(define (assign-gens runnable)
  (define key->sym (make-hash))
  (define ordered '())
  (for* ([law (in-list runnable)]
         [b (in-list (class-law-binders law))])
    (define k (type-key (law-binder-type b)))
    (unless (hash-has-key? key->sym k)
      (define s (gensym 'gen))
      (hash-set! key->sym k s)
      (set! ordered (cons s ordered))))
  (values key->sym (reverse ordered)))

(define (binder-gen-sym key->sym b)
  (hash-ref key->sym (type-key (law-binder-type b))))

;; `(it-prop "<Class>: <law>" <property>)`.
(define (build-it-prop class-name law base key->sym)
  (define label (format "~a: ~a" class-name (class-law-name law)))
  (app base (var base 'it-prop)
       (list (lit base label)
             (build-property law base key->sym))))

;; A property over the law's binders.  One binder: drive its generator
;; directly.  Several: combine the per-binder generators with nested
;; `gen-pair`, destructure with a nested `Pair` pattern, then run the
;; (reused) body / the label render.
(define (build-property law base key->sym)
  (define binders (class-law-binders law))
  (define names   (map law-binder-name binders))
  (define gens    (map (lambda (b) (binder-gen-sym key->sym b)) binders))
  (define body    (freshen-ast (class-law-body law) base))
  (cond
    [(null? (cdr names))
     (define n (car names))
     (app base (var base 'for-all-gen)
          (list (lam base (list n) (label-string base names))
                (var base (car gens))
                (lam base (list n) body)))]
    [else
     (define p-sym (gensym 'p))
     (define pat (pair-pattern base names))
     (app base (var base 'for-all-gen)
          (list (lam base (list p-sym)
                     (match1 base (var base p-sym) pat (label-string base names)))
                (nest-gen base gens)
                (lam base (list p-sym)
                     (match1 base (var base p-sym) pat body))))]))

;; A right-nested `(gen-pair g0 (gen-pair g1 … gk))` over the per-binder
;; generator names.
(define (nest-gen base gens)
  (cond
    [(null? (cdr gens)) (var base (car gens))]
    [else (app base (var base 'gen-pair)
               (list (var base (car gens))
                     (nest-gen base (cdr gens))))]))

;; The matching `(Pair n0 (Pair n1 … nk))` destructuring pattern.
(define (pair-pattern base names)
  (cond
    [(null? (cdr names)) (p:var (car names) (fresh-stx base))]
    [else (p:ctor 'Pair
                  (list (p:var (car names) (fresh-stx base))
                        (pair-pattern base (cdr names)))
                  (fresh-stx base))]))

;; `"n0 = " ++ show n0 ++ ", n1 = " ++ show n1 ++ …` — each binder labeled
;; by its source name, so a failing case reads as a concrete assignment.
(define (label-string base names)
  (string-concat base
    (append*
     (for/list ([n (in-list names)] [i (in-naturals)])
       (append (if (> i 0) (list (lit base ", ")) '())
               (list (lit base (format "~a = " n))
                     (app base (var base 'show) (list (var base n)))))))))

(define (string-concat base exprs)
  (cond
    [(null? exprs) (lit base "")]
    [(null? (cdr exprs)) (car exprs)]
    [else (app base (var base 'string-append)
               (list (car exprs) (string-concat base (cdr exprs))))]))

;; ----- AST constructors (each leaf gets a fresh, distinct stx sharing
;; the class form's lexical context, so method-resolution keying by stx
;; identity never collapses two uses of the same class method) -----

(define (var base name)      (e:var name (fresh-stx base)))
(define (lit base v)         (e:literal v (fresh-stx base)))
(define (app base head args) (e:app head args (fresh-stx base)))
(define (lam base params b)  (e:lam params b (fresh-stx base)))
(define (match1 base scrut pat body)
  (e:match scrut (list (clause pat #f body (fresh-stx base))) #f (fresh-stx base)))

(define (list->ast base exprs)
  (cond
    [(null? exprs) (var base 'Nil)]
    [else (app base (var base 'Cons)
               (list (car exprs) (list->ast base (cdr exprs))))]))

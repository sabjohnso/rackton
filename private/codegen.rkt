#lang racket/base

;; Rackton — code generation.
;;
;; Translates surface AST (after type-checking) into syntax objects
;; that Racket can evaluate.  Type information has done its job by
;; this point and is erased; no runtime tags remain.
;;
;; Surface form  → Racket form
;;   e:literal v       → v
;;   e:var x           → x
;;   e:lam (p ...) b   → (lambda (p ...) b)
;;   e:app h (a ...)   → (h a ...)
;;   e:let ((x e) ...) → (let ((x e) ...) body)
;;   e:if c t e        → (if c t e)
;;   e:ann e _         → e  (ascription erased)
;;   e:match s ((p b) ...) → (match s (p b) ...)
;;
;;   top:def name e         → (define name e)
;;   top:dec _ _            → #f  (no runtime form)
;;   top:data _ _ (ctors)   → (begin (define-data-ctor C arity) ...)

(provide compile-expr
         compile-top)

(require racket/match
         (for-template (except-in racket/base
                                  + - * < > <= >= = compose
                                  not and or length foldr filter
                                  substring string-length string-append
                                  modulo quotient abs min max
                                  number->string string->number
                                  read-line print println
                                  reverse append sort
                                  file-exists?
                                  sqrt
                                  random getenv path->string
                                  delete-file make-directory
                                  char-upcase char-downcase
                                  char-alphabetic? char-numeric? char-whitespace?
                                  char->integer integer->char
                                  string-ref string->list
                                  bytes-length bytes-ref bytes-append
                                  bytes->list list->bytes make-bytes
                                  bytes->string/utf-8 string->bytes/utf-8
                                  string)
                       (except-in racket/match ==)
                       "adt.rkt"
                       "dict.rkt"
                       "prelude-runtime.rkt")
         "types.rkt"
         "env.rkt"
         "surface.rkt"
         "match.rkt"
         "infer.rkt")

;; Build a (possibly curried) lambda over `param-stxs` whose body is the
;; already-compiled `body-stx`.  For arity ≤ 1 we just emit
;; `(lambda (p ...) body)`.  For higher arity we emit a `case-lambda`
;; with N clauses: the full-arity clause runs the body directly, and
;; each shorter prefix returns a recursively-curried lambda over the
;; remaining parameters.  All clauses lexically capture the same body
;; syntax — runtime cost is one closure allocation per partial step.
(define (build-curried-lambda param-stxs body-stx ctx-stx)
  (cond
    [(<= (length param-stxs) 1)
     (with-syntax ([(p ...) param-stxs]
                   [bdy body-stx])
       (syntax/loc ctx-stx (lambda (p ...) bdy)))]
    [else
     (define n (length param-stxs))
     (define clauses
       (for/list ([k (in-range 1 (add1 n))])
         (define prefix (take-prefix param-stxs k))
         (define rest   (drop-prefix param-stxs k))
         (with-syntax ([(p ...) prefix])
           (cond
             [(null? rest)
              (with-syntax ([bdy body-stx])
                #'[(p ...) bdy])]
             [else
              (with-syntax ([inner (build-curried-lambda rest body-stx ctx-stx)])
                #'[(p ...) inner])]))))
     (with-syntax ([(clause ...) clauses])
       (syntax/loc ctx-stx (case-lambda clause ...)))]))

(define (take-prefix xs n)
  (cond [(or (zero? n) (null? xs)) '()]
        [else (cons (car xs) (take-prefix (cdr xs) (sub1 n)))]))

(define (drop-prefix xs n)
  (cond [(or (zero? n) (null? xs)) xs]
        [else (drop-prefix (cdr xs) (sub1 n))]))

(define (compile-expr e)
  (match e
    [(e:literal v stx)   (datum->syntax stx v stx)]
    [(e:var name stx)
     ;; Return-typed class methods have been resolved by inference
     ;; into per-instance impl names; consult the table.
     (define resolved
       (and (current-method-resolutions)
            (hash-ref (current-method-resolutions) stx #f)))
     (define final-name (datum->syntax stx (or resolved name) stx))
     ;; If this var carries a dict-resolution, wrap it in a variadic
     ;; closure that prepends the dict args at call time.  The closure
     ;; defers — calling `(wrapped x y)` becomes `(name dict... x y)`.
     ;; e:app heads see the same wrapper and call it normally; the
     ;; resulting double-call costs one extra closure invocation in
     ;; exchange for unified handling of bare-var and called positions.
     (define dict-impls
       (and (current-method-dict-resolutions)
            (hash-ref (current-method-dict-resolutions) stx #f)))
     (cond
       [(and dict-impls (not (null? dict-impls)))
        ;; Partial-apply the dict args.  For a 0-user-arg reference
        ;; like `get-state-t` this gives the value directly; for an
        ;; N-user-arg reference it gives a closure (Phase 17 currying
        ;; on the hand-written runtime impl handles the rest).
        (with-syntax ([head final-name]
                      [(d ...) (for/list ([sym (in-list dict-impls)])
                                 (datum->syntax stx sym stx))])
          (syntax/loc stx (head d ...)))]
       [else final-name])]

    [(e:lam params body stx)
     ;; A multi-parameter lambda compiles to a `case-lambda` whose
     ;; clauses cover every prefix arity from 1 to N.  This lets
     ;; consumers partially apply the function without making the
     ;; common full-arity call any slower (the first clause matches
     ;; and applies directly).  Zero- and single-parameter lambdas
     ;; pass through as plain `(lambda ...)`.
     (define param-stxs
       (for/list ([n (in-list params)])
         (datum->syntax stx n stx)))
     (define bdy-stx (compile-expr body))
     (build-curried-lambda param-stxs bdy-stx stx)]

    [(e:app head args stx)
     ;; The dict-prepending for needs-dict references is handled by
     ;; the e:var codegen above (it eta-wraps with the dict args), so
     ;; e:app stays simple here.  Phase 17 auto-currying makes
     ;; `((f dict) arg ...)` and `(f dict arg ...)` behave the same.
     (with-syntax ([h (compile-expr head)]
                   [(a ...) (for/list ([x (in-list args)]) (compile-expr x))])
       (syntax/loc stx (h a ...)))]

    [(e:let bindings body stx)
     (with-syntax
      ([(binding ...)
        (for/list ([b (in-list bindings)])
          (with-syntax ([x (datum->syntax stx (car b) stx)]
                        [r (compile-expr (cdr b))])
            #'(x r)))]
       [bdy (compile-expr body)])
       (syntax/loc stx (let (binding ...) bdy)))]

    [(e:letrec bindings body stx)
     (with-syntax
      ([(binding ...)
        (for/list ([b (in-list bindings)])
          (with-syntax ([x (datum->syntax stx (car b) stx)]
                        [r (compile-expr (cdr b))])
            #'(x r)))]
       [bdy (compile-expr body)])
       (syntax/loc stx (letrec (binding ...) bdy)))]

    [(e:if c t e stx)
     (with-syntax ([cc (compile-expr c)]
                   [tt (compile-expr t)]
                   [ee (compile-expr e)])
       (syntax/loc stx (if cc tt ee)))]

    [(e:ann expr _ _)
     (compile-expr expr)]

    [(e:escape _ty _vars body _stx)
     ;; Body is an opaque Racket syntax object with the user's lexical
     ;; context; splice verbatim.
     body]

    [(e:match scrut clauses _irrefutable? stx)
     (with-syntax
      ([sc (compile-expr scrut)]
       [(cl ...)
        (for/list ([c (in-list clauses)])
          (with-syntax ([pat (compile-pattern (clause-pattern c))]
                        [bd  (compile-expr (clause-body c))])
            (cond
              [(clause-guard c)
               (with-syntax ([gd (compile-expr (clause-guard c))])
                 #'[pat #:when gd bd])]
              [else #'[pat bd]])))])
       (syntax/loc stx (match sc cl ...)))]))

(define (compile-top form env)
  (match form
    [(top:dec _ _ _) #f]
    [(top:alias _ _ _ _) #f]
    [(top:def name expr stx)
     (with-syntax ([n (datum->syntax stx name stx)]
                   [e (compile-expr expr)])
       (syntax/loc stx (define n e)))]
    [(top:data tname tparams ctors stx)
     (with-syntax
      ([(ctor-form ...)
        (for/list ([c (in-list ctors)])
          (with-syntax ([nm  (datum->syntax stx (data-ctor-name c) stx)]
                        [arr (length (data-ctor-field-types c))])
            #'(define-data-ctor nm arr)))])
       (syntax/loc stx (begin ctor-form ...)))]
    [(top:class supers head methods stx)
     (compile-class head methods stx env)]
    [(top:instance ctx head methods stx)
     (compile-instance head methods stx env)]
    [(top:require specs stx)
     (with-syntax ([(s ...) specs])
       (syntax/loc stx (require s ...)))]))

;; ----- class & instance codegen ------------------------------------

;; A class compiles to one dispatch table per *method* — different
;; methods of the same class must dispatch into their own tables, or
;; later registrations would overwrite earlier ones.  Each method's
;; dispatch-position is read out of the class-info so that runtime
;; dispatch happens on the right argument.
(define (compile-class head methods stx env)
  (define class-name (constraint-class head))
  (define cinfo (env-ref-class env class-name))
  (define method-names
    (for/list ([m (in-list methods)] #:when (method-sig? m))
      (method-sig-name m)))
  (define defs
    (for/list ([n (in-list method-names)]
               #:unless (eq? (hash-ref (class-info-dispatchpos cinfo) n #f)
                             'return))
      (define base-pos   (hash-ref (class-info-dispatchpos cinfo) n 0))
      (define base-arity (method-arity cinfo n))
      ;; A needs-dict method gets extra leading arguments inserted at
      ;; the call site (one per return-typed method of each required
      ;; class).  Shift the runtime dispatch position and arity to
      ;; match what the wrapper will actually see.
      (define dict-arg-count
        (apply +
               (for/list ([req (in-list (hash-ref (class-info-dictreqs cinfo)
                                                  n '()))])
                 (length (dict-class-return-method-names (car req))))))
      (define pos (+ base-pos   dict-arg-count))
      (define ar  (+ base-arity dict-arg-count))
      (with-syntax ([meth     (datum->syntax stx n stx)]
                    [table    (datum->syntax stx
                                             (method-dispatch-symbol n) stx)]
                    [pos-stx  (datum->syntax stx pos stx)]
                    [ar-stx   (datum->syntax stx ar  stx)])
        #'(begin
            (define table (make-hasheq))
            (define-class-method meth table pos-stx ar-stx)))))
  (with-syntax ([(def ...) defs])
    (syntax/loc stx (begin def ...))))

;; Count the number of arrows in the method's body type — i.e. its
;; full arity — so the curry-dispatch wrapper knows when to stop
;; collecting and fire.
(define (method-arity cinfo method-name)
  (define sch (hash-ref (class-info-methods cinfo) method-name))
  (define body (qual-body-type (scheme-body sch)))
  (let loop ([t body] [n 0])
    (cond
      [(arrow? t) (loop (arrow-cod t) (add1 n))]
      [else n])))

;; An instance compiles to a sequence of `register-instance-method!`
;; calls — one per (method × tag) pair.
(define (compile-instance head methods stx env)
  (define head-pred-class (constraint-class head))
  (define head-arg-types
    (for/list ([a (in-list (constraint-args head))])
      (ty-ast->type a)))
  (define tags (tags-for-instance-head head-arg-types env))

  (define cinfo (env-ref-class env head-pred-class))
  (unless cinfo
    (error 'compile-instance "unknown class: ~s" head-pred-class))
  (define user-impls
    (for/fold ([acc '()]) ([m (in-list methods)])
      (match m
        [(top:def name expr _) (cons (cons name expr) acc)])))
  (define all-method-bodies
    (let loop ([rest (hash-keys (class-info-methods cinfo))]
               [acc  '()])
      (cond
        [(null? rest) acc]
        [else
         (define m (car rest))
         (define body
           (cond
             [(assq m user-impls) => cdr]
             [(hash-ref (class-info-defaults cinfo) m #f)
              => (lambda (default)
                   ;; A class default was originally parsed in the
                   ;; defining module's lexical context.  Relocate its
                   ;; syntax handles to the *instance* site so identifier
                   ;; references resolve via the user module's imports.
                   (relocate-ast default stx))]
             [else
              (error 'compile-instance
                     "no impl or default for ~s in instance ~s"
                     m head-pred-class)]))
         (loop (cdr rest) (cons (cons m body) acc))])))

  (define head-tcon-names
    (for/list ([t (in-list head-arg-types)]) (head-tcon-name t)))
  (define register-forms
    (apply
     append
     (for/list ([mb (in-list all-method-bodies)])
       (define name (car mb))
       (define body (cdr mb))
       (cond
         [(eq? (hash-ref (class-info-dispatchpos cinfo) name #f) 'return)
          ;; Return-typed methods don't dispatch on a runtime value;
          ;; emit one top-level `(define $method:Tcon impl)` whose name
          ;; matches what `infer.rkt` synthesizes in
          ;; current-method-resolutions.
          (with-syntax ([impl-name
                         (datum->syntax stx
                                        (return-impl-symbol name head-tcon-names)
                                        stx)]
                        [impl (compile-expr body)])
            (list #'(define impl-name impl)))]
         [else
          (for/list ([tag (in-list tags)])
            (with-syntax ([table   (datum->syntax stx
                                                  (method-dispatch-symbol name)
                                                  stx)]
                          [impl    (compile-expr body)]
                          [tag-sym (datum->syntax stx tag stx)])
              #'(register-instance-method! table 'tag-sym impl)))]))))
  (with-syntax ([(register ...) register-forms])
    (syntax/loc stx (begin register ...))))

;; Mirror of the dict-class-return-methods registry in
;; private/infer.rkt — kept terse and local so both phases can compute
;; the same dict-arg-count without sharing state.
(define (dict-class-return-method-names class-name)
  (case class-name
    [(Applicative) '(pure)]
    [(Monad)       '(pure)]
    [(Monoid)      '(mempty)]
    [else '()]))

(define (head-tcon-name t)
  (match t
    [(tcon n) n]
    [(tapp h _) (head-tcon-name h)]
    [_ (error 'compile-instance
              "instance head arg must be a concrete type, got ~v" t)]))

;; Build the impl name the codegen emits for a return-typed method on
;; an instance whose head args are `tcon-names`.  Must agree byte-for-
;; byte with the resolver in private/infer.rkt's `resolve-method-uses!`.
(define (return-impl-symbol method-name tcon-names)
  (string->symbol
   (format "$~a:~a"
           method-name
           (apply string-append
                  (let loop ([xs tcon-names])
                    (cond
                      [(null? xs) '()]
                      [(null? (cdr xs)) (list (symbol->string (car xs)))]
                      [else (cons (symbol->string (car xs))
                                  (cons "-" (loop (cdr xs))))]))))))

(define (method-dispatch-symbol method-name)
  (string->symbol (format "$dispatch:~a" method-name)))

;; Walk a surface AST, replacing every stx slot with `new-stx`.  Used
;; when applying a class's default method body inside an instance
;; defined in a different module: the body's identifiers must resolve
;; in the instance site's lexical scope, not the class's defining one.
(define (relocate-ast node new-stx)
  (define (R x) (relocate-ast x new-stx))
  (match node
    [(e:literal v _)     (e:literal v new-stx)]
    [(e:var n _)         (e:var n new-stx)]
    [(e:lam p body _)    (e:lam p (R body) new-stx)]
    [(e:app h args _)    (e:app (R h) (map R args) new-stx)]
    [(e:let bs body _)
     (e:let (for/list ([b (in-list bs)]) (cons (car b) (R (cdr b))))
            (R body) new-stx)]
    [(e:letrec bs body _)
     (e:letrec (for/list ([b (in-list bs)]) (cons (car b) (R (cdr b))))
               (R body) new-stx)]
    [(e:if a b c _)      (e:if (R a) (R b) (R c) new-stx)]
    [(e:ann e t _)       (e:ann (R e) (R t) new-stx)]
    [(e:escape t vs body _)
     ;; Escapes splice raw Racket syntax — relocating it would mean
     ;; rewriting that user-written code, which we don't want to do.
     (e:escape (R t) vs body new-stx)]
    [(e:match s cs irr? _)
     (e:match (R s)
              (for/list ([c (in-list cs)])
                (clause (R (clause-pattern c))
                        (and (clause-guard c) (R (clause-guard c)))
                        (R (clause-body c)) new-stx))
              irr? new-stx)]
    [(p:wild _)          (p:wild new-stx)]
    [(p:var n _)         (p:var n new-stx)]
    [(p:lit v _)         (p:lit v new-stx)]
    [(p:ctor n args _)   (p:ctor n (map R args) new-stx)]
    [(ty:var n _)        (ty:var n new-stx)]
    [(ty:con n _)        (ty:con n new-stx)]
    [(ty:app h args _)   (ty:app (R h) (map R args) new-stx)]
    [(ty:forall vs b _)  (ty:forall vs (R b) new-stx)]
    [(ty:qual cs b _)
     (ty:qual (for/list ([c (in-list cs)]) (R c)) (R b) new-stx)]
    [(constraint c args _) (constraint c (map R args) new-stx)]))

;; Convert a parsed type-AST to a core type, ignoring `All` and `qual`
;; wrappers.  Used here so we can inspect what the instance head names.
(define (ty-ast->type ast)
  (match ast
    [(ty:var n _) (tvar n)]
    [(ty:con n _) (tcon n)]
    [(ty:app h args _)
     (make-tapp (ty-ast->type h)
                (for/list ([a (in-list args)]) (ty-ast->type a)))]
    [(ty:forall _ body _) (ty-ast->type body)]
    [(ty:qual _ body _)   (ty-ast->type body)]))

;; Given a list of head-arg core types, return the dispatch tags for the
;; first one (single-parameter classes only in Phase 2).
(define (tags-for-instance-head head-arg-types env)
  (define t (car head-arg-types))
  (define head-tcon
    (match t
      [(tcon n) n]
      [(tapp (tcon n) _) n]
      [_
       (error 'tags-for-instance-head
              "instance head must be applied to a concrete type, got ~v" t)]))
  (cond
    [(eq? head-tcon 'Integer) '(Integer)]
    [(eq? head-tcon 'Boolean) '(Boolean)]
    [(eq? head-tcon 'String)  '(String)]
    [(eq? head-tcon 'Float)   '(Float)]
    [else
     (define ti (env-ref-tcon env head-tcon))
     (unless ti
       (error 'tags-for-instance-head
              "no tcon info for ~s when registering instance" head-tcon))
     (for/list ([c (in-list (tcon-info-ctors ti))])
       (string->symbol (format "$ctor:~a" c)))]))

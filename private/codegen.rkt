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
         compile-top
         current-codegen-env)

;; Phase 54: compile-top sets this to the post-inference env so
;; that compile-expr — which doesn't otherwise take env — can
;; consult it for things like a record's field-name list during
;; e:update lowering.
(define current-codegen-env (make-parameter #f))

(require racket/match
         racket/list
         racket/set
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
                                  string
                                  void when unless
                                  exp log sin cos tan
                                  numerator denominator
                                  real-part imag-part magnitude)
                       (except-in racket/match ==)
                       "adt.rkt"
                       "dict.rkt"
                       "prelude-runtime.rkt")
         "types.rkt"
         "env.rkt"
         "surface.rkt"
         "match.rkt"
         "entail.rkt"
         "infer.rkt")

;; Prepend `dict-arg-names` to an expression's outermost lambda
;; parameter list.  If `expr` is already an e:lam, return a new e:lam
;; with the dict args added in front; otherwise wrap `expr` in a fresh
;; e:lam that binds the dict args (so even a non-lambda RHS for a
;; needs-dict value becomes a function awaiting its dict).
(define (prepend-lambda-params expr dict-arg-names ctx-stx)
  (match expr
    [(e:lam params body stx)
     (e:lam (append dict-arg-names params) body stx)]
    [_
     (e:lam dict-arg-names expr ctx-stx)]))

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
     ;; Phase 58: when the head is a monomorphized class-method
     ;; reference whose impl was registered as inlinable AND the
     ;; arg count matches the impl's arity, substitute the body
     ;; via a `let` and skip the function call entirely.
     (cond
       [(try-inline-call head args stx)
        => (lambda (s) s)]
       [else
        (with-syntax ([h (compile-expr head)]
                      [(a ...) (for/list ([x (in-list args)]) (compile-expr x))])
          (syntax/loc stx (h a ...)))])]

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
       (syntax/loc stx (match sc cl ...)))]

    [(e:handle expr clauses ret stx)
     ;; Phase 55: lower (handle EXPR clauses... return) using
     ;; Racket's continuation prompts as a deep handler: the prompt
     ;; is re-installed each time the handler runs, so a resumption
     ;; inside the handler body can perform another op under a
     ;; fresh prompt of the same tag.
     (define env (current-codegen-env))
     (define eff-name
       (and (pair? clauses)
            (env-effect-of-op env (handle-clause-op (car clauses)))))
     (unless eff-name
       (raise-syntax-error 'compile
         "handle has no clauses or operations not from a known effect" stx))
     (define tag-id
       (datum->syntax stx
         (string->symbol (format "$effect-tag:~a" eff-name)) stx))
     (with-syntax
      ([tag tag-id]
       [body (compile-expr expr)]
       [v   (datum->syntax stx (handle-return-var ret) stx)]
       [ret-body (compile-expr (handle-return-body ret))]
       [(clause-form ...)
        (for/list ([cl (in-list clauses)])
          (define raw-params (handle-clause-params cl))
          (define compiled-params
            (cond [(null? raw-params) (list '_)]
                  [else raw-params]))
          (with-syntax ([op-sym (datum->syntax stx
                                               (handle-clause-op cl) stx)]
                        [k-name (datum->syntax stx
                                               (handle-clause-k-name cl) stx)]
                        [(p ...)
                         (for/list ([param (in-list compiled-params)])
                           (datum->syntax stx param stx))]
                        [cl-body (compile-expr (handle-clause-body cl))])
            #'[(list (quote op-sym) (list p ...) k-name)
               cl-body]))])
       (syntax/loc stx
         (letrec ([loop-handler
                   (lambda (thunk)
                     (call-with-continuation-prompt
                      thunk
                      tag
                      (lambda (msg)
                        (loop-handler
                         (lambda ()
                           (match (msg)
                             clause-form ...))))))])
           ;; The return clause is applied ONLY when the body
           ;; finishes normally; if it aborts via an op, the
           ;; handler's chosen clause body becomes the result
           ;; directly (no return wrapping).
           (loop-handler
            (lambda ()
              (let ([v body]) ret-body))))))]

    [(e:update record updates stx)
     ;; Phase 54: lower to Racket's `struct-copy` against the
     ;; underlying `$ctor:Name` struct.  The Rackton field name is
     ;; mapped to its positional `fN` slot via the env's
     ;; struct-fields table.
     (define env (current-codegen-env))
     (unless env
       (error 'compile-expr "no codegen env (Phase 54 e:update)"))
     (define type-head (e:update-target-type-head record env))
     (unless type-head
       (raise-syntax-error 'compile
         "could not statically determine record type for update" stx))
     (define field-names (env-ref-struct-fields env type-head))
     (unless field-names
       (raise-syntax-error 'compile
         (format "type ~s is not a record" type-head) stx))
     (define struct-id
       (datum->syntax stx
         (string->symbol (format "$ctor:~a" type-head))
         stx))
     (with-syntax ([s     struct-id]
                   [r-stx (compile-expr record)]
                   [(field-clause ...)
                    (for/list ([upd (in-list updates)])
                      (define idx (index-of field-names (car upd)))
                      (with-syntax ([f   (datum->syntax stx
                                           (string->symbol (format "f~a" idx))
                                           stx)]
                                    [v   (compile-expr (cdr upd))])
                        #'[f v]))])
       (syntax/loc stx (struct-copy s r-stx field-clause ...)))]))

;; Phase 58: attempt to inline a call.  If the head is an e:var
;; whose syntax was resolved to a monomorphized impl name and that
;; impl was registered as inlinable AND the arg count matches the
;; lambda's parameter count AND the same impl isn't already being
;; inlined on this expansion path, return a syntax object that
;; emits `(let ([p arg] ...) body)` in place of the call.
;; Otherwise #f.  The inlining-stack guard prevents an impl from
;; expanding into itself (recursive impl) — without it inlining
;; would loop at compile time.
(define current-inlining-stack (make-parameter (seteq)))

(define (try-inline-call head args stx)
  (cond
    [(not (e:var? head)) #f]
    [else
     (define resolutions (current-method-resolutions))
     (define inlinables  (current-inlinable-bodies))
     (cond
       [(or (not resolutions) (not inlinables)) #f]
       [else
        (define impl-name
          (hash-ref resolutions (e:var-stx head) #f))
        (define body
          (and impl-name (hash-ref inlinables impl-name #f)))
        (cond
          [(not body) #f]
          [(not (e:lam? body)) #f]
          [(not (= (length (e:lam-params body)) (length args))) #f]
          [(set-member? (current-inlining-stack) impl-name) #f]
          [else
           (define params (e:lam-params body))
           (define inner  (e:lam-body body))
           (when (current-inlined-sites)
             (define b (current-inlined-sites))
             (set-box! b (cons (cons (e:var-name head) impl-name)
                               (unbox b))))
           (parameterize ([current-inlining-stack
                           (set-add (current-inlining-stack) impl-name)])
             (with-syntax ([(p ...) (for/list ([n (in-list params)])
                                      (datum->syntax stx n stx))]
                           [(a ...) (for/list ([x (in-list args)])
                                      (compile-expr x))]
                           [body-stx (compile-expr inner)])
               (syntax/loc stx
                 (let ([p a] ...) body-stx))))])])]))

;; Phase 58: is this AST expression simple enough to inline at
;; concrete call sites?  Two criteria:
;;   - AST node count below a threshold (10 nodes here);
;;   - no class-method calls (e:var references whose name is a
;;     class method) so we don't risk pulling in a method whose
;;     impl depends on runtime dispatch.
;; Both rules are conservative — they're enough to catch the
;; common `(define (m x) (+ x 100))` shape without exploding.
(define INLINE-SIZE-LIMIT 10)

(define (inlinable-body? e)
  (< (ast-size e) INLINE-SIZE-LIMIT))

(define (ast-size e)
  (match e
    [(e:literal _ _) 1]
    [(e:var _ _) 1]
    [(e:lam ps b _) (+ 1 (length ps) (ast-size b))]
    [(e:app h args _)
     (+ 1 (ast-size h)
        (for/sum ([a (in-list args)]) (ast-size a)))]
    [(e:let bs b _)
     (+ 1
        (for/sum ([p (in-list bs)]) (ast-size (cdr p)))
        (ast-size b))]
    [(e:if c t e _) (+ 1 (ast-size c) (ast-size t) (ast-size e))]
    [(e:ann e _ _) (ast-size e)]
    [(e:match s cs _ _)
     (+ 1 (ast-size s)
        (for/sum ([c (in-list cs)])
          (+ (ast-size (clause-body c))
             (if (clause-guard c) (ast-size (clause-guard c)) 0))))]
    [_ 5]))

(define (contains-class-method-call? e)
  (define env (current-codegen-env))
  (cond
    [(not env) #f]
    [else
     (let loop ([e e])
       (match e
         [(e:literal _ _) #f]
         [(e:var name _) (and (env-ref-method-class env name) #t)]
         [(e:lam _ b _) (loop b)]
         [(e:app h args _)
          (or (loop h)
              (for/or ([a (in-list args)]) (loop a)))]
         [(e:let bs b _)
          (or (loop b)
              (for/or ([p (in-list bs)]) (loop (cdr p))))]
         [(e:if c t e _) (or (loop c) (loop t) (loop e))]
         [(e:ann e _ _) (loop e)]
         [(e:match s cs _ _)
          (or (loop s)
              (for/or ([c (in-list cs)])
                (or (loop (clause-body c))
                    (and (clause-guard c) (loop (clause-guard c))))))]
         [_ #f]))]))

;; Phase 54: derive the struct's type-head name from the record
;; expression.  We have to re-infer the record's type at codegen
;; time because expressions don't carry their inferred type on the
;; AST.  A minimal local re-inference under a fresh-state suffices
;; — the env we got is the same one inference saw.
(define (e:update-target-type-head record env)
  (with-handlers ([exn:fail? (lambda (_) #f)])
    (define-values (_s t) (infer-expr/fresh record env))
    (define t*
      (cond
        [(tvar? t) t]
        [else t]))
    (match t*
      [(tcon n) n]
      [(tapp (tcon n) _) n]
      [_ #f])))

(define (compile-top form env)
  (parameterize ([current-codegen-env env])
    (compile-top* form env)))

(define (compile-top* form env)
  (match form
    [(top:dec _ _ _) #f]
    [(top:alias _ _ _ _) #f]
    [(top:struct-fields _ _ _) #f]   ;; Phase 54: compile-time only

    [(top:effect ename ops stx)
     ;; Phase 55: compile an effect to a Racket prompt-tag + one
     ;; thunk per operation.  Each op-thunk takes its declared
     ;; args, captures the current continuation, and aborts to
     ;; the prompt with `(list 'op-name args k)`.  A 0-arg op was
     ;; promoted to take Unit; the user-facing call site passes
     ;; MkUnit and the op's compiled body ignores it.
     (define tag-id
       (datum->syntax stx
         (string->symbol (format "$effect-tag:~a" ename)) stx))
     (define op-defs
       (for/list ([o (in-list ops)])
         (define op-name (effect-op-name o))
         (define user-arity (length (effect-op-arg-types o)))
         (define compiled-arity
           (cond [(zero? user-arity) 1]
                 [else user-arity]))
         (define param-names
           (for/list ([i (in-range compiled-arity)])
             (string->symbol (format "$a~a" i))))
         (with-syntax
          ([op  (datum->syntax stx op-name stx)]
           [tag tag-id]
           [(p ...) (for/list ([n (in-list param-names)])
                      (datum->syntax stx n stx))])
          #'(define op
              (lambda (p ...)
                (call-with-composable-continuation
                 (lambda (k)
                   (abort-current-continuation tag
                     (lambda ()
                       (list (quote op) (list p ...) k))))
                 tag))))))
     (with-syntax ([tag tag-id]
                   [(op-def ...) op-defs])
       (syntax/loc stx
         (begin
           (define tag (make-continuation-prompt-tag (quote tag)))
           op-def ...)))]
    [(top:def name expr stx)
     ;; Phase 29: a needs-dict-body def has pre-allocated dict-arg
     ;; names recorded under current-needs-dict-defs.  Prepend them
     ;; to the RHS lambda's parameter list so the body's resolved
     ;; references to the locally-bound names actually find them.
     (define dict-args
       (and (current-needs-dict-defs)
            (hash-ref (current-needs-dict-defs) name #f)))
     (define expr*
       (cond
         [(and dict-args (not (null? dict-args)))
          (prepend-lambda-params expr dict-args stx)]
         [else expr]))
     (with-syntax ([n (datum->syntax stx name stx)]
                   [e (compile-expr expr*)])
       (syntax/loc stx (define n e)))]
    [(top:data tname tparams ctors stx _abstract?)
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
                 (length (dict-class-return-method-names (car req) env)))))
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
        [(top:def name expr _) (cons (cons name expr) acc)]
        ;; Phase 53: #:type bindings are compile-time only — no
        ;; runtime code is emitted for an associated-type binding.
        [(inst-type-fam _ _ _) acc])))
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

  (define head-tcon-names-raw
    (for/list ([t (in-list head-arg-types)]) (head-tcon-name t)))
  ;; Filter out tcons at fundep-determined positions so the per-method
  ;; impl name matches what `resolve-return-impl` synthesizes in
  ;; infer.rkt.  Single-param classes have no fundeps and pass through.
  (define head-tcon-names
    (cond
      [(null? (class-info-fundeps cinfo)) head-tcon-names-raw]
      [else
       (define determined
         (for/fold ([acc (seteq)])
                   ([fd (in-list (class-info-fundeps cinfo))])
           (set-union acc (list->seteq (cdr fd)))))
       (for/list ([p (in-list (class-info-params cinfo))]
                  [tn (in-list head-tcon-names-raw)]
                  #:unless (set-member? determined p))
         tn)]))
  (define register-forms
    (apply
     append
     (for/list ([mb (in-list all-method-bodies)])
       (define name (car mb))
       (define body (cdr mb))
       (cond
         [(eq? (hash-ref (class-info-dispatchpos cinfo) name #f) 'return)
          ;; Return-typed methods don't dispatch on a runtime value;
          ;; emit one top-level `(define $method:Tcon impl)` whose
          ;; name matches what `infer.rkt` synthesizes in
          ;; current-method-resolutions.  Phase 30: if the instance
          ;; is needs-dict, prepend dict-arg parameters so the impl
          ;; can accept them (and use them in the body via the
          ;; current-dict-skolems-driven local references).
          (define dict-pair
            (and (current-needs-dict-defs)
                 (hash-ref (current-needs-dict-defs)
                           (list head-pred-class
                                 (car head-tcon-names)
                                 name)
                           #f)))
          (define dict-args (combined-dict-args dict-pair))
          (define body*
            (cond
              [(and dict-args (not (null? dict-args)))
               (prepend-lambda-params body dict-args stx)]
              [else body]))
          (with-syntax ([impl-name
                         (datum->syntax stx
                                        (return-impl-symbol name head-tcon-names)
                                        stx)]
                        [impl (compile-expr body*)])
            (list #'(define impl-name impl)))]
         [else
          ;; Phase 30/39: positional class-method instance impls have
          ;; two kinds of dict-args, stored as a (inst-args .
          ;; method-args) pair under current-needs-dict-defs.  Phase 30
          ;; (instance-qual): the runtime wrapper can't insert these,
          ;; so we use compile-time inst-dispatch + a named impl.
          ;; Phase 39 (method-qual ONLY): the runtime wrapper already
          ;; inserts method-qual dicts at the call site (per class-
          ;; info-dictreqs at compile-class time), so we can register
          ;; the impl in the runtime dispatch table with method-qual
          ;; dicts as leading lambda params.
          (define dict-pair
            (and (current-needs-dict-defs)
                 (hash-ref (current-needs-dict-defs)
                           (list head-pred-class
                                 (car head-tcon-names)
                                 name)
                           #f)))
          (define inst-args   (and dict-pair (car dict-pair)))
          (define method-args (and dict-pair (cdr dict-pair)))
          (cond
            [(and inst-args (not (null? inst-args)))
             ;; Phase 30: instance-qual dicts present → named impl,
             ;; skip runtime registration.  Method-qual dicts (if
             ;; any) are prepended too so the impl's parameter list
             ;; matches what compile-time inst-dispatch supplies plus
             ;; what the runtime wrapper inserts.
             (define dict-args (append inst-args method-args))
             (define expr* (prepend-lambda-params body dict-args stx))
             (with-syntax ([impl-name (datum->syntax
                                       stx
                                       (return-impl-symbol name head-tcon-names)
                                       stx)]
                           [impl (compile-expr expr*)])
               (list #'(define impl-name impl)))]
            ;; Phase 37: for an overlap-group instance (any class
            ;; whose instance set has at least one pair related by
            ;; "strictly more specific"), emit a deep-fingerprint
            ;; impl name and skip runtime-table registration — two
            ;; instances with the same outer ctor would clobber each
            ;; other in the table, and call sites are routed to the
            ;; right fingerprint at compile time by inst-dispatch.
            [(env-class-has-overlap? env head-pred-class)
             (with-syntax ([impl-name
                            (datum->syntax
                             stx
                             (overlap-impl-symbol name head-arg-types)
                             stx)]
                           [impl (compile-expr body)])
               (list #'(define impl-name impl)))]
            [else
             ;; Phase 39: method-qual dicts (if any) become leading
             ;; lambda params of the runtime-registered impl.
             ;; Phase 57: emit a NAMED `(define $method:Tcon impl)`
             ;; alongside the dispatch-table registration so that
             ;; compile-time monomorphization at call sites can
             ;; reference the impl directly without going through
             ;; the table.
             (define body*
               (cond
                 [(and method-args (not (null? method-args)))
                  (prepend-lambda-params body method-args stx)]
                 [else body]))
             (define impl-name-sym
               (return-impl-symbol name head-tcon-names))
             ;; Phase 58: classify the body for inlining.  Only
             ;; full e:lam bodies whose inner expr is small and
             ;; calls no class methods get registered; the e:app
             ;; codegen reads this hash at call sites.
             (when (and (current-inlinable-bodies)
                        (e:lam? body*)
                        (inlinable-body? (e:lam-body body*)))
               (hash-set! (current-inlinable-bodies)
                          impl-name-sym
                          body*))
             (define def-form
               (with-syntax ([impl-name (datum->syntax stx impl-name-sym stx)]
                             [impl      (compile-expr body*)])
                 #'(define impl-name impl)))
             (define register-forms
               (for/list ([tag (in-list tags)])
                 (with-syntax ([table     (datum->syntax stx
                                                         (method-dispatch-symbol name)
                                                         stx)]
                               [impl-name (datum->syntax stx impl-name-sym stx)]
                               [tag-sym   (datum->syntax stx tag stx)])
                   #'(register-instance-method! table 'tag-sym impl-name))))
             (cons def-form register-forms)])]))))
  (with-syntax ([(register ...) register-forms])
    (syntax/loc stx (begin register ...))))

;; Mirror of the dict-class-return-methods registry in
;; private/infer.rkt — kept terse and local so both phases can compute
;; the same dict-arg-count without sharing state.
(define (dict-class-return-method-names class-name [env #f])
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
                   (dict-class-return-method-names (pred-class sp) env))))
        (remove-duplicates (append own super-methods))])]
    [else '()]))

;; Phase 39: current-needs-dict-defs entries for instance methods are
;; a (inst-args . method-args) cons.  Flatten into a single list for
;; consumers that don't care which group an arg came from.
(define (combined-dict-args entry)
  (cond
    [(not entry) '()]
    [else (append (car entry) (cdr entry))]))

(define (head-tcon-name t)
  (match t
    [(tcon n) n]
    [(tapp h _) (head-tcon-name h)]
    ;; Phase 31: a tvar at a fundep-determined head-arg position is
    ;; legitimate — return-impl-symbol consults class fundeps and drops
    ;; those positions, so we can safely report #f here.
    [_ #f]))

;; Phase 37: deep fingerprint of a head argument, encoding nested
;; ctors so e.g. (Box Integer) → "Box_Integer" and (Box a) → "Box_*".
;; Used as the impl-name suffix for instances in an overlap group so
;; two same-outer-ctor instances don't clobber each other.  Tvars
;; render as "*" — overlap means the specific instance always wins at
;; compile time for monomorphic call sites, and tvar positions in a
;; less-specific match show up as wildcards.
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

;; Phase 37: impl name for an overlap-group instance, using a
;; deep-fingerprint of each head arg.  Must agree byte-for-byte with
;; the resolver path in infer.rkt.
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

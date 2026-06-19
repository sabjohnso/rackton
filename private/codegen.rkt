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
         empty-cg-ctx
         ;; codegen state: the driver makes one, threads it across forms, and
         ;; reads the logs (inlined-sites, exported-impls) out of the final one.
         make-cg-st cg-st-inlined-sites cg-st-exported-impls
         ;; The per-method dispatch-table symbol ($dispatch:<method>).
         ;; The elaborator force-exports these for locally-defined
         ;; protocols so instances declared in other modules can register.
         method-dispatch-symbol)

;; The read-only codegen context, threaded explicitly through the lowering
;; pass (replacing the dynamically-scoped current-codegen-env + the plan-table
;; parameters).  Built once at compile-top from the env + codegen-plan.
(struct cg-ctx (env
                method-resolutions method-dict-resolutions
                needs-dict-defs instance-default-bodies
                return-typed-methods)
  #:transparent)
;; An empty context for isolated callers (tests) that lower a self-contained
;; expression with no resolutions to consult.
(define empty-cg-ctx (cg-ctx #f (hasheq) (hasheq) (hash) (hash) (seteq)))

;; The codegen working/log STATE, threaded up+down through the lowering
;; (replacing the dynamically-scoped current-inlinable-bodies /
;; current-inlined-sites / current-inlining-stack and the instance-exported
;; box).  inlined-sites / exported-impls are newest-first lists matching the
;; old box logs.  monomorphized-sites stays inference-side (codegen never reads
;; it), so it is not part of this state.
(struct cg-st (inlined-sites inlinable-bodies inlining-stack exported-impls)
  #:transparent)
(define (make-cg-st) (cg-st '() (hasheq) (seteq) '()))
(define (cg-record-inlined st method impl)
  (struct-copy cg-st st [inlined-sites (cons (cons method impl) (cg-st-inlined-sites st))]))
(define (cg-register-inlinable st impl-name body)
  (struct-copy cg-st st [inlinable-bodies (hash-set (cg-st-inlinable-bodies st) impl-name body)]))
(define (cg-lookup-inlinable st impl-name)
  (hash-ref (cg-st-inlinable-bodies st) impl-name #f))
(define (cg-add-exported st name)
  (struct-copy cg-st st [exported-impls (cons name (cg-st-exported-impls st))]))
(define (cg-st-add-inlining st impl-name)
  (struct-copy cg-st st [inlining-stack (set-add (cg-st-inlining-stack st) impl-name)]))
;; Compile a list of exprs left-to-right, threading st; returns (values
;; syntaxes st).
(define (compile-exprs es ctx st)
  (let loop ([es es] [acc '()] [st st])
    (cond
      [(null? es) (values (reverse acc) st)]
      [else
       (let-values ([(s st*) (compile-expr (car es) ctx st)])
         (loop (cdr es) (cons s acc) st*))])))

(require racket/match
         racket/list
         racket/set
         racket/string
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
                                  symbol->string string->symbol
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
                       "prelude-runtime.rkt"
                       "array-runtime.rkt")
         "types.rkt"
         "env.rkt"
         "surface.rkt"
         "match.rkt"
         "entail.rkt"
         "impl-symbols.rkt"
         "codegen-plan.rkt"
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
;; already-compiled `body-stx`.  For an N-parameter lambda (N ≥ 1) we emit
;; a `case-lambda` whose first N clauses cover every prefix arity 1..N:
;; the full-arity clause runs the body directly, and each shorter prefix
;; returns a recursively-curried lambda over the remaining parameters —
;; that is partial application.  A final rest-args clause makes
;; OVER-application curried as well: arguments beyond N are `apply`-ed to
;; the fully-applied body, so `(f a b c)` equals `(((f a) b) c)` even when
;; the surplus must flow into a function the body *returns*.  The type
;; checker already treats arrows as fully curried, so this keeps codegen
;; honest with it.  Ill-typed over-application never reaches the rest
;; clause: unification rejects applying a non-function at compile time, so
;; whenever the clause fires the body provably evaluates to a procedure.
;; Clause ordering matters — `case-lambda` prefers the exact-arity clause,
;; so the rest clause only fires for strictly more than N arguments (where
;; `more` is non-empty).  All clauses lexically capture the same body
;; syntax — runtime cost is one closure allocation per partial step and one
;; `apply` per over-application; exact calls pay neither.
(define (build-curried-lambda param-stxs body-stx ctx-stx)
  (cond
    [(zero? (length param-stxs))
     (with-syntax ([bdy body-stx])
       (syntax/loc ctx-stx (lambda () bdy)))]
    [else
     (define n (length param-stxs))
     (define fixed-clauses
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
     ;; `bdy` may be a `(begin (define …) … result)` that splices its
     ;; internal definitions into a definition context (e.g. a `(racket
     ;; …)` escape).  The fixed clauses provide that context directly;
     ;; the over-clause must restore it with `(let () bdy)` before the
     ;; result can be `apply`-ed, or the body's defines land in an
     ;; expression context and fail to compile.
     (define over-clause
       (with-syntax ([(p ...) param-stxs]
                     [bdy body-stx])
         #'[(p ... . more) (apply (let () bdy) more)]))
     (with-syntax ([(clause ...) (append fixed-clauses (list over-clause))])
       (syntax/loc ctx-stx (case-lambda clause ...)))]))

(define (take-prefix xs n)
  (cond [(or (zero? n) (null? xs)) '()]
        [else (cons (car xs) (take-prefix (cdr xs) (sub1 n)))]))

(define (drop-prefix xs n)
  (cond [(or (zero? n) (null? xs)) xs]
        [else (drop-prefix (cdr xs) (sub1 n))]))

;; Lower one expression.  Returns (values syntax st): the read-only ctx flows
;; down; the working/log state `st` threads through every child left-to-right.
(define (compile-expr e ctx st)
  (match e
    ;; Symbols are the one literal kind that isn't self-quoting: a bare
    ;; symbol datum lowers to an identifier (a variable reference), so a
    ;; Symbol literal must be wrapped in `quote`.
    [(e:literal (? symbol? v) stx) (values (datum->syntax stx (list 'quote v) stx) st)]
    [(e:literal v stx)   (values (datum->syntax stx v stx) st)]
    [(e:var name stx)
     ;; Return-typed class methods have been resolved by inference into
     ;; per-instance impl names; consult the table.  A dict-resolution wraps
     ;; the ref in a variadic closure that prepends the dict args at call time.
     (define resolved   (hash-ref (cg-ctx-method-resolutions ctx) stx #f))
     (define dict-impls (hash-ref (cg-ctx-method-dict-resolutions ctx) stx #f))
     ;; A concrete return-typed resolution names "$<name>:<tcon>"; route it
     ;; through the per-method runtime dispatch table so an instance defined in
     ;; another module is reachable.  A needs-dict body instead resolves to a
     ;; LOCAL dict-arg "$dict-<name>-<skolem>" — a direct ref, distinguished by
     ;; the "$<name>:" prefix.  No child exprs, so st is unchanged.
     (define return-prefix (string-append "$" (symbol->string name) ":"))
     (define concrete-return?
       (and resolved
            (set-member? (cg-ctx-return-typed-methods ctx) name)
            (string-prefix? (symbol->string resolved) return-prefix)))
     (values
      (cond
        [(and concrete-return? (or (not dict-impls) (null? dict-impls)))
         (compile-method-impl-ref resolved stx ctx)]
        [(and dict-impls (not (null? dict-impls)))
         (with-syntax ([head (emit-id (or resolved name) stx)]
                       [(d ...) (for/list ([sym (in-list dict-impls)])
                                  (compile-dict-impl sym stx ctx))])
           (syntax/loc stx (head d ...)))]
        [else (emit-id (or resolved name) stx)])
      st)]

    [(e:lam params body stx)
     ;; A multi-parameter lambda compiles to a curried `case-lambda` covering
     ;; every prefix arity; 0/1-param lambdas pass through as `(lambda ...)`.
     (define param-stxs
       (for/list ([n (in-list params)]) (emit-id n stx)))
     (let-values ([(bdy-stx st) (compile-expr body ctx st)])
       (values (build-curried-lambda param-stxs bdy-stx stx) st))]

    [(e:app head args stx)
     ;; Dict-prepending is handled in the e:var codegen, so e:app stays simple.
     ;; When the head's resolved impl was registered inlinable AND the arity
     ;; matches, try-inline-call substitutes the body and returns it directly.
     (let-values ([(inlined st) (try-inline-call head args stx ctx st)])
       (cond
         [inlined (values inlined st)]
         [else
          (let*-values ([(h st)  (compile-expr head ctx st)]
                        [(as st) (compile-exprs args ctx st)])
            (values (with-syntax ([h h] [(a ...) as]) (syntax/loc stx (h a ...)))
                    st))]))]

    [(e:let bindings body stx)
     (let*-values ([(rs st)  (compile-exprs (map cdr bindings) ctx st)]
                   [(bdy st) (compile-expr body ctx st)])
       (values
        (with-syntax ([(binding ...)
                       (for/list ([b (in-list bindings)] [r (in-list rs)])
                         (with-syntax ([x (emit-id (car b) stx)] [r r])
                           #'(x r)))]
                      [bdy bdy])
          (syntax/loc stx (let (binding ...) bdy)))
        st))]

    [(e:letrec bindings body stx)
     (let*-values ([(rs st)  (compile-exprs (map cdr bindings) ctx st)]
                   [(bdy st) (compile-expr body ctx st)])
       (values
        (with-syntax ([(binding ...)
                       (for/list ([b (in-list bindings)] [r (in-list rs)])
                         (with-syntax ([x (emit-id (car b) stx)] [r r])
                           #'(x r)))]
                      [bdy bdy])
          (syntax/loc stx (letrec (binding ...) bdy)))
        st))]

    [(e:if c t e stx)
     (let*-values ([(cc st) (compile-expr c ctx st)]
                   [(tt st) (compile-expr t ctx st)]
                   [(ee st) (compile-expr e ctx st)])
       (values (with-syntax ([cc cc] [tt tt] [ee ee]) (syntax/loc stx (if cc tt ee)))
               st))]

    [(e:ann expr _ _) (compile-expr expr ctx st)]

    [(e:escape _ty _vars body _stx)
     ;; Body is an opaque Racket syntax object with the user's lexical context.
     (values body st)]

    [(e:match scrut clauses _irrefutable? stx)
     (let*-values ([(sc st)  (compile-expr scrut ctx st)]
                   [(cls st) (compile-match-clauses clauses ctx st)])
       (values (with-syntax ([sc sc] [(cl ...) cls]) (syntax/loc stx (match sc cl ...)))
               st))]

    [(e:match* scrutinees clauses _irrefutable? stx)
     ;; Lower to Racket's `match*` for multi-value matching.
     (let*-values ([(scs st) (compile-exprs scrutinees ctx st)]
                   [(cls st) (compile-match*-clauses clauses ctx st)])
       (values (with-syntax ([(sc ...) scs] [(cl ...) cls])
                 (syntax/loc stx (match* (sc ...) cl ...)))
               st))]

    [(e:tuple elems stx)
     ;; Build through the representation helper rather than a raw vector
     ;; op, so the tuple's layout stays hidden in prelude-runtime.
     (let-values ([(es st) (compile-exprs elems ctx st)])
       (values (with-syntax ([(e ...) es])
                 (syntax/loc stx (rackton-tuple-make e ...)))
               st))]

    [(e:tref tup idx stx)
     (let-values ([(t st) (compile-expr tup ctx st)])
       (values (with-syntax ([t t] [i idx])
                 (syntax/loc stx (rackton-tuple-ref t i)))
               st))]

    [(e:array elems stx)
     (let-values ([(es st) (compile-exprs elems ctx st)])
       (values (with-syntax ([(e ...) es])
                 (syntax/loc stx (rackton-array-from-list (list e ...))))
               st))]

    [(e:build-array n proc stx)
     (let-values ([(p st) (compile-expr proc ctx st)])
       (values (with-syntax ([n n] [p p])
                 (syntax/loc stx (rackton-array-make n p)))
               st))]

    [(e:aref ae idx stx)
     (let-values ([(a st) (compile-expr ae ctx st)])
       (values (with-syntax ([a a] [i idx])
                 (syntax/loc stx (rackton-array-ref a i)))
               st))]

    [(e:array-slice op idx ae stx)
     ;; take / drop lower to the array runtime; split is the Pair (a
     ;; 2-tuple) of a take and a drop, built with the tuple constructor.
     (let-values ([(a st) (compile-expr ae ctx st)])
       (values
        (with-syntax ([a a] [k idx])
          (case op
            [(take)  (syntax/loc stx (rackton-array-take a k))]
            [(drop)  (syntax/loc stx (rackton-array-drop a k))]
            [(split) (syntax/loc stx
                       (rackton-tuple-make (rackton-array-take a k)
                                           (rackton-array-drop a k)))]))
        st))]

    [(e:handle expr clauses ret stx)
     ;; Lower (handle EXPR clauses... return) using Racket continuation
     ;; prompts as a deep handler: the prompt is re-installed each time the
     ;; handler runs, so a resumption can perform another op under a fresh
     ;; prompt of the same tag.
     (define env (cg-ctx-env ctx))
     (define eff-name
       (and (pair? clauses)
            (env-effect-of-op env (handle-clause-op (car clauses)))))
     (unless eff-name
       (raise-syntax-error 'compile
         "handle has no clauses or operations not from a known effect" stx))
     (define tag-id
       (datum->syntax stx
         (string->symbol (format "$effect-tag:~a" eff-name)) stx))
     (let*-values ([(body st)         (compile-expr expr ctx st)]
                   [(ret-body st)      (compile-expr (handle-return-body ret) ctx st)]
                   [(clause-forms st)  (compile-handle-clauses clauses ctx stx st)])
       (values
        (with-syntax ([tag tag-id]
                      [body body]
                      [v   (datum->syntax stx (handle-return-var ret) stx)]
                      [ret-body ret-body]
                      [(clause-form ...) clause-forms])
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
              ;; The return clause is applied ONLY when the body finishes
              ;; normally; if it aborts via an op, the handler's chosen clause
              ;; body becomes the result directly (no return wrapping).
              (loop-handler
               (lambda ()
                 (let ([v body]) ret-body))))))
        st))]

    [(e:update record updates stx)
     ;; Lower to Racket's `struct-copy` against the `$ctor:Name` struct; the
     ;; Rackton field name maps to its positional `fN` slot via struct-fields.
     (define env (cg-ctx-env ctx))
     (unless env
       (error 'compile-expr "no codegen env for e:update"))
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
         (string->symbol (format "$ctor:~a" type-head)) stx))
     (let*-values ([(r-stx st) (compile-expr record ctx st)]
                   [(vals st)  (compile-exprs (map cdr updates) ctx st)])
       (values
        (with-syntax ([s     struct-id]
                      [r-stx r-stx]
                      [(field-clause ...)
                       (for/list ([upd (in-list updates)] [v (in-list vals)])
                         (define idx (index-of field-names (car upd)))
                         (with-syntax ([f (datum->syntax stx
                                            (string->symbol (format "f~a" idx)) stx)]
                                       [v v])
                           #'[f v]))])
          (syntax/loc stx (struct-copy s r-stx field-clause ...)))
        st))]))

;; ----- clause compilers (thread st through guards + bodies) ----------

(define (compile-match-clauses clauses ctx st)
  (let loop ([cs clauses] [acc '()] [st st])
    (cond
      [(null? cs) (values (reverse acc) st)]
      [else
       (define c (car cs))
       (let-values ([(bd st) (compile-expr (clause-body c) ctx st)])
         (define-values (cl st*)
           (cond
             [(clause-guard c)
              (let-values ([(gd st) (compile-expr (clause-guard c) ctx st)])
                (values (with-syntax ([pat (compile-pattern (clause-pattern c))]
                                      [bd bd] [gd gd])
                          #'[pat #:when gd bd])
                        st))]
             [else
              (values (with-syntax ([pat (compile-pattern (clause-pattern c))] [bd bd])
                        #'[pat bd])
                      st)]))
         (loop (cdr cs) (cons cl acc) st*))])))

(define (compile-match*-clauses clauses ctx st)
  (let loop ([cs clauses] [acc '()] [st st])
    (cond
      [(null? cs) (values (reverse acc) st)]
      [else
       (define c (car cs))
       (let-values ([(bd st) (compile-expr (clause*-body c) ctx st)])
         (define-values (cl st*)
           (cond
             [(clause*-guard c)
              (let-values ([(gd st) (compile-expr (clause*-guard c) ctx st)])
                (values (with-syntax ([(pat ...)
                                       (for/list ([p (in-list (clause*-patterns c))])
                                         (compile-pattern p))]
                                      [bd bd] [gd gd])
                          #'[(pat ...) #:when gd bd])
                        st))]
             [else
              (values (with-syntax ([(pat ...)
                                     (for/list ([p (in-list (clause*-patterns c))])
                                       (compile-pattern p))]
                                    [bd bd])
                        #'[(pat ...) bd])
                      st)]))
         (loop (cdr cs) (cons cl acc) st*))])))

(define (compile-handle-clauses clauses ctx stx st)
  (let loop ([cs clauses] [acc '()] [st st])
    (cond
      [(null? cs) (values (reverse acc) st)]
      [else
       (define cl (car cs))
       (define raw-params (handle-clause-params cl))
       (define compiled-params (cond [(null? raw-params) (list '_)] [else raw-params]))
       (let-values ([(cl-body st) (compile-expr (handle-clause-body cl) ctx st)])
         (define form
           (with-syntax ([op-sym (datum->syntax stx (handle-clause-op cl) stx)]
                         [k-name (emit-id (handle-clause-k-name cl) stx)]
                         [(p ...) (for/list ([param (in-list compiled-params)])
                                    (emit-id param stx))]
                         [cl-body cl-body])
             #'[(list (quote op-sym) (list p ...) k-name)
                cl-body]))
         (loop (cdr cs) (cons form acc) st))])))

;; Attempt to inline a call.  If the head is an e:var
;; whose syntax was resolved to a monomorphized impl name and that
;; impl was registered as inlinable AND the arg count matches the
;; lambda's parameter count AND the same impl isn't already being
;; inlined on this expansion path, return a syntax object that
;; emits `(let ([p arg] ...) body)` in place of the call.
;; Otherwise #f.  The inlining-stack guard prevents an impl from
;; expanding into itself (recursive impl) — without it inlining
;; would loop at compile time (the inlining-stack lives in cg-st now).

;; Returns (values result st): result is #f (no inline) or the substituted
;; syntax.  Threads st — the inlinable registry and inlining-stack are read
;; from it, the inlined-site recorded into it; the inlining-stack add is scoped
;; to the sub-compile (restored after) while the logs persist.
(define (try-inline-call head args stx ctx st)
  (cond
    [(not (e:var? head)) (values #f st)]
    [else
     (define impl-name
       (hash-ref (cg-ctx-method-resolutions ctx) (e:var-stx head) #f))
     (define body (and impl-name (cg-lookup-inlinable st impl-name)))
     (cond
       [(not body) (values #f st)]
       [(not (e:lam? body)) (values #f st)]
       [(not (= (length (e:lam-params body)) (length args))) (values #f st)]
       [(set-member? (cg-st-inlining-stack st) impl-name) (values #f st)]
       [else
        (define params (e:lam-params body))
        (define inner  (e:lam-body body))
        (define st1 (cg-record-inlined st (e:var-name head) impl-name))
        (define st2 (cg-st-add-inlining st1 impl-name))
        (let*-values ([(arg-stxs st3) (compile-exprs args ctx st2)]
                      [(body-stx st4) (compile-expr inner ctx st3)])
          (values
           (with-syntax ([(p ...) (for/list ([n (in-list params)])
                                    (datum->syntax stx n stx))]
                         [(a ...) arg-stxs]
                         [body-stx body-stx])
             (syntax/loc stx (let ([p a] ...) body-stx)))
           ;; Restore the inlining-stack to its pre-inline value; the logs in
           ;; st4 (inlined-sites, any nested registrations) carry forward.
           (struct-copy cg-st st4 [inlining-stack (cg-st-inlining-stack st)])))])]))

;; Is this AST expression simple enough to inline at
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
    [(e:match* ss cs _ _)
     (+ 1
        (for/sum ([s (in-list ss)]) (ast-size s))
        (for/sum ([c (in-list cs)])
          (+ (ast-size (clause*-body c))
             (if (clause*-guard c) (ast-size (clause*-guard c)) 0))))]
    [_ 5]))

;; Derive the struct's type-head name from the record
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

;; Lower one top-form.  The `plan` carries the inference results codegen
;; needs (see codegen-plan.rkt); they are re-established as the
;; codegen-internal parameters here, at the single codegen entry point, so
;; the deep lowering code reads them as before.  Defaults to the empty plan
;; for isolated callers that drive codegen without an inference pass.
;; Returns (values syntax st).  The driver threads `st` across the form list
;; and reads the logs (inlined-sites, exported-impls) out of the final one;
;; isolated callers can ignore both extra return positions.
(define (compile-top form env [plan empty-codegen-plan] [st (make-cg-st)])
  ;; Normalize #f channels (empty plan, or the REPL's #f return-typed-methods)
  ;; to empty tables so the lowering reads them without guards — matching the
  ;; old `(and (current-…) …)` behaviour where #f meant "empty".
  (compile-top* form
                (cg-ctx env
                        (or (codegen-plan-method-resolutions plan) (hasheq))
                        (or (codegen-plan-method-dict-resolutions plan) (hasheq))
                        (or (codegen-plan-needs-dict-defs plan) (hash))
                        (or (codegen-plan-instance-default-bodies plan) (hash))
                        (or (codegen-plan-return-typed-methods plan) (seteq)))
                st))

;; Returns (values syntax-or-#f st).  Only the def and instance arms touch st
;; (compile-expr / register-inlinable / exported-impls); the rest pass it
;; through unchanged.
(define (compile-top* form ctx st)
  (match form
    [(top:dec _ _ _) (values #f st)]
    [(top:alias _ _ _ _) (values #f st)]
    [(top:struct-fields _ _ _) (values #f st)]   ;; Compile-time only
    [(top:type-family _ _ _ _ _) (values #f st)] ;; Compile-time only
    [(top:type-instance _ _ _ _) (values #f st)] ;; Compile-time only
    [(top:data-family _ _ _ _) (values #f st)]   ;; type only — no runtime form
    [(top:data-instance _ _ ctors stx)
     ;; Each instance constructor lowers to a struct, like a data ctor.
     (values
      (with-syntax
       ([(ctor-form ...)
         (for/list ([c (in-list ctors)])
           (with-syntax ([nm  (datum->syntax stx (data-ctor-name c) stx)]
                         [arr (length (data-ctor-field-types c))])
             #'(define-data-ctor nm arr)))])
        (syntax/loc stx (begin ctor-form ...)))
      st)]

    [(top:effect ename ops stx)
     ;; Compile an effect to a Racket prompt-tag + one
     ;; thunk per operation.  Each op-thunk takes its declared
     ;; args, captures the current continuation, and aborts to
     ;; the prompt with `(list 'op-name args k)`.  A 0-arg op was
     ;; promoted to take Unit; the user-facing call site passes
     ;; Unit and the op's compiled body ignores it.
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
     (values
      (with-syntax ([tag tag-id]
                    [(op-def ...) op-defs])
        (syntax/loc stx
          (begin
            (define tag (make-continuation-prompt-tag (quote tag)))
            op-def ...)))
      st)]
    [(top:def name expr stx)
     ;; A needs-dict-body def has pre-allocated dict-arg
     ;; names recorded under current-needs-dict-defs.  Prepend them
     ;; to the RHS lambda's parameter list so the body's resolved
     ;; references to the locally-bound names actually find them.
     (define dict-args
       (hash-ref (cg-ctx-needs-dict-defs ctx) name #f))
     (define expr*
       (cond
         [(and dict-args (not (null? dict-args)))
          (prepend-lambda-params expr dict-args stx)]
         [else expr]))
     (let-values ([(e st) (compile-expr expr* ctx st)])
       (values (with-syntax ([n (datum->syntax stx name stx)] [e e])
                 (syntax/loc stx (define n e)))
               st))]
    [(top:data tname tparams ctors stx _abstract? _runtime-tag)
     (values
      (with-syntax
       ([(ctor-form ...)
         (for/list ([c (in-list ctors)])
           (with-syntax ([nm  (datum->syntax stx (data-ctor-name c) stx)]
                         [arr (length (data-ctor-field-types c))])
             #'(define-data-ctor nm arr)))])
        (syntax/loc stx (begin ctor-form ...)))
      st)]
    [(top:class supers head methods stx)
     (values (compile-class head methods stx ctx) st)]
    [(top:instance _ctx head methods stx)
     (compile-instance head methods stx ctx st)]
    [(top:require specs stx)
     (values
      (with-syntax ([(s ...) specs])
        (syntax/loc stx (require s ...)))
      st)]
    [(top:foreign name type mod-path racket-id stx)
     ;; Bind the Rackton name to the host binding via a renaming
     ;; only-in require.  Type info is erased; the declared type was the
     ;; (unchecked) trust boundary at inference time.
     (values
      (with-syntax ([mp  (datum->syntax stx mod-path stx)]
                    [rid (datum->syntax stx racket-id stx)]
                    [nm  (datum->syntax stx name stx)])
        (syntax/loc stx (require (only-in mp [rid nm]))))
      st)]
    [(top:foreign-c name type lib symbol arg-tags result-tag io? stx)
     ;; Bind `name` to the C function via the ffi-runtime helper, which
     ;; builds the function ctype from the tag lists and calls
     ;; get-ffi-obj.  ffi/unsafe stays in ffi-runtime; here we emit only
     ;; a require of the helper plus the binding.  The declared type was
     ;; the (unchecked) trust boundary at inference time.
     (with-syntax ([nm      (datum->syntax stx name stx)]
                   [lib-e   (datum->syntax stx lib stx)]
                   [sym-e   (datum->syntax stx symbol stx)]
                   [args-e  (datum->syntax stx (list 'quote arg-tags) stx)]
                   [res-e   (datum->syntax stx (list 'quote result-tag) stx)]
                   [io-e    (datum->syntax stx io? stx)]
                   [arity-e (datum->syntax stx (length arg-tags) stx)])
       (values
        (syntax/loc stx
          (begin
            (require (only-in rackton/private/ffi-runtime rackton-ffi-bind))
            (define nm (rackton-ffi-bind lib-e sym-e args-e res-e io-e arity-e))))
        st))]
    [(top:provide _ _)
     ;; The elaborator resolves the union of all provide-specs and
     ;; emits a single Racket-level (provide …) form afterwards, so
     ;; nothing to emit per-form.
     (values #f st)]))

;; ----- class & instance codegen ------------------------------------

;; A class compiles to one dispatch table per *method* — different
;; methods of the same class must dispatch into their own tables, or
;; later registrations would overwrite earlier ones.  Each method's
;; dispatch-position is read out of the class-info so that runtime
;; dispatch happens on the right argument.
(define (compile-class head methods stx ctx)
  (define env (cg-ctx-env ctx))
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
  ;; Return-typed methods get a dispatch table too — but no
  ;; define-class-method wrapper, since they don't dispatch on a runtime
  ;; argument.  Instances register their impls here; call sites look up
  ;; by the compile-time-resolved result-type tag (see compile-expr's
  ;; e:var branch).  Prelude classes are not compiled here — their
  ;; tables live in prelude-runtime.rkt — so this only fires for
  ;; user-declared classes with return-typed members.
  (define return-table-defs
    (for/list ([n (in-list method-names)]
               #:when (eq? (hash-ref (class-info-dispatchpos cinfo) n #f)
                           'return))
      (with-syntax ([table (datum->syntax stx (method-dispatch-symbol n) stx)])
        #'(define table (make-hasheq)))))
  (with-syntax ([(def ...) (append defs return-table-defs)])
    (syntax/loc stx (begin def ...))))

;; Eta-expand a value-form method body to `arity` parameters, deferring
;; its evaluation to call time.  Leaves already-lambda bodies untouched
;; (they defer on their own) and arity-0 bodies untouched (a value, not a
;; function — eta would wrongly turn it into a thunk).
(define (eta-expand-value-body body arity stx)
  (cond
    [(e:lam? body) body]
    [(<= arity 0)  body]
    [else
     (define params
       (for/list ([i (in-range arity)])
         (string->symbol (format "$eta~a" i))))
     (e:lam params
            (e:app body
                   (for/list ([p (in-list params)]) (e:var p stx))
                   stx)
            stx)]))

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
;; Keep only the elements of `values` (a list aligned with the class's
;; type parameters) at positions that are NOT functionally determined.
;; For a fundep class like (MonadState s m | m -> s) this drops `s`,
;; leaving the dispatchable `m`; single-param classes (no fundeps) pass
;; `values` through unchanged.  Shared by the positional-dispatch tag
;; computation and the per-method impl-name suffix.
(define (drop-fundep-determined cinfo values)
  (cond
    [(null? (class-info-fundeps cinfo)) values]
    [else
     (define determined
       (for/fold ([acc (seteq)]) ([fd (in-list (class-info-fundeps cinfo))])
         (set-union acc (list->seteq (cdr fd)))))
     (for/list ([p (in-list (class-info-params cinfo))]
                [v (in-list values)]
                #:unless (set-member? determined p))
       v)]))

;; Resolve each class method to its impl body for this instance: the
;; user-supplied impl if present, else the class default relocated to
;; the instance's lexical context, else an error.
(define (instance-method-bodies cinfo user-impls head-pred-class head-tcon stx ctx)
  (let loop ([rest (hash-keys (class-info-methods cinfo))]
             [acc  '()])
    (cond
      [(null? rest) acc]
      [else
       (define m (car rest))
       (define body
         (cond
           [(assq m user-impls) => cdr]
           ;; Inherited class default.  Inference already freshened it
           ;; to this instance's site and resolved its return-typed
           ;; method calls against this instance's carrier, recording
           ;; the freshened AST under current-instance-default-bodies.
           ;; Reuse that exact AST so the syntax handles its method
           ;; resolutions are keyed by match (relocating afresh would
           ;; produce different handles and the resolutions would miss,
           ;; leaving the call as a bare, unbound identifier).
           [(and head-tcon
                 (hash-ref (cg-ctx-instance-default-bodies ctx)
                           (list head-pred-class head-tcon m) #f))
            => values]
           [(hash-ref (class-info-defaults cinfo) m #f)
            => (lambda (default)
                 ;; Fallback (e.g. paths that didn't populate the
                 ;; channel): a class default was originally parsed in
                 ;; the defining module's lexical context.  Relocate its
                 ;; syntax handles to the *instance* site so identifier
                 ;; references resolve via the user module's imports.
                 (relocate-ast default stx))]
           [else
            (error 'compile-instance
                   "no impl or default for ~s in instance ~s"
                   m head-pred-class)]))
       (loop (cdr rest) (cons (cons m body) acc))])))

(define (compile-instance head methods stx ctx st)
  (define env (cg-ctx-env ctx))
  (define head-pred-class (constraint-class head))
  (define head-arg-types
    (for/list ([a (in-list (constraint-args head))])
      (ty-ast->type a)))

  (define cinfo (env-ref-class env head-pred-class))
  (unless cinfo
    (error 'compile-instance "unknown protocol: ~s" head-pred-class))

  ;; Positional dispatch keys on the class's DETERMINING parameter(s) —
  ;; for a fundep class like (MonadState s m | m -> s) that is `m` (the
  ;; monad), not the first arg `s` (which is fundep-determined and may be
  ;; a bare tvar).  Drop determined params so tags-for-instance-head sees
  ;; the dispatchable arg.  (Single-param classes: no fundeps, unchanged.)
  ;; A class with no value-level methods (only an associated type, e.g. a
  ;; per-address Γ table) registers nothing at runtime, so the dispatch
  ;; tags are unused — and its head may be a non-tcon literal like
  ;; `(CodeAt 0)`, which `tags-for-instance-head` cannot tag.  Skip it.
  (define tags
    (cond
      [(hash-empty? (class-info-methods cinfo)) '()]
      [else
       (tags-for-instance-head (drop-fundep-determined cinfo head-arg-types) env)]))
  (define user-impls
    (for/fold ([acc '()]) ([m (in-list methods)])
      (match m
        [(top:def name expr _) (cons (cons name expr) acc)]
        ;; #:type bindings are compile-time only — no
        ;; runtime code is emitted for an associated-type binding.
        [(inst-type-fam _ _ _) acc])))
  ;; Filter out tcons at fundep-determined positions so the per-method
  ;; impl name matches what `resolve-return-impl` synthesizes in
  ;; infer.rkt.  Single-param classes have no fundeps and pass through.
  (define head-tcon-names
    (drop-fundep-determined
     cinfo
     (for/list ([t (in-list head-arg-types)]) (head-tcon-name t))))
  (define all-method-bodies0
    (instance-method-bodies cinfo user-impls head-pred-class
                            (and (pair? head-tcon-names) (car head-tcon-names))
                            stx ctx))
  ;; Eta-expand point-free / value-form method bodies.  A method given as
  ;; `(define m some-fn)` (a bare value, not `(define (m args) …)`) emits
  ;; an EAGER reference to `some-fn`.  When `some-fn` is a top-level def
  ;; (codegen'd after instances), that forward-references it.  Wrapping
  ;; the body in a lambda of the method's declared arity —
  ;; `(lambda (a …) (some-fn a …))` — defers the reference to call time,
  ;; by which point every module binding exists.  Bodies that are already
  ;; lambdas (the inline form, and class defaults) are left untouched, as
  ;; are arity-0 return-typed values (e.g. `mempty`), which cannot be
  ;; eta-expanded — see ISSUES.org.
  (define all-method-bodies
    (for/list ([mb (in-list all-method-bodies0)])
      (cons (car mb)
            (eta-expand-value-body (cdr mb)
                                   (method-arity cinfo (car mb))
                                   stx))))

  ;; Cross-method tracking for the pure-via-witness deriver (approach
  ;; (1) for ExceptT): if any value-dispatched method of this instance
  ;; witness-routes its inner `pure`, and the instance also defines a
  ;; return-typed `pure`, we emit a register-pure-witness-deriver! so
  ;; nesting (ExceptT-over-ExceptT) and runtime dispatch resolve the
  ;; inner pure from a witness.
  (define witness-routed? (box #f))
  (define pure-impl-name  (box #f))
  (define-values (register-forms st-after)
    (for/fold ([acc '()] [st st]) ([mb (in-list all-method-bodies)])
      (define name (car mb))
      (define body (cdr mb))
      (define-values (forms st*)
        (cond
          [(eq? (hash-ref (class-info-dispatchpos cinfo) name #f) 'return)
           (compile-instance-return-method
            name body head-pred-class head-tcon-names pure-impl-name tags stx ctx st)]
          [else
           (compile-instance-positional-method
            name body cinfo head-pred-class head-tcon-names head-arg-types
            tags witness-routed? stx ctx st)]))
      (values (append acc forms) st*)))
  ;; If a value-dispatched method witness-routed its inner pure AND this
  ;; instance defines a return-typed `pure`, register a pure-via-witness
  ;; deriver for the type's ctor so nested stacks (e.g. ExceptT over
  ;; ExceptT) and runtime dispatch can reconstruct the inner pure from a
  ;; witness.  The deriver reuses the named `$pure:T` impl, feeding it
  ;; the once-unwrapped witness's inner pure.
  (define deriver-forms
    (cond
      [(and (unbox witness-routed?) (unbox pure-impl-name) (pair? tags))
       (with-syntax ([pure-impl (datum->syntax stx (unbox pure-impl-name) stx)]
                     [tag       (datum->syntax stx (car tags) stx)])
         (list
          #'(register-pure-witness-deriver! 'tag
              (lambda (w)
                (let ([ip (inner-pure-from-witness w)])
                  (lambda (a) (pure-impl ip a)))))))]
      [else '()]))
  (values
   (with-syntax ([(register ...) (append register-forms deriver-forms)])
     (syntax/loc stx (begin register ...)))
   st-after))

;; Compile a return-typed instance method (one whose class dispatch
;; position is 'return — e.g. pure/mempty).  Such methods don't dispatch
;; on a runtime value; we emit one named impl and, for plain (non-needs-
;; dict) instances, also register it in the per-method dispatch table so
;; cross-module call sites can find it.  Returns a list of forms.
(define (compile-instance-return-method
         name body head-pred-class head-tcon-names pure-impl-name tags stx ctx st)
  ;; Return-typed methods don't dispatch on a runtime value;
  ;; emit one top-level `(define $method:Tcon impl)` whose
  ;; name matches what `infer.rkt` synthesizes in
  ;; current-method-resolutions.  If the instance
  ;; is needs-dict, prepend dict-arg parameters so the impl
  ;; can accept them (and use them in the body via the
  ;; current-dict-skolems-driven local references).
  (define dict-pair
    (and (cg-ctx-needs-dict-defs ctx)
         (hash-ref (cg-ctx-needs-dict-defs ctx)
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
  (define impl-name-sym (return-impl-symbol name head-tcon-names))
  (let-values ([(impl st) (compile-expr body* ctx st)])
   (define def-form
    (with-syntax ([impl-name (datum->syntax stx impl-name-sym stx)]
                  [impl impl])
      #'(define impl-name impl)))
   (cond
    ;; needs-dict return-typed instance (e.g. a transformer's
    ;; pure): keep the bare define only.  Its call sites carry
    ;; dict args and resolve via the direct reference, not the
    ;; tag table — so the defining module must export it for
    ;; cross-module call sites to bind.
    [(and dict-args (not (null? dict-args)))
     (when (eq? name 'pure) (set-box! pure-impl-name impl-name-sym))
     (values (list def-form) (cg-add-exported st impl-name-sym))]
    ;; plain return-typed instance: ALSO register into the
    ;; per-method dispatch table so a call site in another module
    ;; can find it.  The tag is the impl-name suffix (after
    ;; "$<name>:"), matching what the call site extracts.
    [else
     (define tag-sym
       (string->symbol
        (substring (symbol->string impl-name-sym)
                   (+ 2 (string-length (symbol->string name))))))
     (define reg-form
       (with-syntax ([table     (datum->syntax stx (method-dispatch-symbol name) stx)]
                     [tag       (datum->syntax stx tag-sym stx)]
                     [impl-name (datum->syntax stx impl-name-sym stx)])
         #'(register-instance-method! table 'tag impl-name)))
     ;; A plain `pure` ALSO registers into the $pure-by-tag witness table,
     ;; keyed by the type's constructor tags, so the monad can serve as a
     ;; transformer base at runtime-dispatched sites (e.g. nested ExceptT,
     ;; where `pure-via-witness` reconstructs the inner pure from a value).
     ;; Restricted to `$ctor:` ADT tags — opaque/runtime-tag monads
     ;; (STM, IO) are hand-registered in prelude-runtime.
     (define witness-forms
       (if (eq? name 'pure)
           (for/list ([t (in-list tags)]
                      #:when (regexp-match? #rx"^[$]ctor:"
                                            (symbol->string t)))
             (with-syntax ([ctor-tag  (datum->syntax stx t stx)]
                           [impl-name (datum->syntax stx impl-name-sym stx)])
               #'(register-pure-impl! 'ctor-tag impl-name)))
           '()))
     (values (list* def-form reg-form witness-forms) st)])))

;; Compile a positional (value-dispatched) instance method.  These key
;; on a runtime argument's tag.  Handles three sub-cases: instance-qual
;; needs-dict (named impl + runtime dispatcher, possibly witness-routed),
;; overlap-group (fingerprinted name, no table), and the plain case
;; (named impl + table registration).  Returns a list of forms.
(define (compile-instance-positional-method
         name body cinfo head-pred-class head-tcon-names head-arg-types
         tags witness-routed? stx ctx st)
  (define env (cg-ctx-env ctx))
  ;; Positional class-method instance impls have two kinds of
  ;; dict-args, stored as a (inst-args . method-args) pair
  ;; under current-needs-dict-defs.  Two cases:
  ;;   - instance-qual: the runtime wrapper can't insert these
  ;;     dicts, so we use compile-time inst-dispatch + a named
  ;;     impl.
  ;;   - method-qual ONLY: the runtime wrapper already inserts
  ;;     method-qual dicts at the call site (per class-info-
  ;;     dictreqs at compile-class time), so we can register
  ;;     the impl in the runtime dispatch table with method-
  ;;     qual dicts as leading lambda params.
  (define dict-pair
    (and (cg-ctx-needs-dict-defs ctx)
         (hash-ref (cg-ctx-needs-dict-defs ctx)
                   (list head-pred-class
                         (car head-tcon-names)
                         name)
                   #f)))
  (define inst-args   (and dict-pair (car dict-pair)))
  (define method-args (and dict-pair (cdr dict-pair)))
  (cond
    [(and inst-args (not (null? inst-args)))
     ;; Instance-qual dicts present.  Emit the named impl
     ;; (dict-carrying) for compile-time inst-dispatch call
     ;; sites.  Method-qual dicts (if any) are prepended too so
     ;; the impl's parameter list matches what compile-time
     ;; inst-dispatch supplies plus what the runtime wrapper
     ;; inserts.
     (define dict-args (append inst-args method-args))
     (define expr* (prepend-lambda-params body dict-args stx))
     (define impl-name-sym (return-impl-symbol name head-tcon-names))
     (define st1 (cg-add-exported st impl-name-sym))
     (define-values (named-impl st2) (compile-expr expr* ctx st1))
     (define named-def
       (with-syntax ([impl-name (datum->syntax stx impl-name-sym stx)]
                     [impl named-impl])
         #'(define impl-name impl)))
     ;; ALSO register a runtime dispatcher so POLYMORPHIC call
     ;; sites (where the dispatch type is an abstract tvar, e.g.
     ;; `flatmap` inside a `(MonadState s m) =>` body) reach the
     ;; instance by runtime tag.
     ;;
     ;; Most positional method bodies resolve the inner monad's
     ;; impl by runtime dispatch on the inner value's tag, so
     ;; they don't use the instance-qual dicts — register the
     ;; plain body (carrying only method-qual dicts).  But
     ;; ExceptT's flatmap/catch-e DO use the inner `pure` (to
     ;; rewrap Ok/Err).  There the runtime dispatcher derives the
     ;; inner pure from the dispatch-arg WITNESS and routes
     ;; through the named dict-carrying impl.
     (define rt-body
       (cond
         [(and method-args (not (null? method-args)))
          (prepend-lambda-params body method-args stx)]
         [else body]))
     (define-values (rt-impl-stx st3) (compile-expr rt-body ctx st2))
     (define uses-inst-dict?
       (syntax-mentions-any? rt-impl-stx (list->seteq inst-args)))
     (define register-forms
       (cond
         [(not uses-inst-dict?)
          ;; plain body — inst-qual dicts unused (StateT/EnvT/
          ;; WriterT value-dispatched methods).
          (for/list ([tag (in-list tags)])
            (with-syntax ([table   (datum->syntax stx (method-dispatch-symbol name) stx)]
                          [tag-sym (datum->syntax stx tag stx)]
                          [rt-impl rt-impl-stx])
              #'(register-instance-method! table 'tag-sym rt-impl)))]
         [(andmap pure-dict-name? inst-args)
          ;; ExceptT-style: the body uses the inner monad's
          ;; `pure` (all inst dicts are pure).  At a runtime
          ;; dispatch there is no compile-time dict, so derive
          ;; the inner pure from the WITNESS — the arg whose
          ;; runtime tag is this instance's ctor — and route
          ;; through the named dict-carrying impl.  Picking the
          ;; witness by tag (not by position) sidesteps the
          ;; class-vs-runtime dispatch-index mismatch (flatmap).
          ;; (method-args must be empty here — no transformer-
          ;; stack method needs both a runtime-inserted method
          ;; dict and a witness-derived inst dict.)
          (set-box! witness-routed? #t)
          (define n      (length (e:lam-params body)))
          (define disp   (monad-dispatch-index cinfo name))
          (define params (for/list ([i (in-range n)])
                           (datum->syntax stx (string->symbol (format "$w~a" i)) stx)))
          (define witness (list-ref params disp))
          (with-syntax ([impl-name (datum->syntax stx impl-name-sym stx)]
                        [(p ...)   params]
                        [wit       witness])
            (for/list ([tag (in-list tags)])
              (with-syntax ([table   (datum->syntax stx (method-dispatch-symbol name) stx)]
                            [tag-sym (datum->syntax stx tag stx)]
                            [(ip ...) (for/list ([_ (in-list inst-args)]) #'ipv)])
                #'(register-instance-method! table 'tag-sym
                    (lambda (p ...)
                      (let ([ipv (inner-pure-from-witness wit)])
                        (impl-name ip ... p ...)))))))]
         [else
          (error 'compile-instance
                 "needs-dict value-dispatched method ~s uses a non-pure instance dict; witness derivation unsupported"
                 name)]))
     (values (cons named-def register-forms) st3)]
    ;; For an overlap-group instance (any class
    ;; whose instance set has at least one pair related by
    ;; "strictly more specific"), emit a deep-fingerprint
    ;; impl name and skip runtime-table registration — two
    ;; instances with the same outer ctor would clobber each
    ;; other in the table, and call sites are routed to the
    ;; right fingerprint at compile time by inst-dispatch.
    [(env-class-has-overlap? env head-pred-class)
     (define-values (impl st*) (compile-expr body ctx st))
     (values
      (with-syntax ([impl-name
                     (datum->syntax
                      stx
                      (overlap-impl-symbol name head-arg-types)
                      stx)]
                    [impl impl])
        (list #'(define impl-name impl)))
      st*)]
    [else
     ;; Method-qual dicts (if any) become leading lambda
     ;; params of the runtime-registered impl.  Emit a NAMED
     ;; `(define $method:Tcon impl)` alongside the dispatch-
     ;; table registration so that compile-time
     ;; monomorphization at call sites can reference the impl
     ;; directly without going through the table.
     (define body*
       (cond
         [(and method-args (not (null? method-args)))
          (prepend-lambda-params body method-args stx)]
         [else body]))
     (define impl-name-sym
       (return-impl-symbol name head-tcon-names))
     ;; Classify the body for inlining.  Only full e:lam bodies whose inner
     ;; expr is small and calls no class methods get registered into st; the
     ;; e:app codegen consults it at call sites.
     (define st1
       (if (and (e:lam? body*) (inlinable-body? (e:lam-body body*)))
           (cg-register-inlinable st impl-name-sym body*)
           st))
     (define-values (impl st2) (compile-expr body* ctx st1))
     (define def-form
       (with-syntax ([impl-name (datum->syntax stx impl-name-sym stx)]
                     [impl      impl])
         #'(define impl-name impl)))
     (define register-forms
       (for/list ([tag (in-list tags)])
         (with-syntax ([table     (datum->syntax stx
                                                 (method-dispatch-symbol name)
                                                 stx)]
                       [impl-name (datum->syntax stx impl-name-sym stx)]
                       [tag-sym   (datum->syntax stx tag stx)])
           #'(register-instance-method! table 'tag-sym impl-name))))
     (values (cons def-form register-forms) st2)]))

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

;; current-needs-dict-defs entries for instance methods are
;; a (inst-args . method-args) cons.  Flatten into a single list for
;; consumers that don't care which group an arg came from.
(define (combined-dict-args entry)
  (cond
    [(not entry) '()]
    [else (append (car entry) (cdr entry))]))

;; Does compiled syntax `s` mention any identifier whose symbol is in
;; `syms`?  Used to tell whether a value-dispatched method body actually
;; uses its instance-qual dict args (ExceptT's flatmap/catch-e do — to
;; rewrap via the inner `pure`; StateT/EnvT/WriterT's don't).
(define (syntax-mentions-any? s syms)
  (let loop ([s s])
    (cond
      [(identifier? s) (set-member? syms (syntax->datum s))]
      [(syntax? s)     (loop (syntax-e s))]
      [(pair? s)       (or (loop (car s)) (loop (cdr s)))]
      [(vector? s)     (loop (vector->list s))]
      [else            #f])))

;; A dict-arg name for an inner `pure` (vs mempty/get-st/…).  Only pure
;; dicts are derivable from a runtime witness (pure-via-witness).
(define (pure-dict-name? sym)
  (string-prefix? (symbol->string sym) "$dict-pure-"))

;; Head name (tcon or tvar) of a possibly-applied type; #f otherwise
;; (e.g. an arrow).  Used to locate a method's monad-value argument.
(define (type-head-name t)
  (match t
    [(tapp h _) (type-head-name h)]
    [(tcon n) n]
    [(tvar n) n]
    [_ #f]))

;; The RUNTIME dispatch position of a value-dispatched method: the index
;; of the first curried argument whose head matches the method's return-
;; type head (the monad/functor param `m`).  This is what the generic
;; dispatcher keys on at runtime — and differs from the class's
;; find-dispatch-pos for `flatmap` (whose first arg `(-> a (m b))`
;; merely MENTIONS m).  Codegen needs it to pick the witness argument
;; for an ExceptT-style needs-dict method's runtime registration.
(define (monad-dispatch-index cinfo method-name)
  (define sch  (hash-ref (class-info-methods cinfo) method-name))
  (define body (qual-body-type (scheme-body sch)))
  (define ret  (let loop ([t body]) (if (arrow? t) (loop (arrow-cod t)) t)))
  (define dpar (type-head-name ret))
  (let loop ([t body] [i 0])
    (cond
      [(not (arrow? t)) 0]
      [(and dpar (eq? (type-head-name (arrow-dom t)) dpar)) i]
      [else (loop (arrow-cod t) (add1 i))])))

(define (head-tcon-name t)
  (match t
    [(tcon n) n]
    [(tapp h _) (head-tcon-name h)]
    ;; A tvar at a fundep-determined head-arg position is
    ;; legitimate — return-impl-symbol consults class fundeps and drops
    ;; those positions, so we can safely report #f here.
    [_ #f]))

;; head-fingerprint, overlap-impl-symbol, and return-impl-symbol — the
;; instance impl-name contract shared with inference — live in
;; "impl-symbols.rkt" so the two stages cannot drift apart.

(define (method-dispatch-symbol method-name)
  (string->symbol (format "$dispatch:~a" method-name)))

;; Compile a reference to a resolved method-impl symbol.  A CONCRETE
;; return-typed impl — "$<method>:<tag>" whose <method> is return-typed —
;; is routed through the per-method runtime dispatch table so it crosses
;; module boundaries (Enabler A); everything else (positional per-instance
;; impls like $==:Integer, and local needs-dict dict-arg params like
;; $dict-mempty-$skolem, which have no ":") is emitted as a direct
;; reference.  Used at both the e:var call site and the dict-arg path.
(define (compile-method-impl-ref sym stx ctx)
  (define m (regexp-match #rx"^[$]([^:]+):(.+)$" (symbol->string sym)))
  (cond
    [(and m
          (set-member? (cg-ctx-return-typed-methods ctx) (string->symbol (cadr m))))
     (with-syntax ([table (datum->syntax stx (method-dispatch-symbol
                                              (string->symbol (cadr m))) stx)]
                   [tg    (datum->syntax stx (string->symbol (caddr m)) stx)]
                   [mn    (datum->syntax stx (string->symbol (cadr m)) stx)])
       (syntax/loc stx (lookup-return-method table 'tg 'mn)))]
    [else (datum->syntax stx sym stx)]))

;; Compile one dict-arg entry from current-method-dict-resolutions.  An
;; entry is either a bare impl symbol (a PLAIN impl — routed through the
;; table when it's a concrete return-typed method, so it crosses module
;; boundaries) or a LIST = a needs-dict impl applied to its sub-dict
;; args.  For the list case the head stays a direct reference (needs-dict
;; impls are prelude-provided and aren't table-registered), while each
;; sub-arg is compiled recursively so a concrete return-typed sub-dict
;; (e.g. the inner monad's $pure:IO) still routes through the table.
(define (compile-dict-impl x stx ctx)
  (cond
    [(symbol? x) (compile-method-impl-ref x stx ctx)]
    [(pair? x)
     (with-syntax ([h        (datum->syntax stx (car x) stx)]
                   [(a ...)  (for/list ([y (in-list (cdr x))])
                               (compile-dict-impl y stx ctx))])
       (syntax/loc stx (h a ...)))]
    [else (datum->syntax stx x stx)]))

;; Walk a surface AST, replacing every stx slot with `new-stx`.  Used
;; when applying a class's default method body inside an instance
;; defined in a different module: the body's identifiers must resolve
;; in the instance site's lexical scope, not the class's defining one.
;; relocate-ast lives in surface.rkt (shared AST utility) and is
;; required from there; it re-anchors a parsed AST's syntax handles to a
;; new site so class defaults / derived bodies resolve in the instance's
;; lexical context.

;; Convert a parsed type-AST to a core type, ignoring `All` and `qual`
;; wrappers.  Used here so we can inspect what the instance head names.
(define (ty-ast->type ast)
  (match ast
    [(ty:var n _) (tvar n)]
    [(ty:con n _) (tcon n)]
    [(ty:nat v _) (tnat v)]
    [(ty:app h args _)
     (make-tapp (ty-ast->type h)
                (for/list ([a (in-list args)]) (ty-ast->type a)))]
    [(ty:forall _ body _) (ty-ast->type body)]
    [(ty:qual _ body _)   (ty-ast->type body)]))

;; Given a list of head-arg core types, return the dispatch tags for the
;; first one (single-parameter classes only).
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
    ;; `Array` values are prefab `rkt-array` structs (see array-runtime);
    ;; an Array-headed instance (Functor / Comonad) registers under that
    ;; tag, which is what dispatch-tag returns for an array value.
    [(eq? head-tcon 'Array)   '(rkt-array)]
    ;; The function arrow has no tcon-info shell; a procedure value
    ;; dispatches as the `->` tycon (see dispatch-tag), so a `->`-headed
    ;; instance registers under that tag.
    [(eq? head-tcon '->)      '(->)]
    ;; `Pair` is the binary tuple: its values are vectors that
    ;; dispatch under the `Tuple` tag, so a `Pair`-headed instance
    ;; (e.g. Bifunctor / Prod) registers there, not under `$ctor:Pair`.
    [(eq? head-tcon 'Pair)    '(Tuple)]
    [else
     (define ti (env-ref-tcon env head-tcon))
     (unless ti
       (error 'tags-for-instance-head
              "no tcon info for ~s when registering instance" head-tcon))
     (cond
       ;; Opaque type whose runtime values carry a declared dispatch tag
       ;; (#:runtime-tag): register positional instance methods under it,
       ;; matching what dispatch-tag returns for those host values.
       [(tcon-info-runtime-tag ti)
        (list (tcon-info-runtime-tag ti))]
       [else
        (for/list ([c (in-list (tcon-info-ctors ti))])
          (string->symbol (format "$ctor:~a" c)))])]))

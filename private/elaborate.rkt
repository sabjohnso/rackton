#lang racket/base

;; Rackton — pipeline glue.
;;
;; (rackton form ...) is a macro that:
;;   1. Parses each surface form via `parse-top`.
;;   2. Type-checks the program via `infer-program`.
;;   3. Compiles type-checked forms to Racket syntax via `compile-top`.
;;   4. Splices the result into the surrounding module as `(begin ...)`.
;;
;; A type error is raised at compile time as a syntax error, with the
;; offending form's source location attached.

(provide rackton rackton/main)

(require (for-syntax racket/base
                     racket/match
                     syntax/parse
                     racket/list
                     "surface.rkt"
                     "infer.rkt"
                     "codegen.rkt"
                     "prelude.rkt"
                     "scheme-codec.rkt"
                     "env.rkt")
         ;; Runtime require so that the macro templates
         ;; below can reference set-rackton-monomorphized-log-
         ;; snapshot! (and rackton-monomorphized-sites) — Racket's
         ;; hygiene then makes the spliced identifier resolve to
         ;; the runtime binding here.
         "prelude-runtime.rkt")

;; Stable phase ordering of top-level forms for codegen emission.
;; The aim is to satisfy runtime dependencies between *side-effecting
;; module top-level forms* — chiefly, `define-instance` registers
;; into a dispatch table that `define-class` defines, so every class
;; must run before any instance.  Within a single phase, source
;; order is preserved (Racket's `sort` is stable), so user-visible
;; sequencing of effects (e.g. ordering of `define`s with
;; side-effecting RHS) doesn't shift.
;;
;; Phase order:
;;   0. requires            — pull in imports first.
;;   1. data types          — `define-data-ctor` calls; independent.
;;   2. struct-fields       — no codegen but kept for completeness.
;;   3. classes             — `(define table (make-hasheq))` per
;;                            method, registers a class-method
;;                            dispatch wrapper.
;;   4. instances           — `register-instance-method!` calls;
;;                            require class tables to exist.
;;   5. effects             — define a continuation-prompt-tag.
;;   6. defs                — module-level `define`s; lazy bindings.
;;   7. everything else     — aliases, decs, provides; mostly no
;;                            codegen.
(define-for-syntax (phase-sort-forms parsed)
  (define (phase f)
    (cond
      [(top:require? f) 0]
      [(top:data? f) 1]
      [(top:struct-fields? f) 2]
      [(top:class? f) 3]
      [(top:instance? f) 4]
      [(top:effect? f) 5]
      [(top:def? f) 6]
      [else 7]))
  ;; Defs need a stronger ordering than "source order within phase":
  ;; `(define use (helper 5))` evaluates `helper` immediately, so
  ;; `helper`'s `define` must run first.  Use the same SCC order
  ;; inference computed; defs in a single SCC are mutually
  ;; recursive (all-functions case) so source order within the SCC
  ;; suffices.
  (define def-order
    (let* ([sccs (def-scc-order parsed)]
           [linear (apply append sccs)])
      (for/hasheq ([n (in-list linear)] [i (in-naturals)])
        (values n i))))
  (define (rank f i)
    (cond
      [(top:def? f) (hash-ref def-order (top:def-name f) i)]
      [else i]))
  ;; Tag each form with its source position; for defs, override
  ;; the position with the dependency-derived rank.
  (define tagged
    (for/list ([f (in-list parsed)] [i (in-naturals)])
      (list (phase f) (rank f i) f)))
  (define sorted
    (sort tagged
          (lambda (a b)
            (cond
              [(< (car a) (car b)) #t]
              [(> (car a) (car b)) #f]
              [else (< (cadr a) (cadr b))]))))
  (for/list ([entry (in-list sorted)]) (caddr entry)))

;; Shared elaboration helper: returns
;;   (values compiled-syntax-list provide-stx
;;           bindings-data data-ctors-data tcons-data classes-data instances-data
;;           mono-log inline-log)
;; `provide-stx` is a syntax object for a single Racket-level
;; `(provide ...)` form (or #f when nothing is exported).
(define-for-syntax (rackton-elaborate forms-stx)
  (define parsed
    (parse-toplevel-list (syntax->list forms-stx)))
  ;; Return-typed class methods are resolved at compile time, with the
  ;; resolution communicated from inference to codegen via these two
  ;; hashtables.  Inference populates `current-method-uses` then
  ;; settles it into `current-method-resolutions` after each top-def's
  ;; constraints reduce; codegen consults the resolutions when
  ;; emitting `e:var` references.
  (parameterize ([current-method-uses             (make-hasheq)]
                 [current-method-resolutions      (make-hasheq)]
                 [current-method-dict-resolutions (make-hasheq)]
                 ;; equal?-keyed so we can use composite list keys
                 ;; for instance method lookups.  Symbol-
                 ;; keyed top-def names compare equal? fine too.
                 [current-needs-dict-defs         (make-hash)]
                 ;; Monomorphization log starts empty per elaborate,
                 ;; accumulates each resolved site.
                 [current-monomorphized-sites     (box '())]
                 ;; inlinable-bodies is populated by
                 ;; compile-instance; the inlined-sites log mirrors
                 ;; the monomorphization log but for actually
                 ;; substituted call sites.
                 [current-inlinable-bodies        (make-hasheq)]
                 [current-inlined-sites           (box '())])
    (define env (infer-program parsed prelude-env))
    ;; Compile each parsed form into Racket syntax.  The emission
    ;; order must respect runtime dependencies — `define-instance`
    ;; mutates a dispatch table created by `define-class`, so all
    ;; classes have to run before any instances.  Phase-sort with
    ;; `sort` (Racket's `sort` is stable): forms within a phase
    ;; preserve their source order so the user-visible execution
    ;; ordering of defs and side-effecting top-level expressions
    ;; doesn't change.
    (define parsed-ordered (phase-sort-forms parsed))
    (define compiled
      (filter values
              (for/list ([f (in-list parsed-ordered)])
                (compile-top f env))))
    ;; Emit a runtime form that publishes this elaborate's
    ;; monomorphization log via the codegen-exposed setter.  The
    ;; rackton-monomorphized-sites accessor returns this list so
    ;; tests can verify the optimization fired.
    (define mono-log (unbox (current-monomorphized-sites)))
    (define inline-log (unbox (current-inlined-sites)))
    ;; Pass the logs alongside compiled forms; the
    ;; rackton macro turns them into runtime forms.
    (define-values (final-compiled prov-stx bs dcs tcs cls insts)
      (elaborate-finish parsed env compiled))
    (values final-compiled prov-stx bs dcs tcs cls insts mono-log inline-log)))

;; ----- export resolution ----------------------------------------------
;;
;; Rackton's (provide spec ...) is parsed into top:provide AST nodes;
;; here we resolve every spec against the final env into four export
;; maps — local-name → external-name — for value bindings, data
;; constructors, type constructors, and classes.  Instances are not
;; gated by provide; they always escape (Haskell convention).

;; Walk the parsed list and accumulate the names that are *locally
;; defined* in this rackton block (as opposed to inherited from the
;; prelude or pulled in via require).  Used to expand
;; (all-defined-out) and to gate `(provide name)` lookups.
(define-for-syntax (collect-local-defs parsed)
  (define vars       (make-hasheq))
  (define data-ctors (make-hasheq))
  (define tcons      (make-hasheq))
  (define classes    (make-hasheq))
  (for ([f (in-list parsed)])
    (match f
      [(top:def name _ _)
       (hash-set! vars name #t)]
      [(top:data tname _ ctors _ _)
       (hash-set! tcons tname #t)
       (for ([c (in-list ctors)])
         (define cname (data-ctor-name c))
         (hash-set! vars cname #t)
         (hash-set! data-ctors cname #t))]
      [(top:class _ head methods _)
       (hash-set! classes (constraint-class head) #t)
       (for ([m (in-list methods)])
         (cond
           [(method-sig? m)     (hash-set! vars (method-sig-name m) #t)]
           [(method-default? m) (hash-set! vars (method-default-name m) #t)]))]
      [(top:alias name _ _ _)
       (hash-set! tcons name #t)]
      [(top:effect _ ops _)
       (for ([o (in-list ops)])
         (hash-set! vars (effect-op-name o) #t))]
      [_ (void)]))
  (values vars data-ctors tcons classes))

;; Resolve every (provide spec ...) form in `parsed` against the final
;; env.  Returns four mutable hashes — local-name → external-name —
;; covering vars, data-ctors, tcons, classes.
(define-for-syntax (resolve-provide-specs parsed env
                                          local-vars local-data-ctors
                                          local-tcons local-classes)
  (define export-vars       (make-hasheq))
  (define export-data-ctors (make-hasheq))
  (define export-tcons      (make-hasheq))
  (define export-classes    (make-hasheq))

  (define (resolve-category name)
    ;; Returns a symbol describing which export bucket `name`
    ;; resolves to — 'data-ctor, 'tcon, 'class, 'var — or #f if
    ;; the name doesn't exist anywhere.  Data-ctor takes precedence
    ;; over `var` because every ctor is also a value-level binding.
    (cond
      [(or (hash-ref local-data-ctors name #f)
           (env-ref-data env name #f)) 'data-ctor]
      [(or (hash-ref local-tcons name #f)
           (env-ref-tcon env name #f)) 'tcon]
      [(or (hash-ref local-classes name #f)
           (env-ref-class env name #f)) 'class]
      [(or (hash-ref local-vars name #f)
           (env-ref-var env name #f))  'var]
      [else #f]))

  (define (add-export local external src-stx)
    (case (resolve-category local)
      [(data-ctor)
       (hash-set! export-data-ctors local external)
       (hash-set! export-vars       local external)]
      [(tcon)
       (hash-set! export-tcons local external)]
      [(class)
       (hash-set! export-classes local external)]
      [(var)
       (hash-set! export-vars local external)]
      [else
       (raise-syntax-error 'provide
         (format "no binding named ~s in scope" local)
         src-stx)]))

  (define (add-all-defined-out)
    (for ([(n _) (in-hash local-vars)])       (hash-set! export-vars       n n))
    (for ([(n _) (in-hash local-data-ctors)]) (hash-set! export-data-ctors n n))
    (for ([(n _) (in-hash local-tcons)])      (hash-set! export-tcons      n n))
    (for ([(n _) (in-hash local-classes)])    (hash-set! export-classes    n n)))

  (define (add-data-out tname src-stx)
    (define ti (or (env-ref-tcon env tname #f)
                   (raise-syntax-error 'provide
                     (format "data-out: ~s is not a type constructor" tname)
                     src-stx)))
    (hash-set! export-tcons tname tname)
    (for ([cname (in-list (tcon-info-ctors ti))])
      (hash-set! export-data-ctors cname cname)
      (hash-set! export-vars       cname cname)))

  (define (add-class-out cname src-stx)
    (define ci (or (env-ref-class env cname #f)
                   (raise-syntax-error 'provide
                     (format "class-out: ~s is not a class" cname)
                     src-stx)))
    (hash-set! export-classes cname cname)
    (for ([(mname _) (in-hash (class-info-methods ci))])
      (hash-set! export-vars mname mname)))

  (define (add-struct-out sname src-stx)
    ;; A define-struct registers its field list in env-struct-fields;
    ;; the lone constructor shares the struct's name and the per-
    ;; field accessors are emitted as `Sname-fname` top:defs by
    ;; parse-struct-form.  (struct-out S) bundles all three.
    (define fields
      (or (env-ref-struct-fields env sname #f)
          (raise-syntax-error 'provide
            (format "struct-out: ~s is not a struct" sname)
            src-stx)))
    (hash-set! export-tcons       sname sname)
    (hash-set! export-data-ctors  sname sname)
    (hash-set! export-vars        sname sname)
    (for ([fname (in-list fields)])
      (define accessor
        (string->symbol (format "~a-~a" sname fname)))
      (hash-set! export-vars accessor accessor)))

  (define (remove-name name)
    ;; except-out: drop name from every category it appears in.
    ;; The drop is silent if the name isn't there — Racket's
    ;; except-out raises in that case, but mirroring that adds
    ;; complexity without practical benefit.
    (hash-remove! export-vars       name)
    (hash-remove! export-data-ctors name)
    (hash-remove! export-tcons      name)
    (hash-remove! export-classes    name))

  (define (process-spec spec)
    (syntax-parse spec
      #:datum-literals (all-defined-out data-out class-out struct-out rename-out except-out)
      [name:id
       (add-export (syntax->datum #'name) (syntax->datum #'name) #'name)]
      [(all-defined-out)
       (add-all-defined-out)]
      [(data-out tname:id)
       (add-data-out (syntax->datum #'tname) #'tname)]
      [(class-out cname:id)
       (add-class-out (syntax->datum #'cname) #'cname)]
      [(struct-out sname:id)
       (add-struct-out (syntax->datum #'sname) #'sname)]
      [(rename-out [old:id new:id] ...)
       (for ([o (in-list (syntax->list #'(old ...)))]
             [n (in-list (syntax->list #'(new ...)))])
         (add-export (syntax->datum o) (syntax->datum n) o))]
      [(except-out inner name:id ...)
       (process-spec #'inner)
       (for ([n (in-list (syntax->list #'(name ...)))])
         (remove-name (syntax->datum n)))]
      [_
       (raise-syntax-error 'provide
         "unsupported provide spec"
         spec)]))

  (for ([f (in-list parsed)])
    (when (top:provide? f)
      (for ([spec (in-list (top:provide-specs f))])
        (process-spec spec))))

  (values export-vars export-data-ctors export-tcons export-classes))

;; Build a single Racket-level `(provide …)` syntax form (or #f when
;; the export set is empty).  Renames become `(rename-out [l e])`;
;; bare names emit as identifiers.  Only value-level entries
;; (export-vars, which already includes ctor names) need to appear
;; in the Racket-level provide — type-only entries (tcons, classes)
;; have no Racket-level binding to export.
;;
;; `anchor-stx` is one of the user's own `(provide …)` form
;; syntaxes (or any user-written syntax at module level).  We use
;; it as the lexical context for synthesized names — without it, a
;; bare `(provide x)` emitted by the macro would carry the macro's
;; introduction scope and fail to bind to the user's `(define x …)`.
(define-for-syntax (build-racket-provide export-vars anchor-stx)
  (define entries
    (for/list ([(local external) (in-hash export-vars)])
      (cond
        [(eq? local external)
         (datum->syntax anchor-stx local anchor-stx)]
        [else
         (with-syntax ([l (datum->syntax anchor-stx local anchor-stx)]
                       [e (datum->syntax anchor-stx external anchor-stx)])
           #'(rename-out [l e]))])))
  (cond
    [(null? entries) #f]
    [else
     ;; Wrap the (provide ...) form itself in the anchor's lex
     ;; context.  Inner entries are already syntax objects with the
     ;; same context (built above via datum->syntax anchor-stx ...),
     ;; so datum->syntax leaves them in place.
     (datum->syntax anchor-stx (cons 'provide entries) anchor-stx)]))

(define-for-syntax (elaborate-finish parsed env compiled)
  (define-values (local-vars local-data-ctors local-tcons local-classes)
    (collect-local-defs parsed))
  (define-values (export-vars export-data-ctors export-tcons export-classes)
    (resolve-provide-specs parsed env
                           local-vars local-data-ctors
                           local-tcons local-classes))
  ;; Sidecar bindings — filtered by export-vars, with renames
  ;; reflected in the published name.
  (define export-bindings
    (for/list ([(local external) (in-hash export-vars)]
               #:when (env-ref-var env local #f)
               #:unless (env-ref-var prelude-env local #f))
      (cons external (scheme->sexp (env-ref-var env local)))))
  ;; Omit ctors whose owning type was declared with the #:abstract
  ;; flag — importers can still mention the TYPE in signatures
  ;; (it's exported via the tcons table) but they can't construct or
  ;; pattern-match.
  (define export-data-ctors-encoded
    (for/list ([(local external) (in-hash export-data-ctors)]
               #:when (env-ref-data env local #f)
               #:unless (env-ref-data prelude-env local #f)
               #:unless
               (let ([di (env-ref-data env local)])
                 (let ([ti (env-ref-tcon env (data-info-type-name di))])
                   (and ti (tcon-info-abstract? ti)))))
      (cons external (encode-data-info (env-ref-data env local)))))
  (define export-tcons-encoded
    (for/list ([(local external) (in-hash export-tcons)]
               #:when (env-ref-tcon env local #f)
               #:unless (env-ref-tcon prelude-env local #f))
      (cons external (encode-tcon-info (env-ref-tcon env local)))))
  (define export-classes-encoded
    (for/list ([(local external) (in-hash export-classes)]
               #:when (env-ref-class env local #f)
               #:unless (env-ref-class prelude-env local #f))
      (cons external (encode-class-info (env-ref-class env local)))))
  ;; Instances always escape.
  (define export-instances
    (apply append
           (for/list ([(class-name insts)
                       (in-hash (env-instance-table env))])
             (define prelude-insts
               (env-instances prelude-env class-name))
             (for/list ([inst (in-list insts)]
                        #:unless (member inst prelude-insts))
               (encode-instance-info class-name inst)))))
  ;; Anchor the generated (provide …) form on a user-written syntax
  ;; — the first (provide …) form the user wrote — so the emitted
  ;; identifiers carry the user's module scope rather than the
  ;; macro's introduction scope.  Without an anchor there's nothing
  ;; to emit, since an empty export set yields #f from
  ;; build-racket-provide anyway.
  (define provide-anchor
    (for/or ([f (in-list parsed)])
      (and (top:provide? f) (top:provide-stx f))))
  (define prov-stx
    (and provide-anchor (build-racket-provide export-vars provide-anchor)))
  (values compiled prov-stx
          export-bindings export-data-ctors-encoded
          export-tcons-encoded export-classes-encoded export-instances))

;; `(rackton form ...)` — embeddable form.  Splices the compiled forms
;; but does NOT emit a sidecar schemes submodule, so multiple
;; `(rackton ...)` invocations can coexist in a single Racket module.
(define-syntax (rackton stx)
  (syntax-parse stx
    [(_ form ...)
     (define-values (compiled prov-stx _b _d _t _c _i mono-log inline-log)
       (rackton-elaborate #'(form ...)))
     (define out-forms
       (cond [prov-stx (append compiled (list prov-stx))]
             [else compiled]))
     (with-syntax ([(out ...) out-forms]
                   [entries mono-log]
                   [inline-entries inline-log])
       (syntax/loc stx
         (begin (set-rackton-monomorphized-log-snapshot! 'entries)
                (set-rackton-inlined-log-snapshot! 'inline-entries)
                out ...)))]))

;; `(rackton/main form ...)` — top-of-module form used by `#lang
;; rackton`.  Emits the schemes submodule so importing modules can
;; recover the types via dynamic-require.
(define-syntax (rackton/main stx)
  (syntax-parse stx
    [(_ form ...)
     (define-values (compiled prov-stx bs dcs tcs cls insts _mono _inline)
       (rackton-elaborate #'(form ...)))
     (define at-module-level?
       (memq (syntax-local-context) '(module module-begin)))
     (define out-forms
       (cond [prov-stx (append compiled (list prov-stx))]
             [else compiled]))
     (with-syntax ([(out ...)    out-forms]
                   [bindings     (datum->syntax stx bs)]
                   [data-ctors   (datum->syntax stx dcs)]
                   [tcons        (datum->syntax stx tcs)]
                   [classes      (datum->syntax stx cls)]
                   [instances    (datum->syntax stx insts)])
       (cond
         [at-module-level?
          (syntax/loc stx
            (begin
              out ...
              (module+ rackton-schemes
                (provide rackton-bindings
                         rackton-data-ctors
                         rackton-tcons
                         rackton-classes
                         rackton-instances)
                (define rackton-bindings   'bindings)
                (define rackton-data-ctors 'data-ctors)
                (define rackton-tcons      'tcons)
                (define rackton-classes    'classes)
                (define rackton-instances  'instances))))]
         [else
          (syntax/loc stx (begin out ...))]))]))

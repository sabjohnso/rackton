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
                     "env.rkt"
                     ;; definition-site collection for the sidecar's
                     ;; rackton-defs table (go-to-definition, search)
                     (only-in "analyze.rkt" collect-defs defs-sidecar-datum)
                     ;; terminal-width detection, read at the phase the
                     ;; macro's inference runs (phase 1) so a type error
                     ;; raised here is rendered to the REPL's width.
                     "term.rkt")
         ;; Runtime require so that the macro templates
         ;; below can reference set-rackton-monomorphized-log-
         ;; snapshot! (and rackton-monomorphized-sites) — Racket's
         ;; hygiene then makes the spliced identifier resolve to
         ;; the runtime binding here.
         "prelude-runtime.rkt")

;; Stable phase ordering of top-level forms for codegen emission.
;; The aim is to satisfy runtime dependencies between *side-effecting
;; module top-level forms* — chiefly, `instance` registers
;; into a dispatch table that `protocol` defines, so every protocol
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
      [(top:foreign? f) 0]
      [(top:foreign-c? f) 0]
      [(top:data? f) 1]
      [(top:data-family? f) 1]
      [(top:data-instance? f) 1]   ; instance ctors are structs — define early
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

;; ----- user-defined macros: front-phase expansion --------------------
;;
;; A `(define-syntax …)` / `(define-syntax-rule …)` inside a Rackton block
;; introduces a real, hygienic Racket transformer.  Before parsing, drive
;; the Racket expander over the body: bind each macro in an internal-
;; definition context, expand every macro USE to core Rackton forms — one
;; transformer step at a time, so Rackton leaves and applications are not
;; lowered into Racket core syntax (`#%app`, `quote`, …) that `parse-top`
;; cannot read.  Finally α-rename the fresh binders a macro introduces so
;; the symbol-keyed parser cannot conflate them with user names (hygiene);
;; introduced *references* to real bindings (`+`, `let`, …) and all
;; user-written identifiers are left alone.  Macro-definition forms are
;; consumed here and never reach `parse-top`.
(define-for-syntax (macro-def-form? f)
  (define l (syntax->list f))
  (and l (pair? l) (identifier? (car l))
       (memq (syntax-e (car l))
             '(define-syntax define-syntax-rule define-syntaxes))))

(define-for-syntax (require-form? f)
  (define l (syntax->list f))
  (and l (pair? l) (identifier? (car l)) (eq? (syntax-e (car l)) 'require)))

;; Per-elaboration sinks, set up in `rackton-elaborate`:
;;   current-collected-macros — a box collecting (cons (listof name-sym) datum)
;;     for every macro DEFINED in this block, so a `#lang rackton` library can
;;     re-emit them in its sidecar for importers (cross-module macro export).
;;   current-had-macros — a box set to #t when any macro (defined here or
;;     imported via require) was processed, so the parser turns on hygiene.
(define-for-syntax current-collected-macros (make-parameter #f))
(define-for-syntax current-had-macros       (make-parameter #f))

(define-for-syntax (note-had-macros!)
  (let ([b (current-had-macros)]) (when b (set-box! b #t))))

(define-for-syntax (collect-macro! names datum)
  (let ([b (current-collected-macros)])
    (when b (set-box! b (cons (cons names datum) (unbox b))))))

(define-for-syntax (expand-user-macros forms)
  (cond
    ;; A block can only involve macros if it defines one or requires a module
    ;; that might export some.  Otherwise: identity, so non-macro programs are
    ;; byte-for-byte unaffected by this phase.
    [(not (or (ormap macro-def-form? forms) (ormap require-form? forms))) forms]
    [else
     (define ctx    (syntax-local-make-definition-context))
     (define ctx-id (list (gensym 'rackton-macros)))
     (define registered '())          ; bound macro-name identifiers

     ;; Bind a macro definition — written directly, produced by an expanding
     ;; macro-defining macro, or reconstructed from an imported library — into
     ;; the context, and register its name(s) so later uses expand.  When
     ;; `collect?`, also record it for this module's own sidecar so it can be
     ;; re-exported.
     (define (bind-macro-def! f #:collect? [collect? #f])
       (define f* (internal-definition-context-introduce ctx f 'add))
       (define ds (local-expand f* ctx-id (list #'define-syntaxes) ctx))
       (syntax-parse ds
         #:literals (define-syntaxes)
         [(define-syntaxes (name:id ...) rhs)
          (define ids (syntax->list #'(name ...)))
          (syntax-local-bind-syntaxes ids #'rhs ctx)
          (set! registered (append ids registered))
          (when collect?
            (collect-macro! (map syntax->datum ids) (syntax->datum f)))]
         [_ (void)]))

     ;; Import the macros a required Rackton library exports: each library's
     ;; `rackton-schemes` sidecar carries `rackton-macros` — a list of
     ;; (names . definition-datum).  Reconstruct each definition in the
     ;; importer's lexical context (so its references and the user's uses
     ;; resolve the same way) and bind it like a local macro.
     (define (import-macros-from! require-stx)
       (syntax-parse require-stx
         [(_ spec ...)
          (for ([spec-stx (in-list (syntax->list #'(spec ...)))])
            (define submod (require-spec->submod-spec spec-stx))
            (when submod
              (define entries
                (with-handlers ([exn:fail? (lambda (_) '())])
                  (dynamic-require submod 'rackton-macros)))
              (for ([entry (in-list entries)])
                (bind-macro-def!
                 (datum->syntax spec-stx (cdr entry) spec-stx)))))]))

     ;; Pass 0 — import macros from required libraries first.
     (for ([f (in-list forms)] #:when (require-form? f))
       (import-macros-from! f))

     ;; Pass 1 — bind every directly-written macro definition, so a macro may
     ;; be used before its textual definition, and collect it for re-export.
     (for ([f (in-list forms)] #:when (macro-def-form? f))
       (bind-macro-def! f #:collect? #t))

     ;; No macros after all (e.g. requires that export none): identity.
     (cond
       [(null? registered) forms]
       [else
        (note-had-macros!)

     ;; The transformer of `head` if it names one of our macros, else #f —
     ;; so the walk never mistakes a Rackton core form (`let`, `if`) or an
     ;; ordinary binding (`+`) for a macro.
     (define (user-macro-transformer head)
       (and (identifier? head)
            (ormap (lambda (m) (free-identifier=? m head)) registered)
            (syntax-local-value head (lambda () #f) ctx)))

     ;; Single-step expand any registered-macro use, recursing into the
     ;; result and into all sub-forms.  Core forms, applications, variables,
     ;; and literals pass through structurally — never lowered into Racket
     ;; core syntax.  Hygiene (renaming the binders a macro introduces, and
     ;; protecting its top-level references from use-site capture) is handled
     ;; downstream by the parser's scope-aware binding, keyed on the scopes
     ;; the expander leaves on these identifiers.
     (define (expand-walk stx)
       (define l (syntax->list stx))
       (cond
         ;; Never expand into a macro definition's template/body — it is
         ;; bound as a transformer, not lowered to Rackton core forms.
         [(macro-def-form? stx) stx]
         [(or (not l) (null? l)) stx]
         [else
          (define tx (user-macro-transformer (car l)))
          (cond
            [tx
             (expand-walk
              (syntax-local-apply-transformer tx (car l) 'expression ctx stx))]
            [else
             (datum->syntax stx (map expand-walk l) stx stx)])]))

     ;; Pass 2 — expand every non-definition form in order.  The ctx scope
     ;; is added so macro USES resolve; it is removed from the residual core
     ;; forms so top-level definition names keep the user's module context.
     ;; A use of a macro-defining macro expands into a fresh macro
     ;; definition: bind it here (so later forms see it) instead of emitting
     ;; it to parse-top.
     (define out '())
     (for ([f (in-list forms)] #:unless (macro-def-form? f))
       (define expanded
         (expand-walk (internal-definition-context-introduce ctx f 'add)))
       (cond
         [(macro-def-form? expanded)
          (bind-macro-def! expanded)]
         [else
          (set! out
                (cons (internal-definition-context-introduce ctx expanded 'remove)
                      out))]))
        (reverse out)])]))

;; Shared elaboration helper: returns
;;   (values compiled-syntax-list provide-stx
;;           bindings-data data-ctors-data tcons-data classes-data instances-data
;;           impls macros defs promoted-data mono-log inline-log)
;; `provide-stx` is a syntax object for a single Racket-level
;; `(provide ...)` form (or #f when nothing is exported).
(define-for-syntax (rackton-elaborate forms-stx)
  (define raw-forms (syntax->list forms-stx))
  ;; `macros-box` gathers this block's own macro definitions (for re-export in
  ;; the sidecar); `had-macros?-box` records whether any macro was defined OR
  ;; imported, so the parser turns on scope-aware binding hygiene only then.
  (define macros-box      (box '()))
  (define had-macros?-box (box #f))
  (define expanded-forms
    (parameterize ([current-collected-macros macros-box]
                   [current-had-macros       had-macros?-box])
      (expand-user-macros raw-forms)))
  (define parsed
    (parameterize ([current-hygiene? (unbox had-macros?-box)])
      (parse-toplevel-list expanded-forms)))
  ;; The inference→codegen resolution tables (method-resolutions,
  ;; method-dict-resolutions, needs-dict-defs, instance-default-bodies) are
  ;; now owned by `infer-program+forms`, which returns them in a
  ;; `codegen-plan` that `compile-top` consumes.  Only the cross-phase logs
  ;; and codegen accumulators are installed here.
  ;; The monomorphization log threads through the inference state and is
  ;; returned by infer-program+forms; the inlinable-bodies / inlined-sites /
  ;; exported-impls that codegen writes thread through cg-st.  No parameters.
  (let ()
    ;; infer-program also returns the post-expansion form list — every
    ;; `#:derive-supers` instance replaced by the plain instances it
    ;; synthesized.  Codegen and export resolution run over THIS list so
    ;; the synthesized superclass instances are lowered and escape.
    ;;
    ;; Render any type error to the terminal's width — but ONLY when
    ;; expanding interactively (`'top-level`: a REPL or a top-level
    ;; `eval`).  A `'module` context (batch `raco make`, a `#lang rackton`
    ;; file) keeps the fixed default, so compiled error text stays
    ;; reproducible.  Detection returns #f when there is no terminal and
    ;; no `COLUMNS`, leaving the default.
    (define-values (env parsed* plan mono-log)
      (let ([cols (and (eq? (syntax-local-context) 'top-level)
                       (detect-display-columns))])
        (parameterize ([current-type-columns
                        (if cols (columns->type-budget cols) (current-type-columns))])
          (infer-program+forms parsed prelude-env))))
    ;; Compile each parsed form into Racket syntax.  The emission
    ;; order must respect runtime dependencies — `instance`
    ;; mutates a dispatch table created by `protocol`, so all
    ;; protocols have to run before any instances.  Phase-sort with
    ;; `sort` (Racket's `sort` is stable): forms within a phase
    ;; preserve their source order so the user-visible execution
    ;; ordering of defs and side-effecting top-level expressions
    ;; doesn't change.
    (define parsed-ordered (phase-sort-forms parsed*))
    ;; The plan carries the inference→codegen tables; compile-top reads them.
    ;; The codegen working/log state (cg-st) threads across the form list.
    (define-values (compiled final-cgst)
      (for/fold ([acc '()] [cgst (make-cg-st)] #:result (values (reverse acc) cgst))
                ([f (in-list parsed-ordered)])
        (let-values ([(s cgst*) (compile-top f env plan cgst)])
          (values (if s (cons s acc) acc) cgst*))))
    ;; The monomorphization log (mono-log, from infer-program+forms above) and
    ;; the inlined-sites log (codegen-side, read out of the final cg-st) so
    ;; tests can verify the optimizations fired.
    (define inline-log (cg-st-inlined-sites final-cgst))
    ;; Pass the logs + the generated exported-impl names (from the final cg-st)
    ;; alongside the compiled forms.
    (define-values (final-compiled prov-stx bs dcs tcs cls insts impls macs defs prom tfs)
      (elaborate-finish parsed* env compiled (unbox macros-box)
                        (cg-st-exported-impls final-cgst)))
    (values final-compiled prov-stx bs dcs tcs cls insts impls macs defs prom tfs
            mono-log inline-log)))

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
      [(top:foreign name _ _ _ _)
       (hash-set! vars name #t)]
      [(top:foreign-c name _ _ _ _ _ _ _)
       (hash-set! vars name #t)]
      [(top:data tname _ ctors _ _ _)
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
                                          local-tcons local-classes
                                          local-macros)
  (define export-vars       (make-hasheq))
  (define export-data-ctors (make-hasheq))
  (define export-tcons      (make-hasheq))
  (define export-classes    (make-hasheq))
  (define export-macros     (make-hasheq))

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
      [(hash-ref local-macros name #f) 'macro]
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
      [(macro)
       (hash-set! export-macros local external)]
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
    (for ([(n _) (in-hash local-classes)])    (hash-set! export-classes    n n))
    (for ([(n _) (in-hash local-macros)])     (hash-set! export-macros     n n)))

  (define (add-data-out tname src-stx)
    (define ti (or (env-ref-tcon env tname #f)
                   (raise-syntax-error 'provide
                     (format "data-out: ~s is not a type constructor" tname)
                     src-stx)))
    (hash-set! export-tcons tname tname)
    (for ([cname (in-list (tcon-info-ctors ti))])
      (hash-set! export-data-ctors cname cname)
      (hash-set! export-vars       cname cname)))

  (define (add-protocol-out cname src-stx)
    (define ci (or (env-ref-class env cname #f)
                   (raise-syntax-error 'provide
                     (format "protocol-out: ~s is not a protocol" cname)
                     src-stx)))
    (hash-set! export-classes cname cname)
    (for ([(mname _) (in-hash (class-info-methods ci))])
      (hash-set! export-vars mname mname)))

  (define (add-struct-out sname src-stx)
    ;; A struct registers its field list in env-struct-fields;
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

  ;; (all-from-out M) — re-export every name M published.  M's exports
  ;; were already folded into `env` when this module's (require M) was
  ;; processed by inference, so adding M's published names to the export
  ;; maps makes the sidecar re-serialize their schemes (from env) and
  ;; makes build-racket-provide re-emit the value bindings (which resolve
  ;; through this module's own (require M)).  Instances escape regardless.
  (define (add-all-from-out mod-stx)
    (define submod-spec (require-spec->submod-spec mod-stx))
    (unless submod-spec
      (raise-syntax-error 'provide
        "all-from-out: unsupported module path" mod-stx))
    (define (published sym)
      (with-handlers ([exn:fail? (lambda (_) '())])
        (dynamic-require submod-spec sym)))
    (for ([e (in-list (published 'rackton-bindings))])
      (hash-set! export-vars (car e) (car e)))
    (for ([e (in-list (published 'rackton-data-ctors))])
      (hash-set! export-data-ctors (car e) (car e))
      (hash-set! export-vars       (car e) (car e)))
    (for ([e (in-list (published 'rackton-tcons))])
      (hash-set! export-tcons (car e) (car e)))
    (for ([e (in-list (published 'rackton-classes))])
      (hash-set! export-classes (car e) (car e)))
    ;; Force-exported needs-dict impls ($pure:ExceptT, $lift:StateT, …)
    ;; are codegen-only names with no scheme, so they live in their own
    ;; sidecar category rather than rackton-bindings.  Re-export them as
    ;; plain Racket value bindings (they resolve through this module's
    ;; own (require M)) so a downstream call site can bind the direct
    ;; reference through a chain of all-from-out re-exports.
    (for ([sym (in-list (published 'rackton-exported-impls))])
      (hash-set! export-vars sym sym)))

  (define (remove-name name src-stx)
    ;; except-out: drop name from every category it appears in.  Like
    ;; Racket's except-out, a name that the nested spec never exported
    ;; is a compile-time error (rather than a silent no-op) — so a typo
    ;; in an except-out list is caught instead of quietly exporting the
    ;; name it was meant to hide.
    (unless (or (hash-has-key? export-vars       name)
                (hash-has-key? export-data-ctors name)
                (hash-has-key? export-tcons      name)
                (hash-has-key? export-classes    name)
                (hash-has-key? export-macros     name))
      (raise-syntax-error 'provide
        (format "except-out: ~s is not in the nested provide spec" name)
        src-stx))
    (hash-remove! export-vars       name)
    (hash-remove! export-data-ctors name)
    (hash-remove! export-tcons      name)
    (hash-remove! export-classes    name)
    (hash-remove! export-macros     name))

  (define (process-spec spec)
    (syntax-parse spec
      #:datum-literals (all-defined-out all-from-out data-out protocol-out struct-out rename-out except-out)
      [name:id
       (add-export (syntax->datum #'name) (syntax->datum #'name) #'name)]
      [(all-defined-out)
       (add-all-defined-out)]
      [(all-from-out mod ...)
       (for ([m (in-list (syntax->list #'(mod ...)))])
         (add-all-from-out m))]
      [(data-out tname:id)
       (add-data-out (syntax->datum #'tname) #'tname)]
      [(protocol-out cname:id)
       (add-protocol-out (syntax->datum #'cname) #'cname)]
      [(struct-out sname:id)
       (add-struct-out (syntax->datum #'sname) #'sname)]
      [(rename-out [old:id new:id] ...)
       (for ([o (in-list (syntax->list #'(old ...)))]
             [n (in-list (syntax->list #'(new ...)))])
         (add-export (syntax->datum o) (syntax->datum n) o))]
      [(except-out inner name:id ...)
       (process-spec #'inner)
       (for ([n (in-list (syntax->list #'(name ...)))])
         (remove-name (syntax->datum n) n))]
      [_
       (raise-syntax-error 'provide
         "unsupported provide spec"
         spec)]))

  (for ([f (in-list parsed)])
    (when (top:provide? f)
      (for ([spec (in-list (top:provide-specs f))])
        (process-spec spec))))

  (values export-vars export-data-ctors export-tcons export-classes export-macros))

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

(define-for-syntax (elaborate-finish parsed env compiled collected-macros
                                     exported-impls-from-cg)
  (define-values (local-vars local-data-ctors local-tcons local-classes)
    (collect-local-defs parsed))
  ;; Macro names defined in this block (consumed by the front phase, so not in
  ;; `parsed`); needed so `provide` can name them and re-export their defs.
  (define local-macros (make-hasheq))
  (for ([entry (in-list collected-macros)])
    (for ([n (in-list (car entry))]) (hash-set! local-macros n #t)))
  (define-values (export-vars export-data-ctors export-tcons export-classes export-macros)
    (resolve-provide-specs parsed env
                           local-vars local-data-ctors
                           local-tcons local-classes
                           local-macros))
  ;; Force-export generated needs-dict return-typed impls (e.g.
  ;; $pure:StateT, $get-st:StateT).  They are codegen-only names (not
  ;; env vars), so this affects ONLY the Racket-level provide, not the
  ;; type sidecar — and lets an importing module's cross-module call
  ;; site bind the direct reference.  Like instances, these escape
  ;; regardless of the user's provide form.
  (define exported-impls (remove-duplicates exported-impls-from-cg))
  (for ([sym (in-list exported-impls)])
    (hash-set! export-vars sym sym))
  ;; Force-export the per-method dispatch tables ($dispatch:<method>) of
  ;; every protocol DEFINED in this module and named in its provide.
  ;; codegen's compile-class makes one such table per method but leaves
  ;; it module-private; exporting it lets an instance of this protocol
  ;; declared in ANOTHER module register its impls into the same table
  ;; (without it the cross-module instance fails with
  ;; "$dispatch:<method>: unbound identifier").  Restricted to LOCAL
  ;; classes — a re-exported protocol's tables live in, and are provided
  ;; by, its origin module.  Prelude protocols are excluded: their tables
  ;; live in prelude-runtime.rkt and are already in scope everywhere.
  (for ([(local _external) (in-hash export-classes)]
        #:when (hash-has-key? local-classes local)
        #:unless (env-ref-class prelude-env local #f))
    (define ci (env-ref-class env local))
    (for ([(mname _sig) (in-hash (class-info-methods ci))])
      (define sym (method-dispatch-symbol mname))
      (hash-set! export-vars sym sym)))
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
  ;; Exported macros for the sidecar: each provided macro name paired with its
  ;; definition datum (under its external name), so importers can reconstruct
  ;; and bind it.  A definition binding several names contributes one entry per
  ;; provided name.
  (define export-macros-encoded
    (apply append
           (for/list ([entry (in-list collected-macros)])
             (define datum (cdr entry))
             (for/list ([n (in-list (car entry))]
                        #:when (hash-ref export-macros n #f))
               (cons (hash-ref export-macros n) datum)))))
  ;; Definition sites for every exported name (under its external
  ;; name), so importers' tools can jump to the source.
  (define export-defs-encoded
    (defs-sidecar-datum (collect-defs parsed #f)
                        (append (hash->list export-vars)
                                (hash->list export-data-ctors)
                                (hash->list export-tcons)
                                (hash->list export-classes))))
  ;; DataKinds-promoted constructors (TInt, SCons, …) of the exported
  ;; data types, as name → kind.  Promotion is computed once, in the
  ;; defining module's `promote-data`; transporting the result lets an
  ;; importing module's kind checker enforce a promoted index instead of
  ;; treating it as a fresh (anything-goes) kind.  Gated exactly like the
  ;; value-level data-ctors above — a promoted ctor crosses iff its value
  ;; constructor does (owning type exported and non-abstract).
  (define export-promoted-encoded
    (for/list ([(local external) (in-hash export-data-ctors)]
               #:when (env-ref-promoted-ctor env local #f)
               #:when (env-ref-data env local #f)
               #:unless (env-ref-data prelude-env local #f)
               #:unless
               (let ([di (env-ref-data env local)])
                 (let ([ti (env-ref-tcon env (data-info-type-name di))])
                   (and ti (tcon-info-abstract? ti)))))
      (cons external (encode-kind-scheme (env-ref-promoted-ctor env local)))))
  ;; Standalone type families (Feature 1) declared in this module, as
  ;; name → encoded tyfam-info.  Transported in full (clauses + inferred
  ;; kind) so an importer reduces family applications exactly as here.
  ;; Prelude families (none today) are excluded; a re-declared local
  ;; family overwrites an imported one of the same name on the importer
  ;; side, where local registration runs after the require fold-in.
  (define export-tyfams-encoded
    (for/list ([(name info) (in-hash (env-tyfams env))]
               #:unless (env-ref-tyfam prelude-env name #f))
      (cons name (encode-tyfam-info info))))
  (values compiled prov-stx
          export-bindings export-data-ctors-encoded
          export-tcons-encoded export-classes-encoded export-instances
          exported-impls export-macros-encoded export-defs-encoded
          export-promoted-encoded export-tyfams-encoded))

;; `(rackton form ...)` — embeddable form.  Splices the compiled forms
;; but does NOT emit a sidecar schemes submodule, so multiple
;; `(rackton ...)` invocations can coexist in a single Racket module.
(define-syntax (rackton stx)
  (syntax-parse stx
    [(_ form ...)
     (define-values (compiled prov-stx _b _d _t _c _i _impls _macs _defs _prom _tfs
                              mono-log inline-log)
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
     (define-values (compiled prov-stx bs dcs tcs cls insts impls macs defs prom tfs
                              _mono _inline)
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
                   [instances    (datum->syntax stx insts)]
                   [impls        (datum->syntax stx impls)]
                   [macros       (datum->syntax stx macs)]
                   [defs-table   (datum->syntax stx defs)]
                   [promoted     (datum->syntax stx prom)]
                   [tyfams       (datum->syntax stx tfs)])
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
                         rackton-instances
                         rackton-exported-impls
                         rackton-macros
                         rackton-defs
                         rackton-promoted
                         rackton-tyfams)
                (define rackton-bindings        'bindings)
                (define rackton-data-ctors      'data-ctors)
                (define rackton-tcons           'tcons)
                (define rackton-classes         'classes)
                (define rackton-instances       'instances)
                (define rackton-exported-impls  'impls)
                (define rackton-macros          'macros)
                (define rackton-defs            'defs-table)
                (define rackton-promoted        'promoted)
                (define rackton-tyfams          'tyfams))))]
         [else
          (syntax/loc stx (begin out ...))]))]))

#lang racket/base

;; Rackton — interactive REPL kernel.
;;
;; Exposes a small state-machine interface so the loop can be driven
;; by tests as easily as by a live stdin.  Persistent state lives in
;; a `rackton-repl-state` value; each input form is processed by
;; `rackton-repl-step` which returns the next state plus a formatted
;; output string.
;;
;; The kernel reuses the same inference + codegen pipeline that the
;; `(rackton ...)` macro uses, but slices it one form at a time and
;; carries the full set of inference parameters between calls so a
;; later definition can see what an earlier one declared.

(provide rackton-repl-init
         rackton-repl-step
         rackton-repl-state?
         rackton-repl-run
         rackton-read-form
         rackton-parse-command-line
         rackton-repl-completions
         (rename-out [rackton-repl-state-quit? rackton-repl-quit?]))

(require racket/match
         racket/format
         racket/list
         racket/string
         (only-in racket/port with-output-to-string)
         (only-in racket/pretty pretty-format)
         "surface.rkt"
         "infer.rkt"
         "codegen.rkt"
         "codegen-plan.rkt"
         "prelude.rkt"
         "env.rkt"
         "types.rkt"
         "term.rkt"
         "repl-input.rkt"
         "repl-term.rkt"
         "repl-source.rkt"
         "repl-search.rkt")

;; ----- session state ----------------------------------------------

;; Wraps every piece of mutable infrastructure that the inference
;; pipeline expects to find through parameters.  The fresh counter and
;; pending-pred bag now live in the immutable `infer-state` that
;; infer-program/phases threads internally; hashes hold the resolution
;; tables that codegen consumes.  `nsp` is the live Racket namespace
;; that executes compiled code.
;; `macros` is the list of symbols this session has bound as user macros
;; (via `define-syntax` / `define-syntax-rule` / `define-syntaxes`).  The
;; transformer bindings themselves live in `nsp`; this list records which
;; heads the expansion walk should expand and turns the parser's hygiene
;; on once the session defines any macro.
;; `sources` maps each session-bound name to the input form(s) that
;; bound it, for the ,source command (see repl-source.rkt).
(struct rackton-repl-state
  (env declared
       sources
       infer-st
       nsp
       expr-counter
       macros
       quit?)
  #:transparent)

(define (rackton-repl-init)
  (define ns (make-base-namespace))
  (parameterize ([current-namespace ns])
    (namespace-require 'racket/base)
    (namespace-require 'rackton))
  (rackton-repl-state prelude-env
                      (hasheq)
                      (hasheq)
                      (make-infer-state)
                      ns
                      0
                      '()
                      #f))

;; ----- dispatch ----------------------------------------------------

(define (rackton-repl-step state input)
  (cond
    [(repl-command? input)   (handle-command state input)]
    [(macro-def-form? input) (handle-macro-def state input)]
    [else
     ;; Expand any session-macro uses first, then dispatch on the result:
     ;; a macro may expand to a top form (e.g. a `define`) or to an
     ;; expression, so the top-form/expression split must see the
     ;; expansion, not the raw input.  The expanded form is carried as
     ;; syntax so macro hygiene (scope-tagged binders) survives into the
     ;; parser; only its head datum is consulted to pick the handler.
     (define exp-stx (expand-session-macros state input))
     (cond
       [(top-form? (syntax->datum exp-stx)) (handle-top-input state exp-stx)]
       [else                                (handle-expr-input state exp-stx)])]))

;; A command is a leading-comma line, which reads as `(unquote word arg ...)`.
;; An `unquote` outside a quasiquote is never a valid Rackton form, so a
;; comma-prefixed line can never be mistaken for ordinary input.
(define (repl-command? form)
  (and (pair? form) (eq? (car form) 'unquote)))

(define (top-form? form)
  (and (pair? form)
       (memq (car form)
             '(define data newtype struct
                protocol instance define-alias
                : require))))

;; A macro-definition form binds a Racket transformer rather than a
;; Rackton value, so it is handled outside the parse/infer/codegen
;; pipeline: evaluated straight into the session namespace.
(define (macro-def-form? form)
  (and (pair? form)
       (memq (car form)
             '(define-syntax define-syntax-rule define-syntaxes))))

;; The macro name(s) a macro-definition form introduces.
(define (macro-def-names form)
  (match form
    [(list 'define-syntax (cons name _) _ ...)      (list name)] ; (define-syntax (m . args) body)
    [(list 'define-syntax (? symbol? name) _ ...)   (list name)] ; (define-syntax m expr)
    [(list 'define-syntax-rule (cons name _) _ ...) (list name)] ; (define-syntax-rule (m . pat) tmpl)
    [(list 'define-syntaxes (list names ...) _ ...) names]       ; (define-syntaxes (m ...) expr)
    [_ '()]))

;; ----- command handling -------------------------------------------

(define (handle-command state input)
  ;; `unquote` is special inside `match`'s quasiquote, so spell the command
  ;; shapes out with explicit `(list 'unquote …)` patterns.
  (match input
    [(list 'unquote)              (values state "")]         ; bare `,` — no-op
    [(list 'unquote 'geiser-no-values) (values state "")]    ; Geiser probe — no-op
    [(list 'unquote 'quit)        (quit state)]
    [(list 'unquote 'q)           (quit state)]
    [(list 'unquote 'clear)       (values (rackton-repl-init) "session cleared\n")]
    [(list 'unquote 'c)           (values (rackton-repl-init) "session cleared\n")]
    [(list 'unquote 'help)        (values state (help-text))]
    [(list 'unquote 'h)           (values state (help-text))]
    [(list 'unquote 'keys)        (values state (keys-text))]
    [(list 'unquote 'type expr)   (values state (show-type state expr))]
    [(list 'unquote 't    expr)   (values state (show-type state expr))]
    [(list 'unquote 'info name)   (values state (show-info state name))]
    [(list 'unquote 'i    name)   (values state (show-info state name))]
    [(list 'unquote 'source name) (values state (show-source state name))]
    [(list 'unquote 'src    name) (values state (show-source state name))]
    [(list 'unquote 'accepts ty)  (values state (show-accepts state ty))]
    [(list 'unquote 'a       ty)  (values state (show-accepts state ty))]
    [_ (values state
               (format "unknown command: ~a\n" (command->string input)))]))

(define (quit state)
  (values (struct-copy rackton-repl-state state [quit? #t]) ""))

;; Render a command datum back to the comma syntax the user typed, for
;; the "unknown command" message: `(unquote foo bar)` -> ",foo bar".
(define (command->string cmd)
  (string-append "," (string-join (map ~a (cdr cmd)) " ")))

(define (help-text)
  (string-append
   ",type EXPR   show inferred type of EXPR\n"
   ",info NAME   show what's bound to NAME\n"
   ",source NAME show the form that defined NAME\n"
   ",accepts TYPE list functions accepting an argument of TYPE\n"
   ",keys        editor key bindings (terminal sessions)\n"
   ",clear       reset the session to a fresh prelude env\n"
   ",quit        exit the REPL\n"
   ",help        this message\n"))

;; The structural editor's bindings, generated from the same keymap
;; table that drives dispatch (repl-term.rkt) — the two cannot drift.
(define (keys-text)
  (string-append
   "Terminal sessions use the structural (paredit) editor:\n"
   "typing (, [, \", Backspace, and C-d keeps the entry balanced.\n"
   (keymap-text)))

(define (show-type state expr-datum)
  (with-handlers
   ([exn:fail?
     (lambda (e) (format "error: ~a\n" (exn-message e)))])
   (define name (gensym '$repl-type-))
   ;; Expand session macros in the expression so `,type (a-macro …)`
   ;; types the expansion; splice the result as syntax to keep its scopes.
   (define exp-stx (expand-session-macros state expr-datum))
   (define synthetic (datum->syntax #f (list 'define name exp-stx)))
   (define-values (env* _declared* _compiled _final-st _parsed)
     (elaborate-form state synthetic))
   (define sch (env-ref-var env* name))
   (format "~s :: ~a\n" expr-datum (scheme->datum sch))))

;; List the functions (and data constructors) in scope that accept an
;; argument of the queried type.  Bare-type-variable argument
;; positions never match — see repl-search.rkt.
(define (show-accepts state type-datum)
  (with-handlers
   ([exn:fail?
     (lambda (e) (format "error: ~a\n" (exn-message e)))])
   (match (accepts-search (rackton-repl-state-env state) type-datum)
     ['bare-query
      (format "~s is a bare type variable — every function accepts it\n"
              type-datum)]
     ['()
      (format "no functions accept ~s\n" type-datum)]
     [matches
      (apply string-append
             (for/list ([m (in-list matches)])
               (format "~s :: ~a\n" (car m) (scheme->datum (cdr m)))))])))

;; Play back the input form(s) that bound `name`: the definition first,
;; then — for a class — the live instances the session has seen.
;; Prelude names show their definition in the prelude source.
(define (show-source state name)
  (define env (rackton-repl-state-env state))
  (define bound?
    (and (or (env-ref-var env name)
             (env-ref-data env name)
             (env-ref-tcon env name)
             (env-ref-class env name))
         #t))
  (match (sources-lookup (rackton-repl-state-sources state) name bound?)
    [#f (format "~s is unbound\n" name)]
    ['no-source (format "~s has no recorded source (imported or builtin)\n" name)]
    [ds (apply string-append
               (for/list ([d (in-list (reverse ds))])
                 (string-append (pretty-format d #:mode 'write) "\n")))]))

(define (show-info state name)
  (define env (rackton-repl-state-env state))
  (cond
    [(env-ref-var env name)
     => (lambda (sch) (format "~s :: ~a\n" name (scheme->datum sch)))]
    [(env-ref-data env name)
     => (lambda (di) (format "~s :: ~a (data ctor)\n"
                             name (scheme->datum (data-info-scheme di))))]
    [(env-ref-tcon env name)
     => (lambda (ti) (format-tcon-info env name ti))]
    [(env-ref-class env name)
     => (lambda (ci) (format-class-info env name ci))]
    [else (format "~s is unbound\n" name)]))

;; Render a class for ,info: parameters, superclasses, methods (each with
;; its scheme), and the heads of its known instances.  Methods and
;; instances live in hashes, so both are sorted for deterministic output.
(define (format-class-info env name ci)
  (define supers (class-info-supers ci))
  (define methods
    (sort (hash->list (class-info-methods ci)) symbol<? #:key car))
  (define insts
    (sort (for/list ([ii (in-list (env-instances env name))])
            (format "~s" (pred->datum (instance-info-head ii))))
          string<?))
  (string-append
   (format "~s (class)\n" name)
   (format "  parameters:   ~a\n"
           (string-join (map symbol->string (class-info-params ci)) " "))
   (if (null? supers)
       ""
       (format "  superclasses: ~a\n"
               (string-join (for/list ([p (in-list supers)])
                              (format "~s" (pred->datum p)))
                            " ")))
   (if (null? methods)
       ""
       (apply string-append
              "  methods:\n"
              (for/list ([m (in-list methods)])
                (format "    ~s :: ~a\n" (car m) (scheme->datum (cdr m))))))
   (if (null? insts)
       ""
       (format "  instances: ~a\n" (string-join insts " ")))))

;; Render a type constructor for ,info: arity (and a `sealed` marker for
;; #:abstract types), the constructors visible in the env with their
;; schemes, and the instance heads that mention this type — the classes
;; the type "implements".  An imported abstract type's constructors don't
;; resolve in the env, so they drop out naturally; a locally defined one's
;; stay visible, matching what the session can actually use.
(define (format-tcon-info env name ti)
  (define ctor-lines
    (for/list ([c (in-list (tcon-info-ctors ti))]
               #:when (env-ref-data env c))
      (format "    ~s :: ~a\n"
              c (scheme->datum (data-info-scheme (env-ref-data env c))))))
  (define impls
    (sort (remove-duplicates
           (for*/list ([(_cls insts) (in-hash (env-instance-table env))]
                       [ii (in-list insts)]
                       #:when (pred-mentions-tcon? (instance-info-head ii) name))
             (format "~s" (pred->datum (instance-info-head ii)))))
          string<?))
  (string-append
   (format "~s (type ctor, arity ~a~a)\n"
           name (tcon-info-arity ti)
           (if (tcon-info-abstract? ti) ", sealed" ""))
   (if (null? ctor-lines)
       ""
       (apply string-append "  constructors:\n" ctor-lines))
   (if (null? impls)
       ""
       (format "  implements: ~a\n" (string-join impls " ")))))

(define (pred-mentions-tcon? p name)
  (ormap (lambda (t) (type-mentions-tcon? t name)) (pred-args p)))

(define (type-mentions-tcon? t name)
  (match t
    [(tcon n)       (eq? n name)]
    [(tapp h args)  (or (type-mentions-tcon? h name)
                        (ormap (lambda (a) (type-mentions-tcon? a name)) args))]
    [(tforall _ b)  (type-mentions-tcon? b name)]
    [(qual cs b)    (or (ormap (lambda (c) (pred-mentions-tcon? c name)) cs)
                        (type-mentions-tcon? b name))]
    [_              #f]))

;; ----- macro-definition input -------------------------------------

;; Evaluate a `define-syntax` / `define-syntax-rule` / `define-syntaxes`
;; straight into the session namespace, binding a real hygienic Racket
;; transformer there.  Record its name(s) so later inputs expand uses of
;; it (see `expand-session-macros`) and so the parser turns hygiene on.
;; The namespace retains the binding across inputs, so a macro defined on
;; one line is usable on every later line with no re-feeding.
(define (handle-macro-def state input)
  (with-handlers
   ([exn:fail?
     (lambda (e) (values state (format "error: ~a\n" (exn-message e))))])
   (eval-in state (datum->syntax #f input))
   (define names (macro-def-names input))
   (define state*
     (struct-copy rackton-repl-state state
                  [macros (append names
                                  (rackton-repl-state-macros state))]
                  [sources (sources-record-names
                            (rackton-repl-state-sources state)
                            names input)]))
   (values state* "")))

;; ----- session-macro expansion ------------------------------------

;; Expand every session-macro use in `input`, returning the result as a
;; syntax object so macro-introduced binders keep the scopes the parser's
;; hygiene relies on.  When the session has defined no macros this just
;; returns `(datum->repl-syntax input)`, so non-macro sessions are
;; unaffected.
(define (expand-session-macros state input)
  (define names (rackton-repl-state-macros state))
  (define stx (datum->repl-syntax input))
  (cond
    [(null? names) stx]
    [else
     (parameterize ([current-namespace (rackton-repl-state-nsp state)])
       (expand-macro-walk stx names))]))

;; Convert REPL input to syntax.  A `require` form is given a notional
;; source in the working directory so a relative library path resolves
;; against the cwd — its spec syntax otherwise carries no source, and
;; `require-spec->submod-spec` then can't locate the library's sidecar
;; (so neither its types nor its macros would import).  Every other form
;; keeps a #f source, so type errors are never prefixed with a fake file.
(define (datum->repl-syntax input)
  (cond
    [(and (pair? input) (eq? (car input) 'require))
     (datum->syntax #f input
                    (list (build-path (current-directory) "repl-input")
                          #f #f #f #f))]
    [else (datum->syntax #f input)]))

;; One structural pass over `stx`: while the head names a session macro,
;; take a single expansion step with `expand-once` (which fires the
;; transformer without lowering the result into Racket core syntax the
;; way a full `expand` would), then recurse into every sub-form.  The
;; `names` guard is essential — `expand-once` on a plain application like
;; `(+ 1 2)` would lower it to `(#%app + 1 2)`, which `parse-top` cannot
;; read; guarding on session-macro heads keeps ordinary forms untouched.
(define (expand-macro-walk stx names)
  (define l (syntax->list stx))
  (cond
    [(or (not l) (null? l)) stx]
    [(head-macro? (car l) names)
     (expand-macro-walk (expand-once stx) names)]
    [else
     (datum->syntax stx
                    (map (lambda (s) (expand-macro-walk s names)) l)
                    stx stx)]))

(define (head-macro? head-stx names)
  (and (identifier? head-stx)
       (and (memq (syntax-e head-stx) names) #t)))

;; ----- top-form input ---------------------------------------------

;; `stx` is the (already macro-expanded) input as a syntax object.
(define (handle-top-input state stx)
  (with-handlers
   ([exn:fail?
     (lambda (e) (values state (format "error: ~a\n" (exn-message e))))])
   (define-values (env* declared* compiled final-st parsed)
     (elaborate-form state stx))
   (define base
     (struct-copy rackton-repl-state state
                  [env env*] [declared declared*] [infer-st final-st]
                  [sources (sources-record
                            (rackton-repl-state-sources state)
                            (syntax->datum stx)
                            parsed)]))
   ;; Run the compiled forms (including a `require`'s runtime import)
   ;; before pulling in any exported macros, so a macro that expands into
   ;; the library's runtime bindings finds them already loaded.
   (for ([c (in-list compiled)])
     (eval-in base c))
   (define datum (syntax->datum stx))
   (define state*
     (if (and (pair? datum) (eq? (car datum) 'require))
         (struct-copy rackton-repl-state base
                      [macros (import-session-macros base stx)])
         base))
   (values state*
           (format-top-result datum
                              (rackton-repl-state-env state)
                              env*))))

;; Bind any macros a required Rackton library exports into the session.
;; Each library's `rackton-schemes` sidecar carries `rackton-macros` — a
;; list of (name . definition-datum), one entry per provided macro name.
;; Evaluate each definition into the session namespace (so later uses
;; expand) and record the name.  A
;; spec with no sidecar, or a library that exports no macros, contributes
;; nothing.  References inside a macro's template resolve in the session
;; namespace, so a macro built only from prelude/exported names works; one
;; that reaches a library-private binding will not (the same boundary the
;; module pipeline has for non-exported references).
(define (import-session-macros state require-stx)
  (define specs (cdr (syntax->list require-stx)))   ; (require spec ...)
  (for/fold ([macros (rackton-repl-state-macros state)])
            ([spec-stx (in-list specs)])
    (define submod (require-spec->submod-spec spec-stx))
    (cond
      [(not submod) macros]
      [else
       (define entries
         (with-handlers ([exn:fail? (lambda (_) '())])
           (dynamic-require submod 'rackton-macros)))
       (for/fold ([macros macros]) ([entry (in-list entries)])
         (eval-in state (datum->syntax #f (cdr entry)))
         (cons (car entry) macros))])))

(define (format-top-result input pre-env post-env)
  (match input
    [`(define ,(? symbol? name) . ,_)        (echo-definition name post-env)]
    [`(define (,(? symbol? name) . ,_) . ,_) (echo-definition name post-env)]
    [_ ""]))

(define (echo-definition name post-env)
  (define sch (env-ref-var post-env name))
  (cond
    [sch (format "~s :: ~a\n" name (scheme->datum sch))]
    [else ""]))

;; ----- expression input -------------------------------------------

;; `stx` is the (already macro-expanded) input expression as a syntax
;; object.  It is spliced into the synthetic `define` as syntax so any
;; macro-introduced scopes survive — `datum->syntax` leaves an embedded
;; syntax object intact rather than re-wrapping it.
(define (handle-expr-input state stx)
  (with-handlers
   ([exn:fail?
     (lambda (e) (values state (format "error: ~a\n" (exn-message e))))])
   (define n (rackton-repl-state-expr-counter state))
   (define name (string->symbol (format "$repl-~a" n)))
   (define synthetic (datum->syntax #f (list 'define name stx)))
   (define-values (env* declared* compiled final-st _parsed)
     (elaborate-form state synthetic))
   (define state*
     (struct-copy rackton-repl-state state
                  [env env*]
                  [declared declared*]
                  [infer-st final-st]
                  [expr-counter (add1 n)]))
   (for ([c (in-list compiled)])
     (eval-in state* c))
   (define value (eval-in state* (datum->syntax #f name)))
   (define sch (env-ref-var env* name))
   (values state*
           (format "~a :: ~a\n"
                   (format-value value)
                   (scheme->datum sch)))))

;; ----- elaboration + eval -----------------------------------------

;; Parse + type-check + compile one top-form syntax under the
;; persisted parameters.  Returns updated env and the list of
;; compiled Racket-syntax forms (a single parsed entry may expand
;; to multiple, e.g. #:deriving).
;; Returns (values env* declared* compiled final-st).  declared* is the
;; carried-forward signature map — this input's `(: foo …)` decs merged
;; in, definitions' consumed — which the caller must persist so a
;; signature declared in one input constrains a define in a later one.
;; The inference state — fresh counter, pending preds, and the resolution
;; tables — is threaded as an immutable infer-state the caller persists
;; into the next repl-state, so the tables accumulate across inputs the
;; way the old mutable hashes did.
(define (elaborate-form state stx)
  ;; A REPL session iterates by re-evaluating forms, so a re-declared instance
  ;; replaces the prior one instead of raising the module coherence error.
  (parameterize ([current-allow-instance-redefinition? #t])
    ;; Turn hygiene on once the session has bound any macro, so a
    ;; macro-introduced local binder is α-renamed apart from a user binder
    ;; of the same symbol (and a macro's reference falls through to the
    ;; prelude).  Off otherwise: non-macro sessions parse exactly as before.
    (define parsed
      (parameterize ([current-hygiene?
                      (not (null? (rackton-repl-state-macros state)))])
        (parse-toplevel-list (list stx))))
    ;; Run the full 4-phase pipeline over the parsed list so that multi-form
    ;; REPL input is order-invariant just like a module body.  Pass the
    ;; persisted st so resolution tables accumulate; get the final st back.
    ;; infer-program/phases also returns the post-expansion form list
    ;; (`#:derive-superclasses` instances replaced by the plain instances they
    ;; synthesize); compile THAT so derived instances are lowered.
    (define-values (env* declared* parsed* final-st)
      (infer-program/phases parsed
                            (rackton-repl-state-env state)
                            (rackton-repl-state-declared state)
                            (rackton-repl-state-infer-st state)))
    ;; Hand codegen the resolution tables inference just wrote into final-st.
    ;; return-typed-methods must be the env's real set: codegen routes a
    ;; resolved "$pure:Stream"-style call site through the runtime dispatch
    ;; table (lookup-return-method) only for methods IN the set, and emits a
    ;; direct impl-name reference otherwise.  A REPL session needs the table
    ;; route — a required module's impl name is module-internal, so a direct
    ;; reference is unbound at the top level.
    (define plan
      (codegen-plan (st-table final-st 'method-resolutions)
                    (st-table final-st 'method-dict-resolutions)
                    (st-table final-st 'needs-dict-defs)
                    (st-table final-st 'instance-default-bodies)
                    (env-return-typed-methods env*)))
    ;; Thread a fresh codegen state across this input's forms; the REPL evals
    ;; the compiled forms directly, so its inline/export logs aren't needed.
    (define-values (compiled _cgst)
      (for/fold ([acc '()] [cgst (make-cg-st)] #:result (values (reverse acc) cgst))
                ([p (in-list parsed*)])
        (let-values ([(s cgst*) (compile-top p env* plan cgst)])
          (values (if s (cons s acc) acc) cgst*))))
    ;; `parsed` (pre-phase) carries the names the USER's form binds —
    ;; source recording wants those, not the synthesized expansions in
    ;; `parsed*` (a derive-superclasses instance records once, under
    ;; the class the user wrote).
    (values env* declared* compiled final-st parsed)))

(define (eval-in state stx)
  (parameterize ([current-namespace (rackton-repl-state-nsp state)])
    (eval stx)))

;; ----- completion ------------------------------------------------

;; Completion candidates from the session env.  Returns
;; a list of strings whose names start with `prefix`.  Consults
;; the four user-extensible namespaces — vars, data ctors,
;; classes, tcons — so a partial type or class name also
;; completes, plus the surface keywords (`define`, `protocol`,
;; `match`, …), which are typed as often as any binding.
(define (rackton-repl-completions state prefix)
  (define env (rackton-repl-state-env state))
  (define all-names
    (append rackton-keyword-names
            (for/list ([n (in-list (append (hash-keys (env-vars env))
                                           (hash-keys (env-data-ctors env))
                                           (hash-keys (env-classes env))
                                           (hash-keys (env-tcons env))))])
              (symbol->string n))))
  (sort (remove-duplicates
         (filter (lambda (s) (string-prefix? s prefix)) all-names))
        string<?))

;; ----- interactive loop -------------------------------------------

;; Drive the kernel from `current-input-port` / `current-output-port`.
;; EOF or `,quit` ends the loop.  Exposed as a single entry that
;; user-facing shims can call (e.g. via `racket -l rackton/repl`).
;;
;; Two input layers share the kernel: when stdin/stdout are a
;; recognized terminal, the structural editor (repl-term.rkt) provides
;; paredit editing, multi-line entries, history, completion, and
;; coloring; otherwise — pipes, tests, dumb terminals — the plain line
;; loop with `rackton-read-form` accumulation runs exactly as before.
(define (rackton-repl-run)
  (display "rackton REPL — ,help for commands, ,quit to exit\n")
  (define current-state (box (rackton-repl-init)))
  (cond
    [(rackton-term-open (rackton-history-load (rackton-history-path)))
     => (lambda (th) (run-term-loop th current-state))]
    [else (run-line-loop current-state)]))

;; The structural-editor loop (repl-term.rkt).  Each iteration reads
;; one accepted entry as text, converts it to a datum (comma commands
;; included), and steps the kernel; eof (^D on an empty entry) or
;; `,quit` persists the history.  A read error in an accepted entry —
;; the ready test deliberately accepts malformed-but-closed input so
;; the kernel can report it — is shown here and the loop continues.
(define (run-term-loop th current-state)
  (define (close!)
    (rackton-history-save! (rackton-history-path)
                           (rackton-term-close th)))
  (let loop ()
    (refresh-type-columns!)
    (define text
      (rackton-term-read
       th
       #:prompt "λ> "
       #:ready? (lambda (s) (rackton-editor-ready? (open-input-string s)))
       #:completions (lambda (prefix)
                       (rackton-repl-completions (unbox current-state) prefix))))
    (cond
      [(eof-object? text) (close!)]
      [else
       (define form
         (with-handlers ([exn:fail:read?
                          (lambda (e)
                            (display (format "error: ~a\n" (exn-message e)))
                            #f)])
           (rackton-editor-read-datum (open-input-string text))))
       (cond
         [(or (not form) (eof-object? form)) (loop)]
         [else
          (define-values (state* output)
            (rackton-repl-step (unbox current-state) form))
          (display output)
          (set-box! current-state state*)
          (cond
            [(rackton-repl-state-quit? state*) (close!)]
            [else (loop)])])])))

;; The plain line-by-line loop (non-terminal input).  Uses readline for
;; history + line editing when available, plus multi-line accumulation
;; via `rackton-read-form`.  Tab completion consults the live session
;; env for variable / type / class names.
(define (run-line-loop current-state)
  ;; Set up readline tab completion: callbacks consult the
  ;; current-state box's snapshot of the env.
  (with-handlers ([exn:fail? (lambda (_) (void))])
    (dynamic-require 'readline/readline 'set-completion-function!)
    (define f (dynamic-require 'readline/readline 'set-completion-function!))
    (f (lambda (text)
         (rackton-repl-completions (unbox current-state) text))))
  (let loop ()
    (define state (unbox current-state))
    (define port (current-input-port))
    ;; Track the terminal width so wrapped type errors fit this session's
    ;; window (re-checked each prompt, so a mid-session resize is honored).
    (refresh-type-columns!)
    (display "λ> ") (flush-output)
    (define form
      (rackton-read-form port
                         (lambda (_depth)
                           (display "..> ")
                           (flush-output)
                           "")))
    (cond
      [(eof-object? form) (newline)]
      [else
       (define-values (state* output) (rackton-repl-step state form))
       (display output)
       (set-box! current-state state*)
       (cond
         [(rackton-repl-state-quit? state*) (void)]
         [else (loop)])])))

;; ----- value rendering --------------------------------------------

;; Top-level catch-all: print everything via ~v for now.  Rackton
;; data ctors are Racket structs whose printers already render them
;; readably.
(define (format-value v) (~v v))

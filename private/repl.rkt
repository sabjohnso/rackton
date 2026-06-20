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
         rackton-name-kind
         (rename-out [rackton-repl-state-quit? rackton-repl-quit?]))

(require racket/match
         racket/format
         racket/list
         racket/string
         (only-in racket/port with-output-to-string)
         (only-in racket/pretty pretty-format)
         (only-in "ast.rkt" class-law-name class-law-stx)
         (only-in "diagnostic.rkt"
                  doc-cat doc-group doc-nest doc-line doc-text render-doc
                  labeled-block)
         "surface.rkt"
         "infer.rkt"
         "codegen.rkt"
         "codegen-plan.rkt"
         "prelude.rkt"
         "env.rkt"
         "types.rkt"
         (only-in "nat-solve.rkt" deep-normalize-nats)
         (only-in "array-runtime.rkt"
                  rackton-array? rackton-array-length rackton-array-ref)
         "term.rkt"
         "repl-input.rkt"
         "repl-term.rkt"
         "repl-source.rkt"
         "repl-search.rkt"
         (only-in "installed-scan.rkt" rackton-workspace-entries)
         (only-in "analyze.rkt" index-entry-name index-entry-scheme)
         (only-in "macro-expand.rkt"
                  macro-def-names head-macro? expand-macro-walk))

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
    (namespace-require 'rackton)
    ;; Codegen emits references to internal runtime helpers (the tuple /
    ;; array representation interfaces) that `rackton` intentionally does
    ;; NOT re-export to users.  In a compiled module those resolve via
    ;; codegen's for-template imports, but the REPL evaluates at top level
    ;; where they must be namespace bindings — so require them directly.
    (namespace-require '(only rackton/private/prelude-runtime
                              rackton-tuple-make rackton-tuple-ref))
    (namespace-require '(only rackton/private/array-runtime
                              rackton-array-from-list rackton-array-make
                              rackton-array-ref rackton-array-length
                              rackton-array-take rackton-array-drop)))
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
  ;; `input` may be a syntax object — the readers use `read-syntax`, so a
  ;; bracket/brace literal's `paren-shape` survives — or a bare datum (the
  ;; many test callers, plus the macro-import path).  Dispatch decisions
  ;; read the datum; the eval path threads the original syntax so
  ;; paren-shape reaches the surface parser.
  (define datum (if (syntax? input) (syntax->datum input) input))
  (cond
    [(repl-command? datum)   (handle-command state datum input)]
    [(macro-def-form? datum) (handle-macro-def state datum)]
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
                type-family type-instance
                : require))))

;; A macro-definition form binds a Racket transformer rather than a
;; Rackton value, so it is handled outside the parse/infer/codegen
;; pipeline: evaluated straight into the session namespace.  (`form` is
;; a datum here — the REPL reads inputs as data; `macro-def-names`,
;; `head-macro?`, and `expand-macro-walk` are shared with the analyzer
;; via `macro-expand.rkt`.)
(define (macro-def-form? form)
  (and (pair? form)
       (memq (car form)
             '(define-syntax define-syntax-rule define-syntaxes))))

;; ----- command handling -------------------------------------------

(define (handle-command state cmd input)
  ;; `cmd` is the datum (matched below); `input` is the original syntax|datum
  ;; so a command-argument expression such as `,type [1 2 3]` keeps its
  ;; paren-shape.  `arg` reads the i-th element as syntax when it can.
  ;; `unquote` is special inside `match`'s quasiquote, so spell the command
  ;; shapes out with explicit `(list 'unquote …)` patterns.
  (define (arg i)
    (if (syntax? input) (list-ref (syntax->list input) i) (list-ref cmd i)))
  (match cmd
    [(list 'unquote)              (values state "")]         ; bare `,` — no-op
    [(list 'unquote 'geiser-no-values) (values state "")]    ; Geiser probe — no-op
    [(list 'unquote 'quit)        (quit state)]
    [(list 'unquote 'q)           (quit state)]
    [(list 'unquote 'clear)       (values (rackton-repl-init) "session cleared\n")]
    [(list 'unquote 'c)           (values (rackton-repl-init) "session cleared\n")]
    [(list 'unquote 'help)        (values state (help-text))]
    [(list 'unquote 'h)           (values state (help-text))]
    [(list 'unquote 'keys)        (values state (keys-text))]
    [(list 'unquote 'type expr)   (values state (show-type state (arg 2)))]
    [(list 'unquote 't    expr)   (values state (show-type state (arg 2)))]
    [(list 'unquote 'info name)   (values state (show-info state name))]
    [(list 'unquote 'i    name)   (values state (show-info state name))]
    [(list 'unquote 'source name) (values state (show-source state name))]
    [(list 'unquote 'src    name) (values state (show-source state name))]
    [(list 'unquote 'accepts ty)  (values state (show-accepts state ty))]
    [(list 'unquote 'a       ty)  (values state (show-accepts state ty))]
    [(list 'unquote 'search q)    (values state (show-search state q 'signature))]
    [(list 'unquote 'returns ty)  (values state (show-search state ty 'returns))]
    [(list 'unquote 'complete pfx) (values state (show-completions state pfx))]
    [(list 'unquote 'complete)    (values state (show-completions state ""))]
    [(list 'unquote 'colors)      (values state (colors-summary))]
    [(list 'unquote 'colors (? symbol? scheme))
     (values state
             (if (set-color-scheme! scheme)
                 (colors-summary)
                 (format "unknown scheme: ~a — try ,colors for the list\n"
                         scheme)))]
    [(list 'unquote 'colors (? symbol? cat) (? symbol? col))
     (values state
             (cond
               [(not (valid-category? cat))
                (format "unknown category: ~a — try ,colors for the list\n" cat)]
               [(not (set-color! cat col))
                (format "unknown color: ~a — try ,colors for the list\n" col)]
               [else (colors-summary)]))]
    [_ (values state
               (format "unknown command: ~a\n" (command->string cmd)))]))

(define (quit state)
  (values (struct-copy rackton-repl-state state [quit? #t]) ""))

;; Render a command datum back to the comma syntax the user typed, for
;; the "unknown command" message: `(unquote foo bar)` -> ",foo bar".
(define (command->string cmd)
  (string-append "," (string-join (map ~a (cdr cmd)) " ")))

(define (help-text)
  (string-append
   ",type EXPR   show the value and inferred type of EXPR\n"
   ",info NAME   show what's bound to NAME\n"
   ",source NAME show the form that defined NAME\n"
   ",accepts TYPE list functions accepting an argument of TYPE\n"
   ",search SIG   search by whole signature; a string searches names\n"
   ",returns TYPE list functions returning TYPE\n"
   ",complete PFX list names completing PFX (editor transport)\n"
   ",keys        editor key bindings (terminal sessions)\n"
   ",colors      show or set the editor color scheme\n"
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
   (define-values (env* _declared* compiled _final-st _parsed)
     (elaborate-form state synthetic))
   (define sch (env-ref-var env* name))
   ;; Evaluate in the session namespace (without persisting the binding
   ;; into the session env) so the value can be shown alongside the
   ;; type, rendered the same way a bare expression result is.
   (for ([c (in-list compiled)]) (eval-in state c))
   (define value (eval-in state (datum->syntax #f name)))
   (render-typed (value->doc value) (scheme->display-datum sch))))

;; The workspace index (curated stdlib + installed user modules) as
;; (name . scheme) search candidates, computed once per process — the
;; scan reads every module's sidecar, so it is memoized.  Only entries
;; carrying a type scheme are searchable; a broken scan contributes
;; nothing rather than breaking search.
(define workspace-search-pairs
  (let ([cached #f])
    (lambda ()
      (unless cached
        (set! cached
              (with-handlers ([exn:fail? (lambda (_) '())])
                (for/list ([e (in-list (rackton-workspace-entries))]
                           #:when (index-entry-scheme e))
                  (cons (index-entry-name e) (index-entry-scheme e))))))
      cached)))

;; Append the workspace-index hits the session has not already shown.
;; Session hits keep their order (and rank) and win on name collisions,
;; since the live binding is the one the user is working with.
(define (merge-search-hits session index)
  (define seen (for/hasheq ([h (in-list session)]) (values (car h) #t)))
  (append session
          (filter (lambda (h) (not (hash-ref seen (car h) #f))) index)))

;; Search the workspace index under `kind`, judging constraints against
;; the session env: a candidate whose constraints the session can refute
;; (e.g. `show` for a type with no `Show` instance) is dropped, so a
;; session-local type does not spuriously match every constrained-
;; variable function.  (The CLI, with no session, passes #f and skips
;; this filter.)
(define (index-search-hits env query kind)
  (define hits (search-entries (workspace-search-pairs) query #:kind kind #:env env))
  (if (symbol? hits) '() hits))   ; 'bare-query never reaches here

;; Hoogle-style search over everything in scope and across every
;; installed module: by whole signature (modulo unification and
;; argument order), by result type, or — for a string query — by name.
;; See repl-search.rkt for the rules.
(define (show-search state query kind)
  (with-handlers
   ([exn:fail?
     (lambda (e) (format "error: ~a\n" (exn-message e)))])
   (define env (rackton-repl-state-env state))
   (match (search-entries (env-search-entries env) query
                          #:kind kind #:env env)
     ['bare-query
      (format "~s is a bare type variable — everything matches\n" query)]
     [session-hits
      (define hits (merge-search-hits session-hits (index-search-hits env query kind)))
      (cond
        [(null? hits) (format "no matches for ~s\n" query)]
        [else
         (apply string-append
                (for/list ([h (in-list hits)])
                  (render-typed (name-doc (car h))
                                (binding-type-datum env (car h) (cdr h)))))])])))

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
     [session-matches
      (define matches
        (merge-search-hits session-matches
                           (index-search-hits (rackton-repl-state-env state)
                                              type-datum 'accepts)))
      (cond
        [(null? matches) (format "no functions accept ~s\n" type-datum)]
        [else
         (apply string-append
                (for/list ([m (in-list matches)])
                  (render-typed (name-doc (car m))
                                (binding-type-datum (rackton-repl-state-env state)
                                                    (car m) (cdr m)))))])])))

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
     => (lambda (sch) (render-typed (name-doc name) (binding-type-datum env name sch)))]
    [(env-ref-data env name)
     => (lambda (di)
          ;; A single-constructor datatype binds the same name as both a
          ;; data ctor and a type ctor.  Show the ctor's scheme, then the
          ;; type description when the name is also a type constructor.
          (string-append
           (render-typed (name-doc name)
                         (scheme->pretty-datum (data-info-scheme di))
                         #:suffix " (data ctor)")
           (cond [(env-ref-tcon env name) => (lambda (ti) (format-tcon-info env name ti))]
                 [else ""])))]
    [(env-ref-tcon env name)
     => (lambda (ti) (format-tcon-info env name ti))]
    [(env-ref-class env name)
     => (lambda (ci) (format-class-info env name ci))]
    [(env-ref-tyfam env name)
     => (lambda (fi) (format-tyfam-info name fi))]
    [(primitive-type? name) (format-primitive-info env name)]
    [else (format "~s is unbound\n" name)]))

;; Render a standalone type family for ,info: its openness, arity, and
;; inferred kind, then its clauses (closed) or instance equations (open).
(define (format-tyfam-info name fi)
  (string-append
   (format "~s (~a type family, arity ~a)~a\n"
           name (tyfam-info-openness fi) (tyfam-info-arity fi)
           (if (tyfam-info-kind fi)
               (format " :: ~a" (kind->datum (tyfam-info-kind fi)))
               ""))
   (let ([cs (tyfam-info-clauses fi)])
     (if (null? cs)
         ""
         (apply string-append
                (if (eq? (tyfam-info-openness fi) 'closed)
                    "  clauses:\n" "  instances:\n")
                (for/list ([c (in-list cs)])
                  (format "    ~s = ~s\n"
                          (map type->datum (car c))
                          (type->datum (cdr c)))))))))

;; Render a class for ,info: parameters, superclasses, methods (each with
;; its scheme), the declared laws, and the heads of its known instances.
;; Methods and instances live in hashes, so both are sorted for
;; deterministic output; laws keep their declaration order.
(define (format-class-info env name ci)
  (define supers (class-info-supers ci))
  (define methods
    (sort (hash->list (class-info-methods ci)) symbol<? #:key car))
  (define laws (class-info-laws ci))
  (define insts
    (sort (for/list ([ii (in-list (env-instances env name))])
            (format "~s" (pred->datum (instance-info-head ii))))
          string<?))
  (string-append
   (format "~s (protocol)\n" name)
   (format "  parameters:   ~a\n"
           (string-join (map symbol->string (class-info-params ci)) " "))
   (if (null? supers)
       ""
       (format "  superprotocols: ~a\n"
               (string-join (for/list ([p (in-list supers)])
                              (format "~s" (pred->datum p)))
                            " ")))
   (if (null? methods)
       ""
       (apply string-append
              "  methods:\n"
              (for/list ([m (in-list methods)])
                (render-typed-line 4 (car m) (scheme->pretty-datum (cdr m))))))
   (if (null? laws)
       ""
       (apply string-append
              "  laws:\n"
              (for/list ([law (in-list laws)])
                ;; The originating clause datum is `(name quantified)`;
                ;; print the `quantified` part beside the name we already
                ;; have so the law reads as written.
                (render-labeled-datum
                 4
                 (format "~s" (class-law-name law))
                 (cadr (syntax->datum (class-law-stx law)))))))
   (if (null? insts)
       ""
       (render-labeled-list "  instances:" insts))))

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
      (render-typed-line 4 c (scheme->pretty-datum (data-info-scheme (env-ref-data env c))))))
  (define impls (tcon-implements env name))
  (string-append
   (format "~s (type ctor, arity ~a~a)\n"
           name (tcon-info-arity ti)
           (if (tcon-info-abstract? ti) ", sealed" ""))
   (if (null? ctor-lines)
       ""
       (apply string-append "  constructors:\n" ctor-lines))
   (if (null? impls)
       ""
       (render-labeled-list "  implements:" impls))))

;; Render a primitive scalar type (Integer, Boolean, String, Float) for
;; ,info.  These are never registered as data — they have no
;; user-visible constructors — so show only the protocols they
;; implement.
(define (format-primitive-info env name)
  (define impls (tcon-implements env name))
  (string-append
   (format "~s (primitive type)\n" name)
   (if (null? impls)
       ""
       (render-labeled-list "  implements:" impls))))

;; The instance heads in the env that mention type constructor `name` —
;; the protocols the type "implements" — sorted for deterministic output.
(define (tcon-implements env name)
  (sort (remove-duplicates
         (for*/list ([(_cls insts) (in-hash (env-instance-table env))]
                     [ii (in-list insts)]
                     #:when (pred-mentions-tcon? (instance-info-head ii) name))
           (format "~s" (pred->datum (instance-info-head ii)))))
        string<?))

(define (pred-mentions-tcon? p name)
  (ormap (lambda (t) (type-mentions-tcon? t name)) (pred-args p)))

(define (type-mentions-tcon? t name)
  (match t
    [(tcon n)       (eq? n name)]
    [(tapp h args)  (or (type-mentions-tcon? h name)
                        (ormap (lambda (a) (type-mentions-tcon? a name)) args))]
    [(tforall _ b)  (type-mentions-tcon? b name)]
    [(texists _ b)  (type-mentions-tcon? b name)]
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
  (define stx (repl-input->syntax input))
  (cond
    [(null? names) stx]
    [else
     (parameterize ([current-namespace (rackton-repl-state-nsp state)])
       (expand-macro-walk stx names))]))

;; Convert REPL input to syntax.  `input` may already be a syntax object
;; (the readers use `read-syntax`, so the bracket/brace literals'
;; `paren-shape` is carried) or a bare datum (test callers, macro
;; imports).  A `require` form is given a notional source in the working
;; directory so a relative library path resolves against the cwd — its
;; spec syntax otherwise carries no source, and `require-spec->submod-spec`
;; then can't locate the library's sidecar (so neither its types nor its
;; macros would import); `require` specs are module paths with no
;; paren-shape, so reconstructing them from the datum loses nothing.
;; Every other syntax input is passed through verbatim so paren-shape
;; survives; a bare datum keeps a #f source, so type errors are never
;; prefixed with a fake file.
(define (repl-input->syntax input)
  (define datum (if (syntax? input) (syntax->datum input) input))
  (cond
    [(and (pair? datum) (eq? (car datum) 'require))
     (datum->syntax #f datum
                    (list (build-path (current-directory) "repl-input")
                          #f #f #f #f))]
    [(syntax? input) (strip-syntax-srcloc input)]
    [else (datum->syntax #f input)]))

;; Re-stamp reader syntax with no source location, preserving paren-shape
;; (and lexical context).  `read-syntax` names a string port 'string, so
;; without this a type error would be prefixed with a fake `string:` file;
;; the datum path historically used a #f source, and this matches it while
;; keeping the paren-shape the bracket/brace literals depend on.
(define (strip-syntax-srcloc stx)
  (cond
    [(syntax? stx)
     (define e (syntax-e stx))
     (define e*
       (cond
         [(pair? e)
          (let loop ([p e])
            (cond
              [(pair? p) (cons (strip-syntax-srcloc (car p)) (loop (cdr p)))]
              [(null? p) '()]
              [else (strip-syntax-srcloc p)]))]   ; improper tail
         [(vector? e) (list->vector (map strip-syntax-srcloc (vector->list e)))]
         [(box? e)    (box (strip-syntax-srcloc (unbox e)))]
         [else e]))
     (datum->syntax stx e* #f stx)]
    [else stx]))

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

;; The display datum for `name`'s scheme: a variadic function is shown in
;; its surface `...` form, every other binding in the ordinary n-ary form.
(define (binding-type-datum env name sch)
  (define k (env-ref-variadic env name #f))
  (if k
      (scheme->variadic-pretty-datum sch k)
      (scheme->pretty-datum sch)))

(define (echo-definition name post-env)
  (define sch (env-ref-var post-env name))
  (cond
    [sch (render-typed (name-doc name) (binding-type-datum post-env name sch))]
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
           (render-typed (value->doc value) (scheme->display-datum sch)))))

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
    ;; (`#:derive-supers` instances replaced by the plain instances they
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
    ;; `parsed*` (a derive-supers instance records once, under
    ;; the class the user wrote).
    (values env* declared* compiled final-st parsed)))

(define (eval-in state stx)
  (parameterize ([current-namespace (rackton-repl-state-nsp state)])
    (eval stx)))

;; ----- completion ------------------------------------------------

;; The editor's name classifier, for syntax coloring: the lexer alone
;; cannot tell a type (`Maybe`) from a data constructor (`Just`) —
;; both are capitalized symbols — so the env decides.  Classes count
;; as types.  Unknown names return #f (the identifier category).
(define (rackton-name-kind state sym)
  (define env (rackton-repl-state-env state))
  (cond
    [(env-ref-tcon env sym) 'type]
    [(env-ref-data env sym) 'constructor]
    [(env-ref-class env sym) 'type]
    [else #f]))

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

;; The `,complete PREFIX` command: the pipe transport for editor
;; completion.  Prints the candidates one per line (empty output when
;; there are none), so a piped client — e.g. the Emacs inferior REPL —
;; can offer completion-at-point.  PFX is the command argument datum;
;; `~a' renders a symbol prefix as its text and "" (no argument) as the
;; empty prefix, which matches every name.
(define (show-completions state pfx)
  (define cs (rackton-repl-completions state (format "~a" pfx)))
  (if (null? cs) "" (string-append (string-join cs "\n") "\n")))

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
  (load-color-prefs!)
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
                       (rackton-repl-completions (unbox current-state) prefix))
       ;; Coloring: only the env can tell a type from a constructor.
       #:name-kind (lambda (sym)
                     (rackton-name-kind (unbox current-state) sym))))
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

;; Render a Rackton value the way the user wrote it, as a document the
;; engine can lay out across lines.  The container values print in the
;; bracket/brace literal syntax — a Cons/Nil chain as `[elem …]`, a Map as
;; `{k v …}`, a Set as `#{m …}`, and a `Pair` as `[a . b]` (or `(Pair a b)`
;; when its tail would not round-trip in dotted position).  Data
;; constructors are transparent structs named `$ctor:<Name>`; print them
;; under the bare `<Name>` (a nullary ctor with no parens, an n-ary one as
;; `(Name field …)`, recursively).  Functions read as `<lambda>` rather
;; than Racket's `#<procedure:…>`; the type, printed alongside, carries the
;; real information.  Everything else (numbers, strings, symbols, …) falls
;; back to `print` form — so a Symbol value reads as `'x`, not the bare `x`
;; that a constructor head uses.
;; Pretty datum for a scheme shown beside a VALUE result: reduce ground
;; Nat arithmetic in the type first, so a computed size like `(* 2 2)`
;; from `flatten-major` shows as `4` rather than the raw expression.
(define (scheme->display-datum sch)
  (match sch
    [(scheme vs body) (scheme->pretty-datum (scheme vs (deep-normalize-nats body)))]))

(define (value->doc v)
  (cond
    ;; Containers print back in the bracket/brace literal syntax, so the
    ;; output round-trips as Rackton source.
    [(rackton-list-elements v)
     => (lambda (elems) (bracket-parts "[" "]" (map value->doc elems)))]
    [(rackton-map-entries v)
     => (lambda (entries)
          (bracket-parts "{" "}"
            (append-map (lambda (kv)
                          (list (value->doc (car kv)) (value->doc (cdr kv))))
                        entries)))]
    [(rackton-set-members v)
     => (lambda (members) (bracket-parts "#{" "}" (map value->doc members)))]
    [(data-ctor-value? v)
     (define-values (name fields) (data-ctor-name+fields v))
     (if (null? fields)
         (doc-text name)
         (group-parts (cons (doc-text name) (map value->doc fields))))]
    ;; Tuples are vectors (see prelude-runtime's rackton-tuple-make).  A
    ;; 2-tuple is a `Pair`: it prints in the dotted-pair literal `[a . b]`
    ;; when the tail reads back as an atom, otherwise as `(Pair a b)` (which
    ;; always round-trips).  Any other arity prints with the `tuple` head.
    [(vector? v)
     (define elems (vector->list v))
     (cond
       [(and (= (length elems) 2) (self-reading-atom? (cadr elems)))
        (pair-doc (value->doc (car elems)) (value->doc (cadr elems)))]
       [(= (length elems) 2)
        (group-parts (cons (doc-text "Pair") (map value->doc elems)))]
       [else
        (group-parts (cons (doc-text "tuple") (map value->doc elems)))])]
    ;; A fixed-size array (the opaque handle from array-runtime) prints
    ;; with the `array` head — its hidden layout never shows.
    [(rackton-array? v)
     (group-parts
      (cons (doc-text "array")
            (for/list ([i (in-range (rackton-array-length v))])
              (value->doc (rackton-array-ref v i)))))]
    [(procedure? v) (doc-text "<lambda>")]
    [else (doc-text (format "~v" v))]))

;; Lay out `<open> p1 p2 … <close>` from already-built part docs (no head),
;; on one line when it fits, else one part per line indented under the
;; opener.  Empty renders as just `<open><close>` (e.g. [] / {} / #{}).
(define (bracket-parts open close parts)
  (cond
    [(null? parts) (doc-text (string-append open close))]
    [(null? (cdr parts)) (doc-cat (doc-text open) (car parts) (doc-text close))]
    [else
     (doc-group
      (doc-cat (doc-text open)
               (car parts)
               (doc-nest (string-length open)
                         (apply doc-cat
                                (map (lambda (p) (doc-cat doc-line p)) (cdr parts))))
               (doc-text close)))]))

;; [a . b] — the dotted-pair literal layout.
(define (pair-doc a-doc b-doc)
  (doc-cat (doc-text "[") a-doc (doc-text " . ") b-doc (doc-text "]")))

;; A value that prints as a single self-reading atom, so it round-trips in
;; the dotted tail position of `[a . b]`.  Symbols print as `'x` (a quote
;; form) and lists/structs as multi-token forms, so they are excluded — a
;; pair with such a tail falls back to `(Pair a b)`.
(define (self-reading-atom? v)
  (or (number? v) (string? v) (boolean? v) (char? v)))

;; The (key . value) entries of a `$map` value (sorted by key), or #f; and
;; the members of a `$set` value (sorted), or #f.  Detected by struct type
;; name via `struct->vector` — like `data-ctor-value?` — so a value built in
;; the evaluation namespace is still recognized.  `$map` / `$set` are
;; transparent, so the backing immutable hash is their sole field.
(define (struct-field-named v type-name)
  (and (struct? v)
       (let ([sv (struct->vector v)])
         (and (eq? (vector-ref sv 0) type-name)
              (= (vector-length sv) 2)
              (vector-ref sv 1)))))

(define (rackton-map-entries v)
  (define h (struct-field-named v 'struct:$map))
  (and (hash? h) (sort (hash->list h) rackton-value<? #:key car)))

(define (rackton-set-members v)
  (define h (struct-field-named v 'struct:$set))
  (and (hash? h) (sort (hash-keys h) rackton-value<?)))

;; A stable order for map/set display.  The keys/members of one collection
;; are homogeneous (the type system guarantees it), so the same-type
;; branches cover every real comparison; the `~v` fallback only keeps the
;; order total for compound element types.
(define (rackton-value<? a b)
  (cond
    [(and (real? a)   (real? b))   (< a b)]
    [(and (string? a) (string? b)) (string<? a b)]
    [(and (char? a)   (char? b))   (char<? a b)]
    [(and (symbol? a) (symbol? b)) (symbol<? a b)]
    [else (string<? (format "~v" a) (format "~v" b))]))

;; The element values of a Cons/Nil list, in order, or #f if `v` is not
;; one.  Matched structurally by constructor name *and* arity so an
;; unrelated user type that reuses the names is not taken for a list.
(define (rackton-list-elements v)
  (let loop ([v v] [acc '()])
    (cond
      [(data-ctor-named? v "Nil" 0) (reverse acc)]
      [(data-ctor-named? v "Cons" 2)
       (define-values (_ fields) (data-ctor-name+fields v))
       (loop (cadr fields) (cons (car fields) acc))]
      [else #f])))

;; Is `v` a data-ctor value built by the named constructor of the given
;; field count?
(define (data-ctor-named? v name arity)
  (and (data-ctor-value? v)
       (let-values ([(n fields) (data-ctor-name+fields v)])
         (and (string=? n name) (= (length fields) arity)))))

;; ----- typed-output layout ----------------------------------------

;; A document for `<head> :: <type>` that lays out on one line when it
;; fits the current width and otherwise breaks the type onto its own
;; hanging `:: …` line; `head` and the type each wrap internally.
(define (typed-doc head type-datum)
  (doc-group
   (doc-cat head
            (doc-nest 3
                      (doc-cat doc-line (doc-text ":: ")
                               (datum->doc type-datum))))))

;; Render `<head> :: <type>` to a line (terminated with a newline),
;; wrapping at the REPL width.  The width reuses `current-type-columns`
;; — the same terminal-derived budget the type-error diagnostics use,
;; refreshed each prompt — so a wide value or signature folds to fit.
(define (render-typed head type-datum #:suffix [suffix ""])
  (string-append
   (render-doc (typed-doc head type-datum) (current-type-columns))
   suffix "\n"))

(define (name-doc name) (doc-text (format "~s" name)))

;; An indented `<spaces><name> :: <type>` line for ,info listings,
;; wrapping the signature under a hanging indent so a long type stays
;; readable beneath its name.
(define (render-typed-line indent name type-datum)
  (string-append
   (render-doc
    (doc-nest indent
              (doc-cat (doc-text (make-string indent #\space))
                       (typed-doc (name-doc name) type-datum)))
    (current-type-columns))
   "\n"))

;; An indented `<spaces><label>: <datum>` line for ,info listings (a
;; protocol law).  The whole thing sits on one line when it fits;
;; otherwise the break after the label fires first, dropping the datum to
;; its own line indented under the label, and the datum wraps internally
;; only if it still does not fit there.  The `doc-line` between label and
;; datum is what gives the after-label break priority over the datum's
;; own group breaks.
(define (render-labeled-datum indent label datum)
  (string-append
   (render-doc
    (doc-group
     (doc-cat (doc-text (string-append (make-string indent #\space) label ":"))
              (doc-nest (+ indent 2)
                        (doc-cat doc-line (datum->doc datum)))))
    (current-type-columns))
   "\n"))

;; A `<header> a b c` line that breaks to one item per line, indented,
;; when the items don't all fit — for ,info's instance list.  `header`
;; carries its own leading indent and trailing colon.
(define (render-labeled-list header items)
  (string-append
   (render-doc (labeled-block header items) (current-type-columns))
   "\n"))

;; A data-ctor value is a struct whose type name starts with `$ctor:`.
(define (data-ctor-value? v)
  (and (struct? v)
       (regexp-match? #rx"^struct:[$]ctor:"
                      (symbol->string (vector-ref (struct->vector v) 0)))))

;; The bare constructor name and field values of a data-ctor struct.
(define (data-ctor-name+fields v)
  (define sv (struct->vector v))
  (define raw (symbol->string (vector-ref sv 0)))   ; "struct:$ctor:Name"
  (values (substring raw (string-length "struct:$ctor:"))
          (cdr (vector->list sv))))

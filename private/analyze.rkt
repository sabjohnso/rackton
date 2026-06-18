#lang racket/base

;; Rackton — the analysis layer under the tooling (search, LSP, DAP).
;;
;; `analyze-module` runs the same parse+infer pipeline the elaborator
;; uses over a `#lang rackton` file — without executing anything —
;; and converts every failure into a structured diagnostic instead of
;; raising.  The result answers the questions tools ask:
;;
;;   - what went wrong, where        → analysis-diagnostics
;;   - what is defined, where        → analysis-def-of (defsite)
;;   - what is this name's type      → analysis-scheme-of
;;   - what name is at line:col      → name-at
;;
;; Scope (v1): whole `#lang rackton` files.  Inference stops at the
;; first error, so a failed analysis carries one diagnostic; the
;; definition sites — which come from parsing alone — survive
;; regardless.  `require`d modules resolve through their
;; `rackton-schemes` sidecars exactly as in compilation, which means
;; the dependency must be loadable.
;;
;; Position conventions match srcloc: lines 1-based, columns 0-based.

(provide analyze-module
         analyze-text
         (struct-out analysis)
         (struct-out defsite)
         (struct-out diag)
         analysis-scheme-of
         analysis-def-of
         name-at
         ;; the sidecar defs table (written by elaborate.rkt)
         collect-defs
         defs-sidecar-datum
         ;; the workspace index
         (struct-out index-entry)
         module-sidecar-tables
         entries-from-tables
         module-index-entries
         index-modules
         rackton-collection-entries)

(require racket/match
         racket/list
         (only-in racket/port port->string)
         (only-in racket/path path-only)
         (only-in syntax/modresolve resolve-module-path)
         "surface.rkt"
         "infer.rkt"
         "prelude.rkt"
         "env.rkt"
         "types.rkt"
         (only-in "scheme-codec.rkt" sexp->scheme decode-data-info)
         (only-in "repl-entry.rkt" tokenize tok-type tok-start tok-end))

;; text: the file's full contents (position queries need it).
;; env: the typing environment after inference, or #f when it failed.
;; defs: hasheq name → defsite.  diagnostics: list of diag.
;; requires: the resolved paths of required modules that exist —
;; cross-module navigation looks their sidecars up.
(struct analysis (path text env defs diagnostics requires) #:transparent)

(struct defsite (name kind srcloc) #:transparent)   ; kind: value | type |
                                                    ;   constructor | class | method
(struct diag (severity srcloc message) #:transparent)

;; ----- reading ---------------------------------------------------------

;; The text's forms as syntax objects with real srclocs against
;; `path`.  The reader for `#lang rackton` wraps forms in
;; (rackton/main …); reading the raw forms after the #lang line
;; reproduces its input exactly.
(define (read-text-forms text path)
  (define in (open-input-string text))
  (port-count-lines! in)
  (void (read-line in))              ; the #lang line
  (let loop ([acc '()])
    (define f (read-syntax path in))
    (if (eof-object? f)
        (reverse acc)
        (loop (cons f acc)))))

;; ----- error conversion ---------------------------------------------------

(define (exn->diag e path)
  (define loc
    (cond
      [(and (exn:fail:syntax? e)
            (pair? (exn:fail:syntax-exprs e)))
       (stx->srcloc (car (exn:fail:syntax-exprs e)) path)]
      [(exn:fail:read? e)
       (match (exn:fail:read-srclocs e)
         [(cons l _) l]
         [_ (srcloc path #f #f #f #f)])]
      [else (srcloc path #f #f #f #f)]))
  (diag 'error loc (exn-message e)))

(define (stx->srcloc stx path)
  (srcloc (or (syntax-source stx) path)
          (syntax-line stx)
          (syntax-column stx)
          (syntax-position stx)
          (syntax-span stx)))

;; ----- definition sites ------------------------------------------------------

;; Every name a parsed top form binds, with its kind and location —
;; from the AST alone, so available even when inference fails.
(define (collect-defs parsed path)
  (for/fold ([defs (hasheq)]) ([t (in-list parsed)])
    (define (add defs name kind stx)
      (hash-set defs name (defsite name kind (stx->srcloc stx path))))
    (match t
      [(top:def name _ stx) (add defs name 'value stx)]
      [(top:data name _ ctors stx _ _)
       (for/fold ([defs (add defs name 'type stx)])
                 ([c (in-list ctors)])
         (add defs (data-ctor-name c) 'constructor (data-ctor-stx c)))]
      [(top:class _ head methods stx)
       (for/fold ([defs (add defs (constraint-class head) 'class stx)])
                 ([m (in-list methods)]
                  #:when (method-sig? m))
         (add defs (method-sig-name m) 'method (method-sig-stx m)))]
      [(top:alias name _ _ stx) (add defs name 'type stx)]
      [(top:effect name ops stx)
       (for/fold ([defs (add defs name 'type stx)])
                 ([op (in-list ops)])
         (add defs (effect-op-name op) 'value (effect-op-stx op)))]
      [(top:foreign name _ _ _ stx) (add defs name 'value stx)]
      [(top:foreign-c name _ _ _ _ _ _ stx) (add defs name 'value stx)]
      [_ defs])))

;; ----- the entry point ----------------------------------------------------------

(define (analyze-module path)
  (analyze-text (call-with-input-file path port->string) path))

;; Analyze buffer contents that need not be on disk (the LSP's
;; unsaved-edit path); `path` labels positions and anchors relative
;; requires.
(define (analyze-text text path)
  (define-values (defs reqs env diags)
    (with-handlers ([exn:fail?
                     (lambda (e)
                       (values (hasheq) '() #f (list (exn->diag e path))))])
      (define forms (read-text-forms text path))
      ;; Parse each form on its own: one broken form (the common
      ;; mid-edit state) must not blind the analysis to every other
      ;; form's definitions.
      (define parses
        (for/list ([f (in-list forms)])
          (with-handlers ([exn:fail? (lambda (e) e)])
            (parse-toplevel-list (list f)))))
      (define parse-errors (filter exn? parses))
      (define parsed (append* (filter list? parses)))
      (define defs (collect-defs parsed path))
      (define reqs (collect-requires parsed path))
      (cond
        [(pair? parse-errors)
         (values defs reqs #f
                 (for/list ([e (in-list parse-errors)])
                   (exn->diag e path)))]
        [else
         (with-handlers ([exn:fail?
                          (lambda (e)
                            (values defs reqs #f (list (exn->diag e path))))])
           (define-values (env* _declared* _parsed* _st)
             (infer-program/phases parsed prelude-env (hasheq)
                                   (make-infer-state)))
           (values defs reqs env* '()))])))
  (analysis path text env defs diags reqs))

;; The resolved on-disk paths of the module's requires (specs that
;; resolve to existing files; others are dropped — inference already
;; diagnoses unloadable requires).
(define (collect-requires parsed path)
  (define dir (path-only path))
  (for*/list ([t (in-list parsed)]
              #:when (top:require? t)
              [spec-stx (in-list (top:require-specs t))]
              [p (in-value (spec->path (syntax->datum spec-stx) dir))]
              #:when (and p (file-exists? p)))
    p))

(define (spec->path spec dir)
  (with-handlers ([exn:fail? (lambda (_) #f)])
    (match spec
      [(? string?) (simplify-path (build-path (or dir (current-directory)) spec))]
      [(? symbol?) (resolve-module-path spec #f)]
      [(list 'lib (? string? s)) (resolve-module-path spec #f)]
      [(list 'file (? string? s)) (string->path s)]
      [_ #f])))

;; ----- queries ---------------------------------------------------------------------

;; The scheme of `name` in the analyzed module's environment — local,
;; imported, or prelude — or #f (unknown name, or failed inference).
(define (analysis-scheme-of a name)
  (define env (analysis-env a))
  (and env
       (or (env-ref-var env name)
           (let ([di (env-ref-data env name)])
             (and di (data-info-scheme di))))))

(define (analysis-def-of a name)
  (hash-ref (analysis-defs a) name #f))

;; The symbol whose token covers line:col, with its local definition
;; site (#f for prelude/imported names and non-name tokens).
(define (name-at a line col)
  (define text (analysis-text a))
  (define off (position->offset text line col))
  (define t
    (and off
         (for/first ([t (in-list (tokenize text))]
                     #:when (and (eq? (tok-type t) 'symbol)
                                 (<= (tok-start t) off)
                                 (< off (tok-end t))))
           t)))
  (cond
    [(not t) (values #f #f)]
    [else
     (define sym (string->symbol (substring text (tok-start t) (tok-end t))))
     (values sym (analysis-def-of a sym))]))

(define (position->offset text line col)
  (let loop ([i 0] [l 1])
    (cond
      [(= l line) (+ i col)]
      [(>= i (string-length text)) #f]
      [(char=? (string-ref text i) #\newline) (loop (add1 i) (add1 l))]
      [else (loop (add1 i) l)])))

;; ----- the sidecar defs table ----------------------------------------------

;; Encode definition sites for the `rackton-defs` sidecar export:
;; one (name kind line col span) entry per *exported* name, under its
;; external name — unexported names stay private, like every other
;; sidecar table.  `renames` is (listof (cons local external)).
(define (defs-sidecar-datum defs renames)
  (for*/list ([p (in-list renames)]
              [d (in-value (hash-ref defs (car p) #f))]
              #:when d)
    (define l (defsite-srcloc d))
    (list (cdr p) (defsite-kind d)
          (srcloc-line l) (srcloc-column l) (srcloc-span l))))

(define (decode-defs-datum entries source)
  (for/fold ([h (hasheq)]) ([e (in-list entries)])
    (match e
      [(list name kind line col span)
       (hash-set h name (defsite name kind (srcloc source line col #f span)))]
      [_ h])))

;; ----- the workspace index ----------------------------------------------------

;; kind: value | method | type | constructor | class.
;; srcloc: from the sidecar defs table; #f for sidecars predating it.
(struct index-entry (name kind scheme module srcloc) #:transparent)

;; A module's sidecar tables, as the serializable s-expressions the
;; sidecar publishes — keyed by symbol so the set is fixed and the
;; index cache can store and replay them verbatim.  Loading the
;; sidecar instantiates the module; each lookup is guarded so a module
;; that is not Rackton, not compiled, or that errors on instantiation
;; simply yields the empty tables.
(define (module-sidecar-tables mod-path)
  (define submod `(submod ,mod-path rackton-schemes))
  (define (table key)
    (with-handlers ([exn:fail? (lambda (_) '())])
      (dynamic-require submod key)))
  (hasheq 'bindings   (table 'rackton-bindings)
          'data-ctors (table 'rackton-data-ctors)
          'tcons      (table 'rackton-tcons)
          'classes    (table 'rackton-classes)
          'defs       (table 'rackton-defs)))

;; Decode the sidecar tables into index entries, attributed to
;; `mod-path`.  Pure: no module loading, so the index cache can rebuild
;; entries from cached tables without re-reading the module.  Empty
;; tables (the non-Rackton case) contribute nothing.
(define (entries-from-tables tables mod-path)
  (define (tbl key) (hash-ref tables key '()))
  (define bindings (tbl 'bindings))
  (cond
    [(and (null? bindings)
          (null? (tbl 'tcons))
          (null? (tbl 'classes)))
     '()]
    [else
     (define defs (decode-defs-datum (tbl 'defs) mod-path))
     (define (loc-of name) (let ([d (hash-ref defs name #f)])
                             (and d (defsite-srcloc d))))
     (define (kind-of name fallback)
       (let ([d (hash-ref defs name #f)])
         (if d (defsite-kind d) fallback)))
     (append
      (for/list ([b (in-list bindings)])
        (index-entry (car b) (kind-of (car b) 'value)
                     (sexp->scheme (cdr b)) mod-path (loc-of (car b))))
      (for/list ([c (in-list (tbl 'data-ctors))])
        (index-entry (car c) 'constructor
                     (data-info-scheme (decode-data-info (cdr c)))
                     mod-path (loc-of (car c))))
      (for/list ([t (in-list (tbl 'tcons))])
        (index-entry (car t) 'type #f mod-path (loc-of (car t))))
      (for/list ([c (in-list (tbl 'classes))])
        (index-entry (car c) 'class #f mod-path (loc-of (car c)))))]))

;; The searchable entries a compiled module's sidecar publishes.
;; A module without a sidecar (not Rackton, not compiled) contributes
;; nothing.
(define (module-index-entries mod-path)
  (entries-from-tables (module-sidecar-tables mod-path) mod-path))

(define (index-modules paths)
  (append-map module-index-entries paths))

;; The installed Rackton standard library's index: the library
;; collection files, deliberately listed (requiring arbitrary package
;; files — tests, examples — would run them).
(define (rackton-collection-entries)
  (define root (collection-path "rackton"))
  (define files
    (append
     (for/list ([f (in-list '("batteries.rkt" "system.rkt" "unit.rkt"))])
       (build-path root f))
     (for*/list ([d (in-list '("data" "control" "text" "numeric"
                               "system" "unit"))]
                 [dir (in-value (build-path root d))]
                 #:when (directory-exists? dir)
                 [f (in-list (directory-list dir))]
                 #:when (regexp-match? #rx"\\.rkt$" (path->string f)))
       (build-path dir f))))
  (index-modules (filter file-exists? files)))

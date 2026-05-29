#lang racket/base

;; tests/doc-coverage-test.rkt — every public Rackton export, surface
;; form, provide spec, and REPL command must be documented in the
;; reference.
;;
;; "Documented" means: an @defproc / @defform / @defidform / @defthing
;; / @defmodule entry exists for the name somewhere in
;; scribblings/reference/.  This is checked by text-grep over the .scrbl
;; files — no rendered HTML required.
;;
;; The list of public exports is computed at test time by
;; `module->exports` on `rackton`, with three filters:
;;   * names provided transitively by racket/base are excluded — those
;;     came in via the `(all-from-out racket/base)` re-export and are
;;     not Rackton API;
;;   * names starting with `$` are excluded — these are compile-time-
;;     resolved per-instance impl identifiers (e.g. `$pure:Maybe`,
;;     `$dispatch:flatmap`) that the elaborator emits and that user code
;;     never types;
;;   * the explicit `internal-names` list below covers a handful of
;;     macro-output helpers (`define-data-ctor` etc.) and the
;;     monomorphization-log setters.
;;
;; Surface forms (which are not provides — they are recognised by the
;; parser) and the REPL commands are checked from hand-maintained
;; lists.

(require racket/string
         racket/file
         racket/path
         racket/match
         rackunit)

;; We don't `(require rackton)` directly because rackton re-exports
;; many names that collide with racket/base / racket/list (`first`,
;; `second`, `length`, `reverse`, `+`, `-`, etc.).  `module->exports`
;; only needs the module to be declared in some namespace; the test
;; harness loads it at `(require ...)`-time below via
;; `dynamic-require`.
(dynamic-require 'rackton #f)

;; ----- where the reference docs live -------------------------------

(define reference-dir
  (build-path (path-only (collection-file-path "main.rkt" "rackton"))
              "scribblings" "reference"))

;; ----- expected names ----------------------------------------------

;; Surface forms are macros recognised by private/surface.rkt, not
;; identifiers exported by main.rkt.  Hand-maintained.
(define surface-forms
  '(define : data newtype struct
     protocol instance define-alias define-effect
     lambda λ let let& let% let+ letrec match-let where
     if cond match do list ann update escape racket handle
     require provide foreign
     All))

;; Provide-spec heads recognised in (provide ...) bodies.
(define provide-specs
  '(all-defined-out all-from-out data-out struct-out protocol-out rename-out except-out))

;; REPL commands accepted by the interactive REPL.
(define repl-commands
  '(:type :t :info :i :quit :q :help :h))

;; Internal identifiers — provided but not part of the public language.
(define internal-names
  '(;; Macro-output runtime helpers that user code never references.
    define-data-ctor
    define-class-method
    register-instance-method!
    lookup-return-method
    ;; Re-export of racket/match's `match` for use inside `(racket ...)`
    ;; escapes.  Documented as the surface form `match`, not separately.
    match
    ;; Monomorphization-log setters (the getters
    ;; rackton-monomorphized-sites / rackton-inlined-sites are part of
    ;; the public testing API and are documented).
    set-rackton-monomorphized-log-snapshot!
    set-rackton-inlined-log-snapshot!
    ;; Codegen-only string-concatenation helper for derived Show.
    $show-concat
    ;; The custom #%module-begin renamed from rackton-module-begin.
    rackton-module-begin
    ;; Module-language essentials covered by Racket itself.
    #%module-begin
    #%datum
    #%app
    #%top
    #%top-interaction))

;; ----- introspection -----------------------------------------------

(define (symbol<? a b) (string<? (symbol->string a) (symbol->string b)))

(define (module-exports modpath)
  (define-values (vals stxs) (module->exports modpath))
  (define (collect bucket)
    (apply append (for/list ([phase-row (in-list bucket)])
                    (for/list ([entry (in-list (cdr phase-row))])
                      (car entry)))))
  (append (collect vals) (collect stxs)))

(define (list->seteq xs)
  (for/hasheq ([x (in-list xs)]) (values x #t)))

(define base-export-set
  (list->seteq (module-exports 'racket/base)))

(define rackton-export-set
  (list->seteq (module-exports 'rackton)))

(define (export->required-name? sym)
  (define name (symbol->string sym))
  (and
   ;; Not from racket/base.
   (not (hash-ref base-export-set sym #f))
   ;; Not an internal dispatch / per-instance impl identifier.
   (not (regexp-match? #px"^\\|?\\$" name))
   ;; Not on the explicit internal list.
   (not (memq sym internal-names))))

(define required-export-names
  (sort (filter export->required-name?
                (hash-keys rackton-export-set))
        symbol<?))

;; ----- documented-name extraction ---------------------------------

;; A simple regexp-based scan over every .scrbl file in the reference
;; directory.  We look for the head identifier of each @def... form.
;;
;; Recognised forms (whitespace and newlines allowed between `@def...`
;; and the opening bracket):
;;
;;   @defproc[(name ...) ...]
;;   @defproc*[((name ...) ...) ...]
;;   @defform[(name ...) ...]
;;   @defform*[[(name ...) ...] ...]
;;   @defform/none[(name ...) ...]
;;   @defidform[name]
;;   @defidform[#:kind "..." name]
;;   @defthing[name ...]
;;   @defparam[name ...]
;;   @defmodule[name ...]
;;
;; The first identifier after the leading bracket (skipping a keyword
;; argument like `#:kind "..."`) is the documented name.

(define def-form-regexp
  ;; Matches `@def... [` or `@def... [(` or `@def... [[(` optionally
  ;; followed by one or more `#:keyword value` pairs (where the value
  ;; may be a string or a parenthesised list of identifiers), then any
  ;; combination of opening `[` and `(`, then the documented
  ;; identifier as capturing group 1.
  (pregexp
   (string-append
    "@def(?:proc|proc\\*|form|form\\*|form/none|idform|thing|param|module)"
    "\\*?"
    "\\["
    ;; Zero or more keyword arguments, each either #:kw "str", #:kw id,
    ;; or #:kw (id id ...).  Whitespace and newlines are allowed.
    "(?:\\s*#:[A-Za-z-]+\\s+(?:\"[^\"]*\"|\\([^)]*\\)|[^\\s\\[\\]()]+))*"
    ;; Allow any opening brackets / parens before the identifier.
    "\\s*[\\[\\(]*"
    "([^\\s()\\[\\]]+)")))

(define (extract-documented-names contents)
  (for/list ([m (in-list (regexp-match* def-form-regexp contents
                                        #:match-select cdr))])
    (string->symbol (car m))))

(define documented-name-set
  (let ([set (make-hasheq)])
    (for ([p (in-list (directory-list reference-dir #:build? #t))]
          #:when (regexp-match? #rx"\\.scrbl$" (path->string p)))
      (define contents (file->string p))
      (for ([name (in-list (extract-documented-names contents))])
        (hash-set! set name #t)))
    set))

(define (documented? name)
  (hash-ref documented-name-set name #f))

;; ----- the tests --------------------------------------------------

(define (missing-from category names)
  (filter (lambda (n) (not (documented? n))) names))

(test-case "every public Rackton export is documented in the reference"
  (define missing (missing-from 'exports required-export-names))
  (when (not (null? missing))
    (fail-check
     (format "~a public exports are not documented:~n  ~a"
             (length missing)
             (string-join (map symbol->string missing) "\n  ")))))

(test-case "every surface form is documented in the reference"
  (define missing (missing-from 'surface surface-forms))
  (when (not (null? missing))
    (fail-check
     (format "~a surface forms are not documented:~n  ~a"
             (length missing)
             (string-join (map symbol->string missing) "\n  ")))))

(test-case "every provide spec is documented in the reference"
  (define missing (missing-from 'specs provide-specs))
  (when (not (null? missing))
    (fail-check
     (format "~a provide specs are not documented:~n  ~a"
             (length missing)
             (string-join (map symbol->string missing) "\n  ")))))

(test-case "every REPL command is documented in the reference"
  (define missing (missing-from 'repl repl-commands))
  (when (not (null? missing))
    (fail-check
     (format "~a REPL commands are not documented:~n  ~a"
             (length missing)
             (string-join (map symbol->string missing) "\n  ")))))

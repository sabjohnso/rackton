#lang racket/base

;; prelude-sidecar.rkt — serialize the whole compile-time `prelude-env`
;; into the same s-expression tables a `rackton-schemes` sidecar carries,
;; so the collection-root module `rackton/prelude` can publish the prelude
;; for qualified import (`(require (qualified-in p rackton/prelude))`).
;;
;; This is the mirror image of elaborate.rkt's per-provide sidecar
;; encoder: that path EXCLUDES prelude names (importers already have
;; them); this path emits ONLY prelude names.  Both use the same
;; `scheme-codec` encoders, so importers decode either identically.
;;
;; Instances are omitted (`'()`): every Rackton module already starts
;; from `prelude-env`, so it already holds every prelude instance;
;; re-exporting them is redundant and only instances carry the
;; conflict-raising coherence check.  Types/classes/families fold via
;; hash-set and harmlessly overwrite identical entries.
;;
;; Public API: `prelude-sidecar-ref`, keyed by the sidecar category
;; symbol (e.g. 'bindings) → the datum the corresponding `rackton-*`
;; export should hold.

(require "prelude.rkt"       ; prelude-env
         "env.rkt"           ; (struct-out env) table accessors, env-ref-*
         "scheme-codec.rkt"  ; encoders
         (only-in "ast.rkt" sugar-reserved-keys))

(provide prelude-sidecar-ref)

;; Reserved internal keys ($sugar:…) live in prelude-env for literal-sugar
;; resolution; they are not user-facing, so they are excluded from the
;; exported sidecar.
(define internal-ctor-keys
  (map cdr sugar-reserved-keys))

;; Each category: iterate the corresponding prelude-env table, encoding
;; every (name . value) with the matching codec.  Names are emitted BARE;
;; a qualified/prefixed import applies its rename on the importer side.
(define (encode-table table encode)
  (for/list ([(name val) (in-hash table)]
             #:unless (memq name internal-ctor-keys))
    (cons name (encode val))))

(define sidecar-table
  (list
   (cons 'bindings        (encode-table (env-vars prelude-env)            scheme->sexp))
   (cons 'data-ctors      (encode-table (env-data-ctors prelude-env)      encode-data-info))
   (cons 'tcons           (encode-table (env-tcons prelude-env)           encode-tcon-info))
   (cons 'classes         (encode-table (env-classes prelude-env)         encode-class-info))
   ;; Omitted — see the header comment on instances.
   (cons 'instances       '())
   ;; Codegen-only force-exports / macros / def sites / requires: none for
   ;; the prelude.  Present so the sidecar has the full shape importers and
   ;; tools expect.
   (cons 'exported-impls  '())
   (cons 'macros          '())
   (cons 'defs            '())
   (cons 'promoted        (encode-table (env-promoted-ctors prelude-env)  encode-kind-scheme))
   (cons 'tyfams          (encode-table (env-tyfams prelude-env)          encode-tyfam-info))
   (cons 'constraint-syns (encode-table (env-constraint-syns prelude-env) encode-constraint-syn))
   (cons 'constraint-fams (encode-table (env-constraint-fams prelude-env) encode-constraint-fam-info))
   ;; Variadic arity is a plain integer — no codec.
   (cons 'variadics       (for/list ([(name arity) (in-hash (env-variadics prelude-env))])
                            (cons name arity)))
   (cons 'requires        '())))

(define (prelude-sidecar-ref key)
  (cond
    [(assq key sidecar-table) => cdr]
    [else (error 'prelude-sidecar-ref "no such sidecar category: ~s" key)]))

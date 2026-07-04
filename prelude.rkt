#lang racket/base

;; rackton/prelude — the implicit prelude, made into a real, importable
;; module so it can be QUALIFIED:
;;
;;   (require (qualified-in p rackton/prelude))
;;   (match xs [(p:Cons x xs) ...] [p:Nil ...])
;;
;; Every Rackton program already starts from the prelude implicitly; this
;; module exposes the same names under a handle a `prefix-in` /
;; `qualified-in` can rename.  Its role is purely to be imported: a module
;; that locally shadows a prelude name (its own `Cons`) can still reach the
;; prelude's as `p:Cons`.
;;
;; Two halves, mirroring every Rackton module:
;;   - Runtime: re-export the prelude runtime value bindings.
;;   - Types: a `rackton-schemes` sidecar submodule carrying the prelude's
;;     schemes, so an importer's type checker recovers them by
;;     dynamic-require (exactly as for a user `#lang rackton` module).

;; Runtime bindings.  prelude-runtime.rkt is the runtime home of every
;; user-facing prelude value (constructors, class-method dispatchers,
;; primitives); re-export it wholesale.  A handful of internal
;; representation helpers ($map, rackton-tuple-make, …) ride along, but
;; they carry no scheme in the sidecar below, so they are untypeable —
;; harmless under a prefix.  (main.rkt excepts them from its UNPREFIXED
;; user re-export, where an untyped name would shadow; here the prefix
;; makes that moot.)
(require "private/prelude-runtime.rkt")
(provide (all-from-out "private/prelude-runtime.rkt"))

;; Type sidecar.  Built from the compile-time `prelude-env` by
;; private/prelude-sidecar.rkt; provides the same `rackton-*` names an
;; importer's `require` fold-in reads (infer.rkt handle-require-form).
(module rackton-schemes racket/base
  (require (only-in rackton/private/prelude-sidecar prelude-sidecar-ref))
  (provide rackton-bindings
           rackton-data-ctors
           rackton-tcons
           rackton-classes
           rackton-instances
           rackton-exported-impls
           rackton-macros
           rackton-defs
           rackton-promoted
           rackton-tyfams
           rackton-constraint-syns
           rackton-constraint-fams
           rackton-variadics
           rackton-requires)
  (define rackton-bindings        (prelude-sidecar-ref 'bindings))
  (define rackton-data-ctors      (prelude-sidecar-ref 'data-ctors))
  (define rackton-tcons           (prelude-sidecar-ref 'tcons))
  (define rackton-classes         (prelude-sidecar-ref 'classes))
  (define rackton-instances       (prelude-sidecar-ref 'instances))
  (define rackton-exported-impls  (prelude-sidecar-ref 'exported-impls))
  (define rackton-macros          (prelude-sidecar-ref 'macros))
  (define rackton-defs            (prelude-sidecar-ref 'defs))
  (define rackton-promoted        (prelude-sidecar-ref 'promoted))
  (define rackton-tyfams          (prelude-sidecar-ref 'tyfams))
  (define rackton-constraint-syns (prelude-sidecar-ref 'constraint-syns))
  (define rackton-constraint-fams (prelude-sidecar-ref 'constraint-fams))
  (define rackton-variadics       (prelude-sidecar-ref 'variadics))
  (define rackton-requires        (prelude-sidecar-ref 'requires)))

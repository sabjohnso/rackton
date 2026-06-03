#lang racket/base

;; Single source of truth for the surface-form and provide-spec name
;; lists that BOTH doc tests iterate over:
;;   - tests/doc-coverage-test.rkt checks every name is DOCUMENTED in
;;     the reference, and
;;   - tests/doc-linking-test.rkt checks every name is BOUND / links.
;; These two lists used to be copied into each test and drifted apart
;; (e.g. `foreign-c` was added to one but not the other).  Keeping the
;; single copy here removes that drift.
;;
;; (The other categorisations are genuinely test-specific and stay
;; local: doc-coverage owns repl-commands / internal-names; doc-linking
;; owns type-ctors / classes / return-typed-methods — names it checks
;; bind, which doc-coverage instead discovers by introspecting the
;; module's exports.)

(provide surface-forms
         provide-specs)

;; Surface forms are macros recognised by private/surface.rkt, not
;; identifiers exported by main.rkt.
(define surface-forms
  '(define : data newtype struct
     protocol instance define-alias define-effect
     lambda λ let let& let% let+ letrec let*
     if cond match do delay list ann update escape racket handle
     require provide foreign foreign-c
     All))

;; Provide-spec heads recognised in (provide ...) bodies.
(define provide-specs
  '(all-defined-out all-from-out data-out struct-out protocol-out rename-out except-out))

#lang racket/base

;; Rackton — the inference → codegen interface.
;;
;; Inference computes several tables that codegen needs but cannot derive
;; on its own (it has erased the types).  Historically these crossed the
;; phase boundary as ambient mutable parameter cells that the elaborator
;; created blank and hoped inference filled — an implicit, untyped contract
;; invisible to the module graph.  This struct makes the contract explicit:
;; `infer-program+forms` produces a `codegen-plan`, and `compile-top` takes
;; one.  The dynamic parameters in infer.rkt are now a codegen-internal
;; implementation detail, re-established from the plan at the codegen entry.
;;
;; Fields:
;;   method-resolutions      stx -> impl-name symbol.  Return-typed and
;;                           positional-monomorphized call sites.
;;   method-dict-resolutions stx -> (listof impl-name) prepended at the call.
;;   needs-dict-defs         def-key -> dict-arg names to prepend as lambda
;;                           params.
;;   instance-default-bodies (list class head-tcon method) -> the freshened
;;                           per-instance default-method body AST.
;;   return-typed-methods    set of method names that dispatch return-typed
;;                           (drives the runtime-table-vs-direct-impl choice).

(provide (struct-out codegen-plan)
         empty-codegen-plan)

(struct codegen-plan (method-resolutions
                      method-dict-resolutions
                      needs-dict-defs
                      instance-default-bodies
                      return-typed-methods)
  #:transparent)

;; The no-information plan: every channel #f, matching the parameters'
;; default (unbound) state.  Used as compile-top's default argument so
;; isolated callers (e.g. codegen unit tests) need not build one.
(define empty-codegen-plan (codegen-plan #f #f #f #f #f))

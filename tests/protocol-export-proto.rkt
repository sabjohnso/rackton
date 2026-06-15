#lang rackton

;; Fixture for tests/protocol-export-test: a user-defined protocol whose
;; instances are declared in a DIFFERENT module.  Exercises that
;; `(protocol-out …)` exports the protocol's runtime dispatch tables, so
;; cross-module instance registration resolves.

(provide (protocol-out Pretty))

(protocol (Pretty a)
  (: pretty (-> a String)))

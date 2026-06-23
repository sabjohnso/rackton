#lang racket/base

;; A return-typed (nullary) class method must be re-exportable and usable
;; across a module boundary.  Its call site compiles to a lookup against the
;; per-method dispatch table $dispatch:<m>, so re-exporting the protocol must
;; publish that table; the bare method name has no runtime binding and must
;; not be provided.

(require rackunit
         "../main.rkt")

;; protocol-out path: theBot resolves by result type, cross-module.
(rackton
  (require "return-typed-export-lib.rkt")
  (: bot-int Integer)  (define bot-int theBot)
  (: bot-bool Boolean) (define bot-bool theBot))

;; all-defined-out path.
(rackton
  (require "return-typed-export-lib-ado.rkt")
  (: u Integer) (define u theUnit))

(test-case "return-typed method via protocol-out crosses (Integer instance)"
  (check-equal? bot-int 0))

(test-case "return-typed method via protocol-out crosses (Boolean instance)"
  (check-equal? bot-bool #t))

(test-case "return-typed method via all-defined-out crosses"
  (check-equal? u 99))

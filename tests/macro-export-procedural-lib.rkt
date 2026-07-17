#lang rackton

;; A Rackton library whose exported macro is a procedural transformer.
;; Evaluating its definition needs the library's own `(for-syntax …)`
;; requires (`syntax-parser` comes from syntax/parse) — an importer must
;; inherit those phase-1 requires from the sidecar, since it does not
;; write them itself.

(provide splice-sum)

;; A plain (non-for-syntax) require, so this library's sidecar `requires`
;; table carries a non-`(for-syntax …)` entry — importers must select only
;; the for-syntax specs from it.
(require "macro-export-lib.rkt")

(require (for-syntax racket/base syntax/parse))

;; (splice-sum 2 arg) expands to (+ (+ arg 0) (+ arg 1)).
(define-syntax splice-sum
  (syntax-parser
   [(_ n:nat arg:expr)
    (with-syntax ([(index ...) (for/list ([i (in-range (syntax->datum #'n))]) i)])
      #'(+ (+ arg index) ...))]))

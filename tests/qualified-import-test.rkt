#lang racket/base

;; `(require (qualified-in st mod))` namespaces the import's TERM-level
;; names behind the `st:` prefix: values and data constructors become
;; `st:depth`, `st:Push`, `st:Empty` (in expressions and patterns).  Type
;; constructors keep their plain names, so a constructor's result type
;; stays consistent with the type the importer annotates.

(require rackunit
         (for-syntax racket/base)
         "../main.rkt")

(rackton
  (require (qualified-in st "qualified-import-lib.rkt"))

  ;; Qualified constructors in an expression; the type `Stack` is plain.
  (: s (Stack Integer))
  (define s (st:Push 1 (st:Push 2 st:Empty)))

  ;; Qualified value (function) call.
  (: d Integer)
  (define d (st:depth s))

  ;; Qualified constructors in patterns: nullary `st:Empty` and applied
  ;; `(st:Push v _)`.
  (: top-of (-> (Stack Integer) Integer))
  (define (top-of x)
    (match x
      [st:Empty       0]
      [(st:Push v _)  v]))

  (: t Integer)
  (define t (top-of s)))

(test-case "qualified construction and qualified function call"
  (check-equal? d 2))

(test-case "qualified type annotation and qualified patterns"
  (check-equal? t 1))

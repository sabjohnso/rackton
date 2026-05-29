#lang racket/base

;; Enabler B regression: the client reaches C's (Eq Color) via BOTH A
;; and B — a diamond.  Before Enabler B this was a hard instance-conflict
;; error; now the same instance (equal origins) is deduped and the
;; program type-checks and runs.

(require rackunit
         "../main.rkt")

(rackton
  (require "instance-diamond-lib-a.rkt")
  (require "instance-diamond-lib-b.rkt")

  (: result Boolean)
  (define result (if a-val b-val b-val)))

(test-case "diamond import of one instance"
  (check-true result))

#lang rackton

;; Enabler B regression: a second import path to C's (Eq Color).

(require "instance-diamond-lib-c.rkt")
(provide (all-defined-out))

(: b-val Boolean)
(define b-val (== Green Green))

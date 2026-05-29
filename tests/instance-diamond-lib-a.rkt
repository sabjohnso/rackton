#lang rackton

;; Enabler B regression: one import path to C's (Eq Color).

(require "instance-diamond-lib-c.rkt")
(provide (all-defined-out))

(: a-val Boolean)
(define a-val (== Red Green))

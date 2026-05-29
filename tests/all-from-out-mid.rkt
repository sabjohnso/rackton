#lang rackton

;; Enabler C: re-export everything from the leaf module without naming
;; each binding — the (all-from-out M) provide spec.

(require "all-from-out-lib.rkt")
(provide (all-from-out "all-from-out-lib.rkt"))

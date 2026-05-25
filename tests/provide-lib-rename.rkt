#lang rackton

;; (rename-out [internal external]) — the binding escapes under a
;; different name.  `internal-name` should NOT be reachable in
;; importers; `external-name` should.

(define internal-name 42)

(provide (rename-out [internal-name external-name]))

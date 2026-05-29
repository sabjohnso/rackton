#lang rackton

;; A #lang rackton module that reaches a host binding via `foreign` and
;; both wraps it (mentions-rackton) and re-exports the foreign binding
;; itself (contains?) — exercising the stdlib use case + the sidecar.

(provide (all-defined-out))

(foreign contains? (-> String (-> String Boolean))
         #:from racket/string #:as string-contains?)

(: mentions-rackton (-> String Boolean))
(define (mentions-rackton s) (contains? s "rackton"))

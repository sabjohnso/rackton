#lang rackton

;; Same, but exported through `(all-defined-out)` — the other provide path
;; that must skip the bare name of a return-typed method.

(provide (all-defined-out))

(protocol (HasUnit a) (: theUnit a))

(instance (HasUnit Integer) (define theUnit 99))

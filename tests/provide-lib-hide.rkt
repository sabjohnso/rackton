#lang rackton

;; `pub` is exported; `priv` is hidden.  A client that tries to
;; mention `priv` should fail at compile time — provide gates both
;; runtime visibility and the type-level sidecar.

(: pub Integer)
(define pub 1)

(: priv Integer)
(define priv 2)

(provide pub)

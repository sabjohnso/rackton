#lang rackton

;; Library exporting a protocol with a RETURN-TYPED (nullary) method via
;; `protocol-out`.  `theBot` dispatches on its result type, so it has no
;; plain runtime binding — only the per-method table `$dispatch:theBot`.
;; Re-exporting it must publish that table, not the (non-existent) bare name.

(provide (protocol-out HasBot))

(protocol (HasBot a) (: theBot a))

(instance (HasBot Integer) (define theBot 0))
(instance (HasBot Boolean) (define theBot #t))

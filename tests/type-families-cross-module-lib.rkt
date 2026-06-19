#lang rackton

;; Fixture for type-families-cross-module-test.rkt.  A library that
;; declares standalone type families (closed `Other`, open `Tag`); an
;; importing module must recover them from the `rackton-schemes` sidecar
;; so its own type checker can reduce family applications.

(provide (data-out Color))

(data Color Red Green)

;; Closed family keyed on the promoted Color tags.
(type-family (Other c)
  [Red   = Integer]
  [Green = String])

;; Open family extended by standalone equations.
(type-family (Tag c))
(type-instance (Tag Red)   = String)
(type-instance (Tag Green) = Integer)

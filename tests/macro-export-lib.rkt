#lang rackton

;; A Rackton library that exports a macro (Option B: macros cross `require`).
;; `double` is a pattern macro; `quadruple` is defined in terms of it, to
;; check that a library's macros may build on one another before export.

(provide double quadruple)

(define-syntax-rule (double x) (+ x x))

(define-syntax-rule (quadruple x) (double (double x)))

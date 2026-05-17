#lang racket/base

;; Rackton — pattern compilation.
;;
;; (compile-pattern p) → syntax-object
;;
;; Translates a surface pattern AST node into a Racket match pattern.
;; Constructor patterns are emitted by name so that the match expanders
;; created by `define-data-ctor` (private/adt.rkt) handle the actual
;; struct-shape match.  The original syntax object's lexical context is
;; preserved so that those match expanders resolve correctly.
;;
;; Pattern correspondence
;;   p:wild  → _
;;   p:var x → x
;;   p:lit v → v   (literal datum, comparison by equal?)
;;   p:ctor C (p ...) → (C compiled-p ...)

(provide compile-pattern)

(require racket/match
         "surface.rkt")

(define (compile-pattern p)
  (match p
    [(p:wild stx)
     (datum->syntax stx '_ stx)]
    [(p:var name stx)
     (datum->syntax stx name stx)]
    [(p:lit v stx)
     (datum->syntax stx v stx)]
    [(p:ctor name args stx)
     (define name-stx (datum->syntax stx name stx))
     (define arg-stxs (map compile-pattern args))
     (datum->syntax stx (cons name-stx arg-stxs) stx)]))

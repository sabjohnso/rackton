#lang racket/base

;; Reading the macro-export tables of a Rackton library's
;; `rackton-schemes` sidecar submodule.  This module is the single owner
;; of that read policy — the table keys, the shape of a `(for-syntax …)`
;; require spec, the "only macro-exporting libraries contribute phase-1
;; requires" gate, and the missing-sidecar fallback — so the two
;; consumers (the module pipeline in elaborate.rkt, at phase 1, and the
;; REPL in repl.rkt, at phase 0) cannot drift apart.
;;
;; Public API:
;;   sidecar-macro-entries              submod -> (listof (cons names datum))
;;   sidecar-macro-for-syntax-requires  submod -> (listof spec-datum)

(provide sidecar-macro-entries
         sidecar-macro-for-syntax-requires)

;; A sidecar table, or '() when the module has no sidecar (not a Rackton
;; library) or the table is absent.
(define (sidecar-table submod key)
  (with-handlers ([exn:fail? (lambda (_) '())])
    (dynamic-require submod key)))

;; The macros the library itself defines and provides, as
;; (cons name-symbols definition-datum) entries.
(define (sidecar-macro-entries submod)
  (sidecar-table submod 'rackton-macros))

;; The `(for-syntax …)` require spec datums the library recorded — the
;; phase-1 imports its transformer bodies need.  Empty unless the library
;; exports macros: an importer only needs these specs to re-evaluate those
;; macro definitions, so a macro-less library contributes nothing.  The
;; sidecar records ALL require specs verbatim (plain module paths
;; included); only the `(for-syntax …)` ones are selected here.
(define (sidecar-macro-for-syntax-requires submod)
  (cond
    [(null? (sidecar-macro-entries submod)) '()]
    [else
     (for/list ([r (in-list (sidecar-table submod 'rackton-requires))]
                #:when (and (pair? r) (eq? (car r) 'for-syntax)))
       r)]))

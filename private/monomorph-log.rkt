#lang racket/base

;; Rackton — compile-time monomorphization bookkeeping.
;;
;; `current-monomorphized-sites` is a box of (method . impl) pairs.  When a
;; positional class-method call site's dispatch type resolves to a concrete
;; tcon, inference's instance resolver (resolve-method-uses) records the
;; redirection here; the elaborator reads it back out via the snapshot so the
;; runtime can report it through `rackton-monomorphized-sites`.
;;
;; The inlining bookkeeping that codegen produces (the inlinable-body registry,
;; the inlining-stack guard, the inlined-sites log) no longer lives here — it
;; is threaded through codegen's immutable `cg-st` instead.  This module is now
;; only the (inference-side) monomorphization log.

(provide current-monomorphized-sites
         make-monomorph-log
         record-monomorphized-site!
         monomorphized-sites-snapshot)

;; Per-elaborate state.  #f outside an elaborate; bound by the elaborator via
;; `parameterize` to a fresh log for each module.
(define current-monomorphized-sites (make-parameter #f))

;; A fresh log value for the elaborator's parameterize.
(define (make-monomorph-log) (box '()))

;; Append a (method . impl) pair.  No-op when the log is unset (outside an
;; elaborate).
(define (record-monomorphized-site! method impl)
  (define b (current-monomorphized-sites))
  (when b (set-box! b (cons (cons method impl) (unbox b)))))

;; The accumulated site list (newest-first, as recorded), or '() when unset.
(define (monomorphized-sites-snapshot)
  (define b (current-monomorphized-sites))
  (if b (unbox b) '()))

#lang racket/base

;; Rackton — compile-time monomorphization & inlining bookkeeping.
;;
;; Three pieces of per-elaborate state record what the optimizer did so
;; that (a) codegen can act on it and (b) the runtime can report it via
;; `rackton-monomorphized-sites` / `rackton-inlined-sites`:
;;
;;   current-monomorphized-sites — a box of (method . impl) pairs.  When a
;;     positional class-method call site's dispatch type resolves to a
;;     concrete tcon, inference's instance resolver records the
;;     redirection here.
;;
;;   current-inlinable-bodies — a hasheq from an impl-name symbol
;;     (e.g. '$tag-of:Integer) to its lambda AST.  Codegen registers a
;;     small leaf instance body here; the call-site codegen consults it.
;;
;;   current-inlined-sites — a box of (method . impl) pairs accumulated as
;;     codegen substitutes a registered body in place of a call.
;;
;; The storage shapes (box vs hasheq) are private to this module: every
;; producer and consumer goes through the record/lookup/snapshot
;; interface below, so the representation can change without rippling
;; into infer.rkt, codegen.rkt, or elaborate.rkt.  The inlining *policy*
;; (which bodies are small enough to register) lives in codegen — this
;; module is only the bookkeeping.

(provide current-monomorphized-sites
         current-inlinable-bodies
         current-inlined-sites
         make-monomorph-log
         make-inlinable-registry
         record-monomorphized-site!
         record-inlined-site!
         register-inlinable-body!
         lookup-inlinable-body
         monomorphized-sites-snapshot
         inlined-sites-snapshot)

;; Per-elaborate state.  #f outside an elaborate; bound by the elaborator
;; via `parameterize` to fresh log/registry values for each module.
(define current-monomorphized-sites (make-parameter #f))
(define current-inlinable-bodies    (make-parameter #f))
(define current-inlined-sites       (make-parameter #f))

;; Fresh log / registry values for the elaborator's parameterize.
(define (make-monomorph-log)     (box '()))
(define (make-inlinable-registry) (make-hasheq))

;; Append a (method . impl) pair to a site log.  No-op when the log
;; parameter is unset (outside an elaborate).
(define (push-site! param method impl)
  (define b (param))
  (when b (set-box! b (cons (cons method impl) (unbox b)))))

(define (record-monomorphized-site! method impl)
  (push-site! current-monomorphized-sites method impl))

(define (record-inlined-site! method impl)
  (push-site! current-inlined-sites method impl))

;; Register an inlinable body under its impl name.  No-op when the
;; registry parameter is unset.  Eligibility (size, shape) is decided by
;; the caller in codegen before registering.
(define (register-inlinable-body! impl-name body)
  (define h (current-inlinable-bodies))
  (when h (hash-set! h impl-name body)))

;; The body registered for `impl-name`, or #f (also #f when the registry
;; is unset).
(define (lookup-inlinable-body impl-name)
  (define h (current-inlinable-bodies))
  (and h (hash-ref h impl-name #f)))

;; The accumulated site list (newest-first, as recorded), or '() when the
;; log is unset.  Read by the elaborator when publishing to the runtime.
(define (snapshot param)
  (define b (param))
  (if b (unbox b) '()))

(define (monomorphized-sites-snapshot) (snapshot current-monomorphized-sites))
(define (inlined-sites-snapshot)       (snapshot current-inlined-sites))

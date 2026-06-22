#lang racket/base

;; Coverage Phase 2 — the coverage matrix (Coverage.org).
;;
;; Joins the EXERCISED axis (dispatch log written by
;; coverage/dispatch-log.rkt during an instrumented test run) against the
;; DENOMINATOR (the authoritative instance inventory from Phase 1,
;; coverage/instance-inventory.rkt).  For each declared instance cell
;; (class, type) it reports covered / uncovered.
;;
;; Usage:
;;   : > /tmp/cov.log
;;   RACKTON_COVERAGE=/tmp/cov.log raco test -p rackton
;;   racket coverage/matrix.rkt /tmp/cov.log
;;
;; The log records (method, tag) pairs.  We map method -> class (from the
;; class tables of the prelude and every sidecar) and tag -> type
;; (`$ctor:Some` -> the constructor's data type; a primitive tag IS its
;; type), yielding exercised (class, type) cells.
;;
;; This is a LOWER BOUND.  The log records only real method dispatches, so
;; a "covered" cell truly ran — that half is trustworthy.  But a call can
;; reach an instance WITHOUT dispatching: through a passed dictionary
;; (polymorphic code), a monomorphized rewrite, or a return-typed method
;; referenced by its direct binding (`$mempty:String`, `$pure:Maybe`).
;; Those bypass the dispatch table, so a cell can read "uncovered" yet be
;; exercised.  Every uncovered cell is therefore labelled "verify": it was
;; not seen via dispatch and needs a manual or complete-build check.  (The
;; complete, dict/monomorph-aware build was deferred — it needs codegen
;; instrumentation; see Coverage.org Phase 2.)

(require racket/list
         racket/string
         racket/path
         (only-in "../private/prelude.rkt" prelude-env)
         "../private/env.rkt"
         "../private/scheme-codec.rkt"
         "instance-inventory.rkt")

(provide build-matrix)

;; ----- sidecar reader -------------------------------------------------

(define (sidecar-export path sym)
  (with-handlers ([exn:fail? (lambda (_) '())])
    (dynamic-require
     `(submod (file ,(path->string (simplify-path path))) rackton-schemes)
     sym)))

;; ----- method -> class ------------------------------------------------

(define (method->class)
  (define m (make-hash))
  (define (add! cls ci)
    (for ([meth (in-hash-keys (class-info-methods ci))])
      (hash-set! m meth cls)))
  (for ([(cls ci) (in-hash (env-classes prelude-env))]) (add! cls ci))
  (for* ([f (in-list (stdlib-files))]
         [entry (in-list (sidecar-export f 'rackton-classes))])
    (add! (car entry) (decode-class-info (cdr entry))))
  m)

;; ----- type tag -> type name ------------------------------------------

(define (ctor->type)
  (define m (make-hash))
  (for ([(ctor di) (in-hash (env-data-ctors prelude-env))])
    (hash-set! m ctor (data-info-type-name di)))
  (for* ([f (in-list (stdlib-files))]
         [entry (in-list (sidecar-export f 'rackton-data-ctors))])
    (hash-set! m (car entry) (data-info-type-name (decode-data-info (cdr entry)))))
  m)

;; `$ctor:Some` -> 'Maybe ;  a primitive tag (Integer, ->, …) IS its type.
(define (tag->type c->t tag)
  (define s (symbol->string tag))
  (if (string-prefix? s "$ctor:")
      (hash-ref c->t (string->symbol (substring s 6)) #f)
      tag))

;; ----- exercised cells from the log -----------------------------------

(define (exercised-cells log-file)
  (define m->c (method->class))
  (define c->t (ctor->type))
  (define cells (make-hash))
  (define unmatched (make-hash))   ; (method . tag) we could not map
  (for ([line (in-lines (open-input-file log-file))])
    (define parts (string-split line "\t"))
    (when (= 2 (length parts))
      (define method (string->symbol (first parts)))
      (define tag    (string->symbol (second parts)))
      (define cls (hash-ref m->c method #f))
      (define typ (tag->type c->t tag))
      (cond
        [(and cls typ) (hash-set! cells (cons cls typ) #t)]
        [else (hash-set! unmatched (cons method tag) #t)])))
  (values cells unmatched))

;; ----- the matrix -----------------------------------------------------

;; Returns (values rows unmatched), rows = (list class type source status),
;; status ∈ {covered verify}.  "covered" = seen dispatching (trustworthy);
;; "verify" = NOT seen via dispatch (may still be exercised via a dict /
;; monomorphized / direct-binding call — needs checking).
(define (build-matrix log-file)
  (define-values (ex unmatched) (exercised-cells log-file))
  (define rows
    (for/list ([r (in-list (collect-instance-records))])
      (define cell (cons (first r) (second r)))
      (list (first r) (second r) (third r)
            (if (hash-has-key? ex cell) 'covered 'verify))))
  (values rows unmatched))

;; ----- report ---------------------------------------------------------

(module+ main
  (define args (current-command-line-arguments))
  (when (zero? (vector-length args))
    (error 'matrix "usage: racket coverage/matrix.rkt <coverage-log-file>"))
  (define log-file (vector-ref args 0))
  (define-values (rows unmatched) (build-matrix log-file))

  (define total    (length rows))
  (define covered  (count (lambda (r) (eq? (fourth r) 'covered)) rows))
  (define verify   (count (lambda (r) (eq? (fourth r) 'verify))  rows))

  (printf "Coverage matrix (Coverage Phase 2 — dispatch lower bound)\n")
  (printf "========================================================\n")
  (printf "instance cells:         ~a\n" total)
  (printf "covered (trustworthy):  ~a  (~a%)\n" covered
          (if (zero? total) 0 (round (/ (* 100 covered) total))))
  (printf "verify (not seen via dispatch): ~a\n\n" verify)

  (define (dump status title)
    (define these (filter (lambda (r) (eq? (fourth r) status)) rows))
    (unless (null? these)
      (printf "## ~a (~a)\n" title (length these))
      (for ([r (in-list (sort these string<?
                              #:key (lambda (r) (format "~a/~a" (first r) (second r)))))])
        (printf "  ~a ~a\t[~a]\n" (first r) (second r) (third r)))
      (printf "\n")))
  (dump 'verify "VERIFY — not exercised via dispatch (check dict/monomorph/direct-binding)")

  (unless (zero? (hash-count unmatched))
    (printf "## unmatched log pairs (method, tag not mapped to a cell): ~a\n"
            (hash-count unmatched))
    (for ([p (in-hash-keys unmatched)])
      (printf "  ~a\t~a\n" (car p) (cdr p)))))

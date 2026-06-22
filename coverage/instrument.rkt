#lang racket/base

;; Coverage Phase 2 — instrumentation, COMPILE-TIME GATED (Coverage.org).
;;
;; A normal build emits NO instrumentation: the gate `when-coverage`
;; expands to nothing, and `coverage-build-codegen?` returns #f so codegen
;; emits instance impls exactly as today.  The instrumentation exists only
;; in a COVERAGE BUILD — a compile performed with the environment variable
;; RACKTON_COVERAGE_BUILD set.  So non-coverage builds are byte-identical
;; and pay nothing (no call, no check, no dependency in their emitted
;; code).
;;
;; Two flags, deliberately separate:
;;   RACKTON_COVERAGE_BUILD  (compile time) — emit instrumentation?
;;   RACKTON_COVERAGE        (run time)     — file to append the log to.
;;
;; Caveat on the build cache: Racket's `.zo` cache keys on source, not on
;; env vars, so a coverage build must compile to a SEPARATE compiled root
;; (PLTCOMPILEDROOTS=…) to avoid poisoning the normal cache.

(require (for-syntax racket/base))

(provide when-coverage              ; macro: gated at THIS module's compile
         coverage-build-codegen?    ; fn: gate read by codegen at emit time
         record-cover!              ; runtime: append one (method, tag)
         cover-fn)                  ; runtime: wrap a method impl to log on call

;; ----- compile-time gate ----------------------------------------------

(define-for-syntax coverage-build?
  (and (getenv "RACKTON_COVERAGE_BUILD") #t))

;; `(when-coverage body ...)` => `(begin body ...)` in a coverage build,
;; `(begin)` (nothing) otherwise.  Use in package modules (dict.rkt,
;; prelude-runtime.rkt) so the off-build emits no code at all.
(define-syntax (when-coverage stx)
  (syntax-case stx ()
    [(_ body ...) (if coverage-build? #'(begin body ...) #'(begin))]))

;; Codegen runs at the compile time of USER/stdlib code and emits syntax;
;; it reads the gate as a plain runtime call at that point, so the emitted
;; `.zo` carries the wrap only when this is a coverage build.
(define (coverage-build-codegen?)
  (and (getenv "RACKTON_COVERAGE_BUILD") #t))

;; ----- runtime recorder (only reached in a coverage build) ------------

(define coverage-file (getenv "RACKTON_COVERAGE"))

(define seen (make-hash))

(define port
  (and coverage-file (open-output-file coverage-file #:exists 'append)))

;; Append one (method, tag) on first sighting in this process; later
;; repeats are dropped.  Silent when RACKTON_COVERAGE is unset at run time
;; (a coverage-built program run without a log target).
(define (record-cover! method tag)
  (when port
    (define key (cons method tag))
    (unless (hash-has-key? seen key)
      (hash-set! seen key #t)
      (fprintf port "~a\t~a\n" method tag)
      (flush-output port))))

;; Wrap a method impl so each invocation logs (method, tag) before
;; running.  Codegen emits `(cover-fn 'method 'Tcon <impl>)` around an
;; instance impl's binding in a coverage build, so EVERY path to it —
;; runtime dispatch, a dict-passed reference, or a monomorphized direct
;; call — goes through the same wrapper.  Self-guards on `procedure?`:
;; a return-typed VALUE impl (e.g. `mempty`) is returned untouched, since
;; a value has no invocation to log and must not become a function.
(define (cover-fn method tag impl)
  (if (procedure? impl)
      (lambda args
        (record-cover! method tag)
        (apply impl args))
      impl))

#lang racket/base

;; Rackton — pipeline glue.
;;
;; (rackton form ...) is a macro that:
;;   1. Parses each surface form via `parse-top`.
;;   2. Type-checks the program via `infer-program`.
;;   3. Compiles type-checked forms to Racket syntax via `compile-top`.
;;   4. Splices the result into the surrounding module as `(begin ...)`.
;;
;; A type error is raised at compile time as a syntax error, with the
;; offending form's source location attached.

(provide rackton rackton/main)

(require (for-syntax racket/base
                     syntax/parse
                     racket/list
                     "surface.rkt"
                     "infer.rkt"
                     "codegen.rkt"
                     "prelude.rkt"
                     "scheme-codec.rkt"
                     "env.rkt")
         ;; Phase 57: runtime require so that the macro templates
         ;; below can reference set-rackton-monomorphized-log-
         ;; snapshot! (and rackton-monomorphized-sites) — Racket's
         ;; hygiene then makes the spliced identifier resolve to
         ;; the runtime binding here.
         "prelude-runtime.rkt")

;; Shared elaboration helper: returns (values compiled-syntax-list
;; bindings-data data-ctors-data tcons-data classes-data instances-data).
(define-for-syntax (rackton-elaborate forms-stx)
  (define parsed
    (parse-toplevel-list (syntax->list forms-stx)))
  ;; Return-typed class methods are resolved at compile time, with the
  ;; resolution communicated from inference to codegen via these two
  ;; hashtables.  Inference populates `current-method-uses` then
  ;; settles it into `current-method-resolutions` after each top-def's
  ;; constraints reduce; codegen consults the resolutions when
  ;; emitting `e:var` references.
  (parameterize ([current-method-uses             (make-hasheq)]
                 [current-method-resolutions      (make-hasheq)]
                 [current-method-dict-resolutions (make-hasheq)]
                 ;; equal?-keyed so we can use composite list keys
                 ;; for instance method lookups (Phase 30).  Symbol-
                 ;; keyed top-def names compare equal? fine too.
                 [current-needs-dict-defs         (make-hash)]
                 ;; Phase 57: monomorphization log starts empty per
                 ;; elaborate, accumulates each resolved site.
                 [current-monomorphized-sites     (box '())])
    (define env (infer-program parsed prelude-env))
    (define compiled
      (filter values
              (for/list ([f (in-list parsed)])
                (compile-top f env))))
    ;; Phase 57: emit a runtime form that publishes this elaborate's
    ;; monomorphization log via the codegen-exposed setter.  The
    ;; rackton-monomorphized-sites accessor returns this list so
    ;; tests can verify the optimization fired.
    (define mono-log (unbox (current-monomorphized-sites)))
    ;; Phase 57: pass the log alongside compiled forms; the rackton
    ;; macro turns it into a runtime form using a `for-template`
    ;; binding it has but rackton-elaborate doesn't.
    (define-values (final-compiled bs dcs tcs cls insts)
      (elaborate-finish parsed env compiled))
    (values final-compiled bs dcs tcs cls insts mono-log)))

(define-for-syntax (elaborate-finish parsed env compiled)
  (define export-bindings
    (for/list ([(name sch) (in-hash (env-vars env))]
               #:unless (env-ref-var prelude-env name #f))
      (cons name (scheme->sexp sch))))
  ;; Phase 56: omit ctors whose owning type was declared with the
  ;; #:abstract flag — importers can still mention the TYPE in
  ;; signatures (it's exported via the tcons table) but they
  ;; can't construct or pattern-match.
  (define export-data-ctors
    (for/list ([(name di) (in-hash (env-data-ctors env))]
               #:unless
               (or (env-ref-data prelude-env name #f)
                   (let ([ti (env-ref-tcon env (data-info-type-name di))])
                     (and ti (tcon-info-abstract? ti)))))
      (cons name (encode-data-info di))))
  (define export-tcons
    (for/list ([(name ti) (in-hash (env-tcons env))]
               #:unless (env-ref-tcon prelude-env name #f))
      (cons name (encode-tcon-info ti))))
  (define export-classes
    (for/list ([(name ci) (in-hash (env-classes env))]
               #:unless (env-ref-class prelude-env name #f))
      (cons name (encode-class-info ci))))
  (define export-instances
    (apply append
           (for/list ([(class-name insts)
                       (in-hash (env-instance-table env))])
             (define prelude-insts
               (env-instances prelude-env class-name))
             (for/list ([inst (in-list insts)]
                        #:unless (member inst prelude-insts))
               (encode-instance-info class-name inst)))))
  (values compiled
          export-bindings export-data-ctors export-tcons
          export-classes export-instances))

;; `(rackton form ...)` — embeddable form.  Splices the compiled forms
;; but does NOT emit a sidecar schemes submodule, so multiple
;; `(rackton ...)` invocations can coexist in a single Racket module.
(define-syntax (rackton stx)
  (syntax-parse stx
    [(_ form ...)
     (define-values (compiled _b _d _t _c _i mono-log)
       (rackton-elaborate #'(form ...)))
     (with-syntax ([(out ...) compiled]
                   [entries mono-log])
       (syntax/loc stx
         (begin (set-rackton-monomorphized-log-snapshot! 'entries)
                out ...)))]))

;; `(rackton/main form ...)` — top-of-module form used by `#lang
;; rackton`.  Emits the schemes submodule so importing modules can
;; recover the types via dynamic-require.
(define-syntax (rackton/main stx)
  (syntax-parse stx
    [(_ form ...)
     (define-values (compiled bs dcs tcs cls insts _mono)
       (rackton-elaborate #'(form ...)))
     (define at-module-level?
       (memq (syntax-local-context) '(module module-begin)))
     (with-syntax ([(out ...)    compiled]
                   [bindings     (datum->syntax stx bs)]
                   [data-ctors   (datum->syntax stx dcs)]
                   [tcons        (datum->syntax stx tcs)]
                   [classes      (datum->syntax stx cls)]
                   [instances    (datum->syntax stx insts)])
       (cond
         [at-module-level?
          (syntax/loc stx
            (begin
              out ...
              (module+ rackton-schemes
                (provide rackton-bindings
                         rackton-data-ctors
                         rackton-tcons
                         rackton-classes
                         rackton-instances)
                (define rackton-bindings   'bindings)
                (define rackton-data-ctors 'data-ctors)
                (define rackton-tcons      'tcons)
                (define rackton-classes    'classes)
                (define rackton-instances  'instances))))]
         [else
          (syntax/loc stx (begin out ...))]))]))

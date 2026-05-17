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
                     "env.rkt"))

;; Shared elaboration helper: returns (values compiled-syntax-list
;; bindings-data data-ctors-data tcons-data classes-data instances-data).
(define-for-syntax (rackton-elaborate forms-stx)
  (define parsed
    (parse-toplevel-list (syntax->list forms-stx)))
  (define env (infer-program parsed prelude-env))
  (define compiled
    (filter values
            (for/list ([f (in-list parsed)])
              (compile-top f env))))
  (define export-bindings
    (for/list ([(name sch) (in-hash (env-vars env))]
               #:unless (env-ref-var prelude-env name #f))
      (cons name (scheme->sexp sch))))
  (define export-data-ctors
    (for/list ([(name di) (in-hash (env-data-ctors env))]
               #:unless (env-ref-data prelude-env name #f))
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
     (define-values (compiled _b _d _t _c _i)
       (rackton-elaborate #'(form ...)))
     (with-syntax ([(out ...) compiled])
       (syntax/loc stx (begin out ...)))]))

;; `(rackton/main form ...)` — top-of-module form used by `#lang
;; rackton`.  Emits the schemes submodule so importing modules can
;; recover the types via dynamic-require.
(define-syntax (rackton/main stx)
  (syntax-parse stx
    [(_ form ...)
     (define-values (compiled bs dcs tcs cls insts)
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

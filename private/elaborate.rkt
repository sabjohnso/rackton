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

(provide rackton)

(require (for-syntax racket/base
                     syntax/parse
                     racket/list
                     "surface.rkt"
                     "infer.rkt"
                     "codegen.rkt"))

(define-syntax (rackton stx)
  (syntax-parse stx
    [(_ form ...)
     (define parsed
       (parse-toplevel-list (syntax->list #'(form ...))))
     ;; Type-check.  Errors surface as exn:fail:syntax.  We also
     ;; capture the resulting env so codegen can resolve tcon-info
     ;; for instance dispatch tag generation.
     (define env (infer-program parsed))
     (define compiled
       (filter values
               (for/list ([f (in-list parsed)])
                 (compile-top f env))))
     (with-syntax ([(out ...) compiled])
       (syntax/loc stx (begin out ...)))]))

#lang racket/base

;; Rackton — the shape of a `require` spec: which sub-forms wrap a module
;; reference, and where inside the form that reference sits.
;;
;; One concern, one place.  Two clients need this knowledge and would
;; otherwise each encode it:
;;
;;   inference (infer.rkt) peels a spec datum down to the module path it
;;       names, so the importee's `rackton-schemes` sidecar can be loaded;
;;
;;   completion (complete-context.rkt) decides, from a cursor position in
;;       partially typed text, whether the point sits in a module-path
;;       position and should be completed with module paths.
;;
;; Adding a sub-form is then one entry in `wrapper-base-index` rather than
;; an edit in each client.  Note this module describes *position* only —
;; the name transform a sub-form imposes on the importee's exports is a
;; separate concern and stays in infer.rkt.
;;
;; Public API:
;;   require-wrapper-base-index : symbol -> (or/c exact-nonnegative-integer #f)
;;   require-spec-base-datum    : any -> (or/c string? symbol? #f)

(provide require-wrapper-base-index
         require-spec-base-datum)

;; The sub-forms that wrap exactly one module reference, mapped to that
;; reference's position in the form (0 is the sub-form's own name).
;;
;;   (only-in    MOD clause …)   (except-in MOD name …)
;;   (rename-in  MOD clause …)   -> position 1
;;   (prefix-in  pfx MOD)        (qualified-in pfx MOD)
;;                               -> position 2
;;
;; Deliberately absent: `combine-in` (names several modules, so no single
;; position is *the* reference), and `lib` / `file` / `submod` (module
;; references in their own right, not wrappers).
(define wrapper-base-index
  (hasheq 'only-in      1
          'except-in    1
          'rename-in    1
          'prefix-in    2
          'qualified-in 2))

;; Where `form` keeps the module reference it wraps, or #f when `form` is
;; not a wrapper we handle.
(define (require-wrapper-base-index form)
  (and (symbol? form) (hash-ref wrapper-base-index form #f)))

;; Peel wrapper sub-forms off `d` down to the module reference they wrap:
;; a relative-path string or a collection-path symbol.  An unhandled shape
;; — or a wrapper truncated mid-edit, whose reference position is not
;; present — yields #f, which every client treats as "skip this spec".
(define (require-spec-base-datum d)
  (cond
    [(string? d) d]
    [(symbol? d) d]
    [(pair? d)
     (define i (require-wrapper-base-index (car d)))
     (define base (and i (list-ref-or-false d i)))
     (and base (require-spec-base-datum base))]
    [else #f]))

;; `(list-ref d i)` for a proper list long enough to have an ith element,
;; else #f — the spec may be an improper or truncated form mid-edit.
(define (list-ref-or-false d i)
  (let loop ([d d] [i i])
    (cond
      [(not (pair? d)) #f]
      [(zero? i) (car d)]
      [else (loop (cdr d) (sub1 i))])))

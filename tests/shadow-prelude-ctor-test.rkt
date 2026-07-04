#lang racket/base

;; A module may locally redefine a prelude constructor name (here `Cons`,
;; as a constructor of `Nonempty-List`).  Its type sidecar must publish
;; the local `Cons`; otherwise an importer's type checker falls back to
;; the prelude's `Cons : a -> List a -> List a`, and `(Cons x (Sole y))`
;; mis-types.  This is the REPL scenario: `,require` a library that
;; shadows a prelude constructor, then use that constructor — the REPL's
;; top-level namespace tolerates the runtime name clash (both `Cons`es
;; are the same prefab struct), so only the TYPE must be recovered.

(require rackunit
         racket/runtime-path
         "../private/repl.rkt")

;; The REPL resolves a `require`'s relative module-path string against the
;; current directory (a Racket module-path string may not be absolute), so
;; anchor the session on this test's own directory and name the library by
;; its basename — independent of where `raco test` runs.
(define-runtime-path here ".")
(define lib "shadow-prelude-ctor-lib.rkt")

(define (drive-session inputs)
  (parameterize ([current-directory here])
    (for/fold ([state (rackton-repl-init)] [out '()] #:result (reverse out))
              ([form (in-list inputs)])
      (define-values (state* o) (rackton-repl-step state form))
      (values state* (cons o out)))))

(test-case "REPL: locally-shadowed prelude constructor keeps the library's type"
  ;; After requiring the library, `(Cons 1 (Sole 2))` must type as
  ;; `Nonempty-List Integer`.  With the prelude `Cons` it would type as
  ;; `List Integer`, so `,type` distinguishes fixed from broken.
  (define outs
    (drive-session
     (list (list 'require lib)
           (list 'unquote 'type '(Cons 1 (Sole 2))))))
  (define type-line (car (reverse outs)))
  (check-regexp-match #rx"Nonempty-List" type-line))

(test-case "REPL: shadowed constructor is usable through a library function"
  ;; `ne-length` accepts only `Nonempty-List`; passing a `Cons`-built
  ;; value type-checks and runs only when `Cons` is the imported one.
  (define outs
    (drive-session
     (list (list 'require lib)
           '(ne-length (Cons 1 (Cons 2 (Sole 3)))))))
  (check-regexp-match #rx"3" (car (reverse outs))))

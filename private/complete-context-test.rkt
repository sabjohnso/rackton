#lang racket/base

;; Tests for private/complete-context.rkt — what the point is positioned
;; to complete.
;;
;; The `|` marker is repl-entry.rkt's example notation for the point.
;; Each case states the text a user has typed so far and the category the
;; completion clients must switch on: an identifier, a collection path, or
;; a relative file path.  The category is decided structurally, from the
;; enclosing forms, so it must hold mid-edit — the text is never balanced
;; while the user is still typing.

(module+ test
  (require rackunit
           racket/string
           "repl-entry.rkt"
           "complete-context.rkt")

  ;; The category at the `|` in `marked`.
  (define (kind-at marked)
    (define en (marked->entry marked))
    (define-values (kind _start)
      (completion-context (entry-text en) (entry-point en)))
    kind)

  ;; The text the completion would replace: [start, point).
  (define (prefix-at marked)
    (define en (marked->entry marked))
    (define-values (_kind start)
      (completion-context (entry-text en) (entry-point en)))
    (substring (entry-text en) start (entry-point en)))

  ;; ----- identifiers: the default everywhere else -----------------------

  (test-case "a bare name completes as an identifier"
    (check-equal? (kind-at "ma|") 'identifier)
    (check-equal? (prefix-at "ma|") "ma"))

  (test-case "a name inside an ordinary form completes as an identifier"
    (check-equal? (kind-at "(define (f x) (ma|))") 'identifier)
    (check-equal? (prefix-at "(define (f x) (ma|))") "ma"))

  (test-case "the head of a require form is still an identifier"
    (check-equal? (kind-at "(requ|)") 'identifier))

  ;; ----- collection paths ----------------------------------------------

  (test-case "an argument of require is a module path"
    (check-equal? (kind-at "(require rackton/da|)") 'module-path)
    (check-equal? (prefix-at "(require rackton/da|)") "rackton/da"))

  (test-case "an empty argument position is a module path with no prefix"
    (check-equal? (kind-at "(require |)") 'module-path)
    (check-equal? (prefix-at "(require |)") ""))

  (test-case "every argument of require is a module path, not just the first"
    (check-equal? (kind-at "(require rackton/data/list rackton/te|)")
                  'module-path))

  (test-case "a selection sub-form wraps its module reference at position 1"
    (check-equal? (kind-at "(require (only-in rackton/da|))") 'module-path)
    (check-equal? (kind-at "(require (except-in rackton/da|))") 'module-path)
    (check-equal? (kind-at "(require (rename-in rackton/da|))") 'module-path))

  (test-case "a prefixing sub-form wraps its module reference at position 2"
    (check-equal? (kind-at "(require (prefix-in l: rackton/da|))") 'module-path)
    (check-equal? (kind-at "(require (qualified-in l rackton/da|))") 'module-path))

  (test-case "sub-forms nest"
    (check-equal? (kind-at "(require (prefix-in p: (only-in rackton/da|)))")
                  'module-path))

  (test-case "the imported names of a sub-form are identifiers, not paths"
    (check-equal? (kind-at "(require (only-in rackton/data/list ma|))")
                  'identifier)
    (check-equal? (kind-at "(require (prefix-in l|: rackton/data/list))")
                  'identifier))

  (test-case "an unhandled sub-form offers no module path"
    ;; combine-in names several modules; no position is *the* reference.
    (check-equal? (kind-at "(require (combine-in rackton/da|))") 'identifier))

  (test-case "a sub-form outside a require is not a module-path context"
    (check-equal? (kind-at "(only-in rackton/da|)") 'identifier))

  (test-case "a nested form inside a require argument is not a module path"
    (check-equal? (kind-at "(require (only-in rackton/data/list [ma|]))")
                  'identifier))

  ;; ----- relative paths -------------------------------------------------

  (test-case "a string in a module-path position completes as a file path"
    (check-equal? (kind-at "(require \"hel|\")") 'relative-path)
    (check-equal? (prefix-at "(require \"hel|\")") "hel"))

  (test-case "an unterminated string still completes as a file path"
    (check-equal? (kind-at "(require \"hel|") 'relative-path)
    (check-equal? (prefix-at "(require \"hel|") "hel"))

  (test-case "a string in a sub-form's module position completes as a file path"
    (check-equal? (kind-at "(require (only-in \"hel|\"))") 'relative-path))

  (test-case "a string elsewhere completes nothing special"
    (check-equal? (kind-at "(define s \"hel|\")") 'identifier)
    (check-equal? (kind-at "(require (only-in \"m.rkt\" \"hel|\"))") 'identifier))

  ;; ----- mid-edit robustness --------------------------------------------

  (test-case "the category holds while the text is still unbalanced"
    (check-equal? (kind-at "(require (only-in rackton/da|") 'module-path)
    (check-equal? (kind-at "(require rackton/da|") 'module-path))

  (test-case "a require spanning lines keeps its category"
    (check-equal? (kind-at "(require rackton/data/list\n         rackton/te|)")
                  'module-path))

  (test-case "a comment inside the form does not disturb the category"
    (check-equal? (kind-at "(require ; a note\n rackton/te|)") 'module-path)))

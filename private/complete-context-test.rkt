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
           rackcheck
           racket/list
           racket/string
           "repl-entry.rkt"
           (only-in "require-spec-shape.rkt" require-spec-base-datum)
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
    (check-equal? (kind-at "(require ; a note\n rackton/te|)") 'module-path))

  ;; ----- properties ------------------------------------------------------

  ;; Text over the characters that carry structure, so a generated case
  ;; is as likely to be unbalanced, half-quoted, or comment-ridden as
  ;; anything a user passes through while typing.
  (define gen:text
    (gen:let ([cs (gen:list (gen:one-of (string->list "()[]\"; ab/#\\,'\n"))
                            #:max-length 40)])
      (list->string cs)))

  (define gen:text+pos
    (gen:let ([text gen:text]
              [n (gen:integer-in 0 40)])
      (cons text (min n (string-length text)))))

  (check-property
   (property the-region-lies-behind-the-point ([tp gen:text+pos])
     ;; The clients slice [start, point) and the editor replaces it, so a
     ;; start outside that range is a crash, not a bad suggestion.  This
     ;; must hold for text no reader would accept.
     (define-values (kind start) (completion-context (car tp) (cdr tp)))
     (and (memq kind '(identifier module-path relative-path))
          (<= 0 start (cdr tp)))))

  (check-property
   (property the-text-after-the-point-is-irrelevant ([tp gen:text+pos])
     ;; What justifies asking mid-entry at all: the classification of a
     ;; position cannot depend on text not yet typed.
     (define-values (k1 s1) (completion-context (car tp) (cdr tp)))
     (define-values (k2 s2)
       (completion-context (substring (car tp) 0 (cdr tp)) (cdr tp)))
     (and (eq? k1 k2) (= s1 s2))))

  ;; Require specs built from the wrapper grammar, to any nesting depth.
  (define (gen:spec depth)
    (if (zero? depth)
        (gen:one-of '(rackton/data/list "helpers.rkt"))
        (gen:let ([inner (gen:spec (sub1 depth))]
                  [shape (gen:one-of '(only-in except-in rename-in
                                       prefix-in qualified-in))])
          (case shape
            [(only-in except-in) (list shape inner 'foo)]
            [(rename-in) (list shape inner '(foo bar))]
            [else (list shape 'p: inner)]))))

  (check-property
   (property the-classification-agrees-with-the-spec-grammar
             ([spec (gen:let ([d (gen:integer-in 0 3)]) (gen:spec d))])
     ;; The point of the shared table: wherever inference peels a spec
     ;; down to its module reference, completion classifies that same
     ;; position as a module reference.  Adding a sub-form to the table
     ;; must keep the two in step.
     (define base (require-spec-base-datum spec))
     (define text (format "(require ~s)" spec))
     (define shown (format "~s" base))
     (define end (cdar (regexp-match-positions (regexp (regexp-quote shown)) text)))
     ;; For a string base the reference is the text inside the quotes, so
     ;; the point goes before the closing one.
     (define pos (if (string? base) (sub1 end) end))
     (define-values (kind _start) (completion-context text pos))
     (eq? kind (if (string? base) 'relative-path 'module-path))))

  (check-property
   (property the-region-is-a-run-of-name-characters ([tp gen:text+pos])
     ;; What `completion-word-start` means, stated where clients can see
     ;; it: the region is exactly the name characters behind the point.
     (define text (car tp))
     (define pos (cdr tp))
     (define start (completion-word-start text pos))
     (and (for/and ([i (in-range start pos)])
            (completion-word-char? (string-ref text i)))
          (or (zero? start)
              (not (completion-word-char? (string-ref text (sub1 start)))))))))

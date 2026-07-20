#lang racket/base

;; Tests for private/require-spec-shape.rkt — the single description of
;; where a `require` sub-form keeps the module reference it wraps.
;;
;; The table is consumed by two very different clients: inference, which
;; peels a spec datum down to its module path, and completion, which
;; decides from a cursor position whether the point sits in a module-path
;; position.  Both are pinned here so neither can drift from the other.

(module+ test
  (require rackunit
           "require-spec-shape.rkt")

  (test-case "wrapper base index: selection sub-forms wrap at position 1"
    (for ([form '(only-in except-in rename-in)])
      (check-equal? (require-wrapper-base-index form) 1
                    (format "~a wraps its module reference at position 1" form))))

  (test-case "wrapper base index: prefixing sub-forms wrap at position 2"
    (for ([form '(prefix-in qualified-in)])
      (check-equal? (require-wrapper-base-index form) 2
                    (format "~a wraps its module reference at position 2" form))))

  (test-case "wrapper base index: an unhandled form has no known position"
    ;; combine-in names several modules, so no single position is *the*
    ;; module reference; lib/file/submod are not peeled either.
    (for ([form '(combine-in lib file submod not-a-require-form)])
      (check-false (require-wrapper-base-index form))))

  (test-case "require-wrapper-form? is the table's membership test"
    (check-true (require-wrapper-form? 'only-in))
    (check-true (require-wrapper-form? 'prefix-in))
    (check-false (require-wrapper-form? 'combine-in)))

  (test-case "base datum: a bare module reference is itself"
    (check-equal? (require-spec-base-datum 'rackton/data/list) 'rackton/data/list)
    (check-equal? (require-spec-base-datum "helpers.rkt") "helpers.rkt"))

  (test-case "base datum: a wrapper peels to its inner reference"
    (check-equal? (require-spec-base-datum '(only-in rackton/data/list map))
                  'rackton/data/list)
    (check-equal? (require-spec-base-datum '(except-in rackton/data/list map))
                  'rackton/data/list)
    (check-equal? (require-spec-base-datum '(rename-in rackton/data/list [map m]))
                  'rackton/data/list)
    (check-equal? (require-spec-base-datum '(prefix-in l: rackton/data/list))
                  'rackton/data/list)
    (check-equal? (require-spec-base-datum '(qualified-in l rackton/data/list))
                  'rackton/data/list))

  (test-case "base datum: wrappers nest, innermost reference wins"
    (check-equal? (require-spec-base-datum
                   '(prefix-in p: (only-in "helpers.rkt" foo)))
                  "helpers.rkt"))

  (test-case "base datum: an unhandled shape yields #f"
    (check-false (require-spec-base-datum '(combine-in a b)))
    (check-false (require-spec-base-datum '(lib "racket/list")))
    (check-false (require-spec-base-datum 42)))

  (test-case "base datum: a truncated wrapper yields #f, it does not raise"
    ;; Mid-edit text reaches the completion client as an incomplete form.
    (check-false (require-spec-base-datum '(only-in)))
    (check-false (require-spec-base-datum '(prefix-in p:)))))

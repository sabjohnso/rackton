#lang racket/base

;; The Functor Widget instance must dispatch correctly even though Widget
;; is opaque (no Rackton ctors) — its instance registers under the
;; declared :runtime-tag, matching the runtime struct's dispatch-tag.

(require rackunit
         "../main.rkt")

(rackton
  (require "opaque-tag-lib.rkt")

  (: w (Widget Integer))
  (define w (make-widget 5))

  (: w2 (Widget Integer))
  (define w2 (fmap (lambda (x) (+ x 100)) w))

  (: result Integer)
  (define result (widget-get w2)))

(test-case "fmap dispatches on an opaque type via :runtime-tag"
  (check-equal? result 105))

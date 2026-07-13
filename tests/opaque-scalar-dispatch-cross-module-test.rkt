#lang rackton

;; Regression: a type-class instance headed on a value-predicate-
;; dispatched, constructor-less prelude scalar must dispatch correctly
;; when the instance lives in an imported module.  Before the fix,
;; tags-for-instance-head special-cased only Integer / Float / Boolean /
;; String; instances headed on Rational / Complex / ComplexExact / Char /
;; Bytes / Symbol registered under no tag and raised `no instance for:
;; <value>` at runtime across a module boundary.

(require "opaque-scalar-dispatch-cross-module-lib.rkt"
         "../unit.rkt")

(: t-rat String)          (define t-rat          (scalar-tag (make-rational 1 2)))
(: t-complex String)      (define t-complex      (scalar-tag (make-complex 1.0 2.0)))
(: t-complex-exact String)(define t-complex-exact (scalar-tag (make-complex-exact 1 2)))
(: t-char String)         (define t-char         (scalar-tag #\A))
(: t-bytes String)        (define t-bytes        (scalar-tag #"ab"))
(: t-symbol String)       (define t-symbol       (scalar-tag 'foo))

(: suite (List Test))
(define suite
  (list
    (it "predicate-dispatched opaque scalars dispatch across a module boundary"
        (all-checks
          (list (check-equal? t-rat           "rational")
                (check-equal? t-complex        "complex")
                (check-equal? t-complex-exact  "complex-exact")
                (check-equal? t-char           "char")
                (check-equal? t-bytes          "bytes")
                (check-equal? t-symbol         "symbol"))))))

(: test-main (IO Unit))
(define test-main (run-suite "opaque scalar dispatch cross-module" suite))

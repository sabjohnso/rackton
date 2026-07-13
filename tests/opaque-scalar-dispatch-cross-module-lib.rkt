#lang rackton

;; Companion library for opaque-scalar-dispatch-cross-module-test.
;; Defines a class with instances headed on every value-predicate-
;; dispatched, constructor-less prelude scalar (see dict.rkt's
;; dispatch-tag): the numeric-tower opaque types Rational / Complex /
;; ComplexExact, plus Char / Bytes / Symbol.  These have no constructors,
;; so before the tags-for-instance-head fix they registered under no tag
;; and missed at runtime when called from an importing module.

(provide (protocol-out ScalarTag))

(protocol (ScalarTag a)
  (: scalar-tag (-> a String)))

(instance (ScalarTag Rational)     (define (scalar-tag x) "rational"))
(instance (ScalarTag Complex)      (define (scalar-tag x) "complex"))
(instance (ScalarTag ComplexExact) (define (scalar-tag x) "complex-exact"))
(instance (ScalarTag Char)         (define (scalar-tag x) "char"))
(instance (ScalarTag Bytes)        (define (scalar-tag x) "bytes"))
(instance (ScalarTag Symbol)       (define (scalar-tag x) "symbol"))

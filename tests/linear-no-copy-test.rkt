#lang racket/base

;; The defining property of the linear arrow: a `Lin` wire CANNOT be copied
;; or discarded — there is no `Copyable Lin` / `Discardable Lin` instance, so
;; `dup` / `discard` at type `Lin` is a TYPE ERROR.  The cartesian `Fn` has
;; both, as a positive control.  (Compile-error-at-expansion checks need
;; #lang racket/base, hence this separate file.)

(require rackunit
         (for-syntax racket/base)
         "../main.rkt")

(define-syntax-rule (check-rackton-compile-error form ...)
  (check-exn
   exn:fail?
   (lambda ()
     (eval #'(rackton form ...)
           (variable-reference->namespace (#%variable-reference))))))

(test-case "a linear arrow cannot be copied — no (Copyable Lin)"
  (check-rackton-compile-error
   (require "../linear.rkt")
   (: bad (Lin Integer (Ten Lin Integer Integer)))
   (define bad dup)))

(test-case "a linear arrow cannot be discarded — no (Discardable Lin)"
  (check-rackton-compile-error
   (require "../linear.rkt")
   (: bad (Lin Integer Unit))
   (define bad discard)))

;; positive control: the cartesian Fn CAN copy and discard.
(rackton
  (require "../linear.rkt")
  (: good-dup (Fn Integer (Ten Fn Integer Integer)))
  (define good-dup dup)
  (: good-discard (Fn Integer Unit))
  (define good-discard discard))

(test-case "a cartesian arrow CAN copy and discard — (Copyable Fn)/(Discardable Fn)"
  (check-true #t))

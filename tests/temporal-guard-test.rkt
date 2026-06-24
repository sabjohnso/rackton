#lang racket/base

;; The guarded-stream TYPE enforces productivity partially, for free: a
;; `Signal`'s tail is a `Later`, so an UNGUARDED tail — a FORCED value
;; (`adv self`) where a `Later` is required — is a TYPE ERROR.  The modality
;; rejects the most common non-productive mistake.  (Full productivity — no
;; unguarded use anywhere — needs the graded core; this is the partial,
;; library-level enforcement.)

(require rackunit
         (for-syntax racket/base)
         "../main.rkt")

(define-syntax-rule (check-rackton-compile-error form ...)
  (check-exn
   exn:fail?
   (lambda ()
     (eval #'(rackton form ...)
           (variable-reference->namespace (#%variable-reference))))))

(test-case "an unguarded stream tail is a type error (tail must be Later)"
  (check-rackton-compile-error
   (require "../temporal.rkt")
   ;; (adv self) is a forced Signal, but SigCons's tail demands a Later
   (: bad (Signal Integer))
   (define bad (lob (lambda (self) (SigCons 0 (adv self)))))))

;; positive control: a GUARDED tail (self is left as the Later) compiles.
(rackton
  (require "../temporal.rkt")
  (: good (Signal Integer))
  (define good (lob (lambda (self) (SigCons 0 self)))))

(test-case "a guarded tail compiles"
  (check-true #t))

#lang racket/base

;; Deriving Prism on a constructor with two or more fields is rejected
;; at compile time (rather than silently skipping that constructor).
;; A multi-field constructor would need a product-focused prism, which
;; is not implemented — see ISSUES.org.  Compile-error tests must live
;; in #lang racket/base (they eval at expansion), so this file is not
;; part of the #lang rackton sweep.

(require rackunit
         "../main.rkt")

(define-syntax-rule (check-rackton-compile-error form ...)
  (check-exn
   exn:fail?
   (lambda ()
     (eval #'(rackton form ...)
           (variable-reference->namespace (#%variable-reference))))))

(test-case "deriving Prism on a 2-field ctor is rejected"
  (check-rackton-compile-error
   (require rackton/data/lens)
   (data PT (PA Integer) (PB Integer Integer) #:deriving Prism)))

(test-case "deriving Prism on a 3-field ctor is rejected"
  (check-rackton-compile-error
   (require rackton/data/lens)
   (data QT (QA Integer) (QB Integer Integer Integer) #:deriving Prism)))

;; Positive sanity: a type whose ctors are all nullary or single-field
;; still derives prisms fine (compiling this block is the assertion).
(rackton
 (require rackton/data/lens)
 (data Okp PNone (PJust Integer) #:deriving Prism)
 (: probe (Maybe Integer))
 (define probe (preview Okp-PJust-prism (PJust 7))))

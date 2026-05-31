#lang racket/base

;; A multi-field prism focuses a flat tuple, defined up to 7 fields
;; (Pair + Tuple3..Tuple7).  Deriving Prism on a constructor with more
;; fields than that is a compile error.  Compile-error tests must live
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

(test-case "deriving Prism on an 8-field ctor is rejected (tuples stop at 7)"
  (check-rackton-compile-error
   (require rackton/data/lens)
   (data Huge
     (H8 Integer Integer Integer Integer Integer Integer Integer Integer)
     #:deriving Prism)))

;; Positive sanity: a 7-field ctor is at the limit and still derives
;; (compiling this block is the assertion).
(rackton
 (require rackton/data/lens)
 (data Lim (L7 Integer Integer Integer Integer Integer Integer Integer)
   #:deriving Prism)
 (: probe (Maybe (Tuple7 Integer Integer Integer Integer Integer Integer Integer)))
 (define probe (preview Lim-L7-prism (L7 1 2 3 4 5 6 7))))

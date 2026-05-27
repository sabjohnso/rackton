#lang racket/base

;; Diagnostics: structured expected/got errors, per-arg blame
;; in applications, did-you-mean? extended to classes and constructors.

(require rackunit
         (for-syntax racket/base)
         "../main.rkt")

(define-syntax-rule (rackton-error form ...)
  (with-handlers ([exn:fail? exn-message])
    (eval #'(rackton form ...)
          (variable-reference->namespace (#%variable-reference)))))

(test-case "if condition mismatch reports expected/got"
  (define msg
    (rackton-error
     (define x (if 1 1 2))))
  (check-regexp-match #rx"expected: Boolean" msg)
  (check-regexp-match #rx"got: +Integer"     msg))

(test-case "if branches disagree shows both"
  (define msg
    (rackton-error
     (define x (if #t 1 "two"))))
  (check-regexp-match #rx"expected: Integer" msg)
  (check-regexp-match #rx"got: +String"      msg))

(test-case "ann mismatch reports expected/got"
  (define msg
    (rackton-error
     (define x (ann "hi" Integer))))
  (check-regexp-match #rx"expected: Integer" msg)
  (check-regexp-match #rx"got: +String"      msg))

(test-case "application argument-position blame"
  (define msg
    (rackton-error
     (define x (+ 1 "two"))))
  ;; The error message should call out the argument type, not just
  ;; the whole application form.
  (check-regexp-match #rx"expected: +Integer" msg)
  (check-regexp-match #rx"got: +String"       msg))

(test-case "unknown class suggests a near match"
  (define msg
    (rackton-error
     (instance (Eqq Integer)
       (define (eq x y) (= x y)))))
  (check-regexp-match #rx"unknown class: Eqq" msg)
  (check-regexp-match #rx"did you mean `Eq`"  msg))

(test-case "unknown constructor in a pattern suggests"
  (define msg
    (rackton-error
     (define x (match Nil [(Nul) 0] [_ 1]))))
  (check-regexp-match #rx"unknown data constructor: Nul" msg)
  (check-regexp-match #rx"did you mean `Nil`"            msg))

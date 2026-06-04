#lang racket/base

;; Better error messages for the monadic / parallel let forms.
;;
;; let&, let%, and let+ all bind with the shape `([var expr] ...+)`.
;; When a user writes a malformed binding clause — e.g. an extra token
;; between the variable and its expression, as in `[_ <> (pure Unit)]`
;; — the form used to fall through to a function-application reading and
;; report the useless "unbound identifier: let&".  These tests pin the
;; replacement: an error that names the form and points at the offending
;; binding clause.

(require rackunit
         (for-syntax racket/base)
         "../main.rkt")

(define-syntax-rule (catch-rackton-error form ...)
  ;; Returns the error message string for an ill-formed rackton form,
  ;; or fails the test if no error was raised.
  (with-handlers ([exn:fail? exn-message])
    (eval #'(rackton form ...)
          (variable-reference->namespace (#%variable-reference)))
    (error 'catch-rackton-error "expected an error, none raised")))

;; ----- the reported case: a stray `<>` in a let& binding -----------

(test-case "let& with a stray token in a binding names the form, not 'unbound'"
  (define msg
    (catch-rackton-error
     (define _ (run-io (let& ([_ <> (pure Unit)]) (pure Unit))))))
  ;; The old, unhelpful message said "unbound identifier: let&".
  (check-false (regexp-match? #rx"unbound identifier" msg))
  (check-regexp-match #rx"let&" msg)
  (check-regexp-match #rx"binding" msg))

(test-case "let& stray-token error pinpoints the offending token"
  (define msg
    (catch-rackton-error
     (define _ (run-io (let& ([_ <> (pure Unit)]) (pure Unit))))))
  ;; The `<>` between the variable and the expression is the problem.
  (check-regexp-match #rx"<>" msg)
  ;; And the message explains the correct shape.
  (check-regexp-match #rx"var expr" msg))

;; ----- the same diagnosis for let% and let+ ------------------------

(test-case "let% with a stray token names the form, not 'unbound'"
  (define msg
    (catch-rackton-error
     (define x (let% ([a <> (Some 1)]) (Some a)))))
  (check-false (regexp-match? #rx"unbound identifier" msg))
  (check-regexp-match #rx"let%" msg))

(test-case "let+ with a stray token names the form, not 'unbound'"
  (define msg
    (catch-rackton-error
     (define x (let+ ([a <> (Some 1)]) a))))
  (check-false (regexp-match? #rx"unbound identifier" msg))
  (check-regexp-match #rx"let\\+" msg))

;; ----- a missing right-hand side is also diagnosed -----------------

(test-case "let& with a single-element binding clause is diagnosed"
  (define msg
    (catch-rackton-error
     (define _ (run-io (let& ([a]) (pure Unit))))))
  (check-false (regexp-match? #rx"unbound identifier" msg))
  (check-regexp-match #rx"let&" msg))

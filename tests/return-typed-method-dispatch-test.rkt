#lang racket/base

;; Return-typed dispatch for `pure`.

(require rackunit
         "../main.rkt")

(rackton
  ;; ----- pure for Maybe ------------------------------------
  (: pure-maybe-int (Maybe Integer))
  (define pure-maybe-int (pure 5))

  ;; ----- pure for Result ------------------------------------
  (: pure-result-int (Result String Integer))
  (define pure-result-int (pure 42))

  ;; ----- pure for List --------------------------------------
  (: pure-list-int (List Integer))
  (define pure-list-int (pure 7))

  ;; ----- pure for IO ----------------------------------------
  (: pure-io-int (IO Integer))
  (define pure-io-int (pure 99))

  ;; ----- pure inside a do-block where context fixes f ------
  (: do-with-pure (IO Integer))
  (define do-with-pure
    (do [_ <- (pure 1)]
        (pure 2)))

  ;; ----- pure with explicit ascription -------------------
  (: ascribed (Maybe String))
  (define ascribed (ann (pure "ok") (Maybe String))))

;; ----- ambiguous pure ----------------------------------------
;; `(pure 5)` without enough context to pin down `f` should fail
;; cleanly at compile time, not produce a misleading runtime error.
(require (for-syntax racket/base))
(define-syntax-rule (check-rackton-compile-error form ...)
  (check-exn
   exn:fail?
   (lambda ()
     (eval #'(rackton form ...)
           (variable-reference->namespace (#%variable-reference))))))

(test-case "ambiguous pure with no type context is rejected"
  (check-rackton-compile-error
   (define x (pure 5))))

;; ---------- assertions ----------------------------------------

(test-case "pure resolves to Maybe.pure"
  (check-equal? pure-maybe-int (Some 5)))

(test-case "pure resolves to Result.pure (Ok)"
  (check-equal? pure-result-int (Ok 42)))

(test-case "pure resolves to List.pure (singleton)"
  (check-equal? pure-list-int (Cons 7 Nil)))

(test-case "pure resolves to IO.pure"
  (check-equal? (run-io pure-io-int) 99))

(test-case "pure inside do-block uses outer monad"
  (check-equal? (run-io do-with-pure) 2))

(test-case "pure with explicit ann"
  (check-equal? ascribed (Some "ok")))

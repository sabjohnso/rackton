#lang racket/base

;; Better error messages.
;;
;; Two targeted improvements:
;;   1. Constructor arity errors include the struct's field names,
;;      so a mis-arity on a `struct` ctor tells the user
;;      which fields are missing.
;;   2. "No instance" errors suggest `#:deriving X` when X is one
;;      of the derivable classes (Eq, Ord, Show, Functor, etc.).

(require rackunit
         (for-syntax racket/base)
         "../main.rkt")

(define-syntax-rule (catch-rackton-error form ...)
  ;; Returns the error message string for an ill-typed rackton form,
  ;; or fails the test if no error was raised.
  (with-handlers ([exn:fail? exn-message])
    (eval #'(rackton form ...)
          (variable-reference->namespace (#%variable-reference)))
    (error 'catch-rackton-error "expected an error, none raised")))

;; ----- 59.1 ctor arity message lists field names ----------------

(test-case "ctor arity mismatch in pattern lists struct field names"
  (define msg
    (catch-rackton-error
     (struct Point [x : Integer] [y : Integer])
     (define (peek p) (match p [(Point a) a]))))
  (check-regexp-match #rx"fields: x, y" msg))

(test-case "ctor arity mismatch in pattern lists struct field names (3 fields)"
  (define msg
    (catch-rackton-error
     (struct Tri [a : Integer] [b : Integer] [c : Integer])
     (define (peek t) (match t [(Tri x y) x]))))
  (check-regexp-match #rx"fields: a, b, c" msg))

;; ----- 59.2 no-instance suggests #:deriving ---------------------

(test-case "missing Eq instance suggests #:deriving Eq"
  (define msg
    (catch-rackton-error
     (data Box (MkBox Integer))
     (define same (== (MkBox 1) (MkBox 2)))))
  (check-regexp-match #rx"#:deriving Eq" msg))

(test-case "missing Show instance suggests #:deriving Show"
  (define msg
    (catch-rackton-error
     (data Box (MkBox Integer))
     (define shown (show (MkBox 1)))))
  (check-regexp-match #rx"#:deriving Show" msg))

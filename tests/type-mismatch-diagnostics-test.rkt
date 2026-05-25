#lang racket/base

;; Better error messages.  Each test compiles a Rackton
;; snippet that should error, and asserts the error message contains
;; a useful phrase.

(require rackunit
         racket/string
         (only-in "../main.rkt" rackton))

;; Compile-and-expect-error helper.  Returns the error message string
;; or fails the test if the snippet compiles successfully.
(define-syntax-rule (compile-error form ...)
  (with-handlers ([exn:fail? exn-message])
    (eval #'(rackton form ...)
          (variable-reference->namespace (#%variable-reference)))
    (fail "expected a compile error but the snippet compiled cleanly")))

(test-case "no-instance error lists available instances for the class"
  (define msg
    (compile-error
     (define-class (Showable a)
       (: shw (-> a String)))
     (define-instance (Showable Integer)
       (define (shw n) ""))
     (define-instance (Showable Boolean)
       (define (shw b) ""))
     (: bad String)
     (define bad (shw "hi"))))
  ;; Should mention the class, the missing instance, AND list the
  ;; instances we do have.
  (check-true (regexp-match? #rx"no instance" msg) msg)
  (check-true (regexp-match? #rx"Showable" msg) msg)
  (check-true (regexp-match? #rx"Integer|Boolean" msg) msg))

(test-case "type-mismatch points to the bad argument and shows expected/got"
  (define msg
    (compile-error
     (define (incr n) (+ n 1))
     (: bad Integer)
     (define bad (incr "hello"))))
  (check-true (regexp-match? #rx"type mismatch" msg) msg)
  (check-true (regexp-match? #rx"expected" msg) msg)
  (check-true (regexp-match? #rx"got" msg) msg)
  (check-true (regexp-match? #rx"Integer" msg) msg)
  (check-true (regexp-match? #rx"String" msg) msg))

(test-case "did-you-mean for a misspelled class method"
  (define msg
    (compile-error
     (: oops (Maybe Integer))
     (define oops (pyre 1))))
  (check-true (regexp-match? #rx"unbound" msg) msg)
  ;; "did you mean `pure`?" — the search includes class methods.
  (check-true (regexp-match? #rx"pure" msg) msg))

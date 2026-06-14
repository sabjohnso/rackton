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
     (protocol (Showable a)
       (: shw (-> a String)))
     (instance (Showable Integer)
       (define (shw n) ""))
     (instance (Showable Boolean)
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

(test-case "first-arm result mismatch blames the declared result type, not earlier arms"
  ;; The mismatch is on the very first arm — there are no earlier arms,
  ;; so the message must point at the declared/expected result type.
  (define msg
    (compile-error
     (data (Expr a)
       (Lit  : (-> a (Expr a)))
       (Zero : (Expr a)))
     (: eval-boolean (-> (Expr Boolean) (Expr Boolean)))
     (define (eval-boolean e)
       (match e
         [(Lit a) a]
         [Zero #f]))))
  (check-false (regexp-match? #rx"earlier arms" msg) msg)
  (check-true  (regexp-match? #rx"expected result type" msg) msg)
  (check-true  (regexp-match? #rx"Expr Boolean" msg) msg))

(test-case "later-arm result mismatch still blames earlier arms"
  ;; No declared signature: the first arm fixes the result type, so a
  ;; later mismatch genuinely conflicts with an earlier arm.
  (define msg
    (compile-error
     (define (g b)
       (match b
         [#t 1]
         [#f "two"]))))
  (check-true (regexp-match? #rx"earlier arms" msg) msg))

(test-case "first-clause result mismatch in a multi-clause define blames the signature"
  (define msg
    (compile-error
     (: f (-> Integer String))
     (define (f 0) 0)
     (define (f n) "n")))
  (check-false (regexp-match? #rx"earlier clauses" msg) msg)
  (check-true  (regexp-match? #rx"expected result type" msg) msg))

(test-case "a missing-constraint instance error names the variable, not a skolem"
  ;; The Category instance for (Kleisli m) needs (Monad m); the error
  ;; must read `(Monad m)`, not `(Monad $gen-skolem.m.NN)`.
  (define msg
    (compile-error
     (data (Kleisli m a b) (Kleisli (-> a (m b))))
     (instance (Category (Kleisli m))
       (define ident (Kleisli pure))
       (define (comp (Kleisli f) (Kleisli g))
         (Kleisli (lambda (x) (flatmap f (g x))))))))
  (check-true  (regexp-match? #rx"no instance for \\(Monad m\\)" msg) msg)
  (check-false (regexp-match? #rx"skolem" msg) msg))

(test-case "a missing-constraint instance error carries a source location"
  ;; The error is a syntax error blaming the instance form, not a bare
  ;; exn:fail with no location.
  (define blamed
    (with-handlers ([exn:fail:syntax?
                     (lambda (e)
                       (and (pair? (exn:fail:syntax-exprs e))
                            (car (syntax->datum (car (exn:fail:syntax-exprs e))))))])
      (eval #'(rackton
               (data (Kleisli m a b) (Kleisli (-> a (m b))))
               (instance (Category (Kleisli m))
                 (define ident (Kleisli pure))
                 (define (comp (Kleisli f) (Kleisli g))
                   (Kleisli (lambda (x) (flatmap f (g x)))))))
            (variable-reference->namespace (#%variable-reference)))
      #f))
  (check-equal? blamed 'instance))

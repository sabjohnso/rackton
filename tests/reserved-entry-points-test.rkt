#lang racket/base

;; Reserved entry-point names `main` and `test-main`.
;;
;; A `#lang rackton` module (or `(module name rackton ...)`) that
;; defines `main` gets an auto-generated `(module+ main (run-io
;; main))`; one that defines `test-main` gets `(module+ test (run-io
;; test-main))`.  Both must be typed `(IO Unit)` — either because the
;; user declared it, or because Rackton synthesizes that declaration
;; when none is given.  This only applies at module-top level
;; (`rackton/main`, i.e. `#lang rackton`); the embeddable `(rackton
;; ...)` form leaves `main`/`test-main` as ordinary names.

(require rackunit
         racket/port
         racket/runtime-path
         (for-syntax racket/base)
         "../main.rkt")

(define-runtime-path main-fixture      "reserved-main-fixture.rkt")
(define-runtime-path test-main-fixture "reserved-test-main-fixture.rkt")
(define-runtime-path both-fixture      "reserved-both-fixture.rkt")

;; ----- module+ main / module+ test emission ----------------------

(test-case "a `main`-only module gets a working `module+ main`"
           (define out
             (with-output-to-string
               (lambda () (dynamic-require `(submod ,main-fixture main) #f))))
           (check-equal? out "main ran\n"))

(test-case "a `main`-only module has no `test` submodule"
           (check-exn exn:fail?
                      (lambda () (dynamic-require `(submod ,main-fixture test) #f))))

(test-case "a `test-main`-only module gets a working `module+ test`"
           (define out
             (with-output-to-string
               (lambda () (dynamic-require `(submod ,test-main-fixture test) #f))))
           (check-equal? out "test-main ran\n"))

(test-case "a `test-main`-only module has no `main` submodule"
           (check-exn exn:fail?
                      (lambda () (dynamic-require `(submod ,test-main-fixture main) #f))))

(test-case "a module defining both gets both submodules, running independently"
           (define main-out
             (with-output-to-string
               (lambda () (dynamic-require `(submod ,both-fixture main) #f))))
           (define test-out
             (with-output-to-string
               (lambda () (dynamic-require `(submod ,both-fixture test) #f))))
           (check-equal? main-out "both: main ran\n")
           (check-equal? test-out "both: test-main ran\n"))

;; ----- type enforcement, via compile-time expansion ---------------

(define-syntax-rule (check-rackton/main-compile-error form ...)
  (check-exn
    exn:fail?
    (lambda ()
      (eval #'(rackton/main form ...)
            (variable-reference->namespace (#%variable-reference))))))

(define-syntax-rule (check-rackton/main-compiles-ok form ...)
  (check-not-exn
    (lambda ()
      (eval #'(rackton/main form ...)
            (variable-reference->namespace (#%variable-reference))))))

(test-case "main with no declared signature still resolves an ambiguous body"
           ;; `(pure Unit)` alone is ambiguous (no type-class defaulting) unless
           ;; something pins the Applicative to IO — the synthesized `(: main
           ;; (IO Unit))` declaration must do that.
           (check-rackton/main-compiles-ok
             (define main (pure Unit))))

(test-case "test-main with no declared signature still resolves an ambiguous body"
           (check-rackton/main-compiles-ok
             (define test-main (pure Unit))))

(test-case "main with an explicit, wrong declared type is rejected"
           (check-rackton/main-compile-error
             (: main Unit)
             (define main Unit)))

(test-case "test-main with an explicit, wrong declared type is rejected"
           (check-rackton/main-compile-error
             (: test-main String)
             (define test-main "not IO Unit")))

(test-case "main with the correct explicit declared type is accepted"
           (check-rackton/main-compiles-ok
             (: main (IO Unit))
             (define main (pure Unit))))

;; ----- embedded (rackton ...) blocks: ordinary names, no reservation --

(test-case "main/test-main are ordinary names inside an embedded (rackton ...) block"
           (check-not-exn
             (lambda ()
               (eval #'(rackton
                         (: main Unit)
                         (define main Unit)
                         (: test-main String)
                         (define test-main "just a string"))
                     (variable-reference->namespace (#%variable-reference))))))

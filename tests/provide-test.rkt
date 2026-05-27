#lang racket/base

;; Exercise Rackton's (provide ...) surface form.
;;
;; Under the new semantics:
;;   - A module with no (provide ...) form exports nothing.
;;   - Bare names, (all-defined-out), (data-out T), (protocol-out C),
;;     (rename-out ...), and (except-out ...) all behave like their
;;     Racket counterparts (with the Rackton-specific data-out /
;;     protocol-out additions).
;;   - Unexported bindings are invisible to importers' type checker
;;     too, not just at runtime.
;;   - Instances always escape regardless of provide-list.

(require rackunit
         (for-syntax racket/base)
         "../main.rkt")

(define-syntax-rule (check-rackton-compile-error form ...)
  (check-exn
   exn:fail?
   (lambda ()
     (eval #'(rackton form ...)
           (variable-reference->namespace (#%variable-reference))))))

(define (require-name lib name)
  (dynamic-require lib name))

(define (require-name/fails? lib name)
  (with-handlers ([exn:fail? (lambda (_) #t)])
    (dynamic-require lib name)
    #f))

;; ----- default: no provide form exports nothing -------------------

(test-case "no provide form: foo is not exported"
  (check-true (require-name/fails? "provide-lib-default.rkt" 'foo)))

(test-case "no provide form: bar is not exported"
  (check-true (require-name/fails? "provide-lib-default.rkt" 'bar)))

;; ----- bare name -------------------------------------------------

(test-case "(provide foo): foo escapes"
  (check-equal? (require-name "provide-lib-explicit.rkt" 'foo) 1))

(test-case "(provide foo): bar stays hidden"
  (check-true (require-name/fails? "provide-lib-explicit.rkt" 'bar)))

;; ----- (all-defined-out) ----------------------------------------

(test-case "(all-defined-out): foo escapes"
  (check-equal? (require-name "provide-lib-all.rkt" 'foo) 1))

(test-case "(all-defined-out): bar escapes"
  (check-equal? (require-name "provide-lib-all.rkt" 'bar) 2))

;; ----- (data-out T) ---------------------------------------------

(test-case "(data-out Box): MkBox constructor escapes"
  (define MkBox (require-name "provide-lib-data-out.rkt" 'MkBox))
  (check-not-false (MkBox 7)))

(test-case "(data-out Box): helper stays hidden"
  (check-true (require-name/fails? "provide-lib-data-out.rkt" 'helper)))

;; Hoisted: a (rackton ...) block must be at module top level so its
;; spliced (require ...) lands at module level.  We assert the
;; behavior in a separate test-case below.
(rackton
  (require "provide-lib-data-out.rkt")
  (: a-box (Box Integer))
  (define a-box (MkBox 5)))

(test-case "(data-out Box): client can use Box at the type level"
  ;; If we got here at module load without raising, success.
  ;; a-box was also Racket-defined at module level; verify it.
  (check-not-false a-box))

;; ----- (struct-out S) -------------------------------------------

(test-case "(struct-out Point): constructor escapes"
  (define Point-ctor (require-name "provide-lib-struct-out.rkt" 'Point))
  (check-not-false (Point-ctor 3 4)))

(test-case "(struct-out Point): field accessors escape"
  (check-not-false (require-name "provide-lib-struct-out.rkt" 'Point-x))
  (check-not-false (require-name "provide-lib-struct-out.rkt" 'Point-y)))

(test-case "(struct-out Point): helper stays hidden"
  (check-true (require-name/fails? "provide-lib-struct-out.rkt" 'helper)))

(rackton
  (require "provide-lib-struct-out.rkt")
  (: struct-out-px Integer)
  (define struct-out-px (Point-x (Point 7 9)))
  (: struct-out-py Integer)
  (define struct-out-py (Point-y (Point 7 9))))

(test-case "(struct-out Point): client uses type, ctor, and accessors"
  (check-equal? struct-out-px 7)
  (check-equal? struct-out-py 9))

;; ----- (protocol-out C) --------------------------------------------

(rackton
  (require "provide-lib-protocol-out.rkt")
  (: protocol-out-result Integer)
  (define protocol-out-result (my-size (MkSphere 42))))

(test-case "(protocol-out Sized): my-size method dispatches across modules"
  (check-equal? protocol-out-result 42))

(test-case "(protocol-out Sized) + (data-out Sphere): helper stays hidden"
  (check-true (require-name/fails? "provide-lib-protocol-out.rkt" 'helper)))

;; ----- (rename-out [old new]) -----------------------------------

(test-case "(rename-out [internal-name external-name]): exported under new name"
  (check-equal?
   (require-name "provide-lib-rename.rkt" 'external-name)
   42))

(test-case "(rename-out): old name does not escape"
  (check-true
   (require-name/fails? "provide-lib-rename.rkt" 'internal-name)))

;; ----- (except-out ...) -----------------------------------------

(test-case "(except-out (all-defined-out) internal-helper): foo escapes"
  (check-equal? (require-name "provide-lib-except.rkt" 'foo) 1))

(test-case "(except-out (all-defined-out) internal-helper): bar escapes"
  (check-equal? (require-name "provide-lib-except.rkt" 'bar) 2))

(test-case "(except-out (all-defined-out) internal-helper): internal-helper hidden"
  (check-true
   (require-name/fails? "provide-lib-except.rkt" 'internal-helper)))

;; ----- type-level hiding ----------------------------------------

(test-case "hidden binding pub escapes"
  (check-equal? (require-name "provide-lib-hide.rkt" 'pub) 1))

(test-case "hidden binding priv stays hidden at runtime"
  (check-true (require-name/fails? "provide-lib-hide.rkt" 'priv)))

(test-case "hidden binding priv is invisible to importer's type checker"
  ;; If the sidecar still encoded priv's scheme, the client below
  ;; would type-check but then fail at the Racket level.  We want
  ;; the FAILURE to happen at the Rackton compile-time stage —
  ;; either way `check-rackton-compile-error` is satisfied.
  (check-rackton-compile-error
   (require "provide-lib-hide.rkt")
   (: bad Integer)
   (define bad priv)))

;; ----- instances always escape ----------------------------------

(rackton
  (require "provide-lib-instance-escape.rkt")
  (: red-eq-red    Boolean)
  (define red-eq-red    (== Red Red))
  (: red-eq-green  Boolean)
  (define red-eq-green  (== Red Green)))

(test-case "instance for (Eq Color) escapes even with only (data-out Color)"
  (check-true  red-eq-red)
  (check-false red-eq-green))

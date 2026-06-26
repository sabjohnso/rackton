#lang racket/base

;; The `require` sub-forms (only-in, rename-in, prefix-in, except-in)
;; must fold the importee's bindings into the importer's TYPE env under
;; the names the sub-form selects/renames — not only at runtime (codegen
;; passes the spec through verbatim) but during inference, which folds the
;; sidecar schemes.  Before the fix the inferencer treated a wrapped spec
;; as "not a module path" and silently imported nothing, so every renamed
;; or selected name was an "unbound identifier" type error.

(require rackunit
         (for-syntax racket/base)
         "../main.rkt")

(define-syntax-rule (check-rackton-compile-error form ...)
  (check-exn
   exn:fail?
   (lambda ()
     (eval #'(rackton form ...)
           (variable-reference->namespace (#%variable-reference))))))

;; ----- rename-in -----
(rackton
  (require (rename-in "require-subforms-cross-module-lib.rkt"
                      [greet hello] [answer the-answer]))
  (: rn-greeting String)
  (define rn-greeting (hello "x"))
  (: rn-answer Integer)
  (define rn-answer the-answer))

;; ----- only-in (with a rename clause) -----
(rackton
  (require (only-in "require-subforms-cross-module-lib.rkt"
                    shout [greet g]))
  (: oi-result String)
  (define oi-result (g (shout "x"))))

;; ----- prefix-in -----
(rackton
  (require (prefix-in lib: "require-subforms-cross-module-lib.rkt"))
  (: pi-greeting String)
  (define pi-greeting (lib:greet "y"))
  (: pi-answer Integer)
  (define pi-answer lib:answer))

;; ----- except-in -----
(rackton
  (require (except-in "require-subforms-cross-module-lib.rkt" greet))
  (: ei-answer Integer)
  (define ei-answer answer)
  (: ei-shout String)
  (define ei-shout (shout "z")))

(test-case "rename-in binds the importee under the new names"
  (check-equal? rn-greeting "hi x")
  (check-equal? rn-answer 42))

(test-case "only-in keeps and renames just the selected names"
  (check-equal? oi-result "hi x!"))

(test-case "prefix-in binds every importee name behind the prefix"
  (check-equal? pi-greeting "hi y")
  (check-equal? pi-answer 42))

(test-case "except-in binds every importee name but the excluded ones"
  (check-equal? ei-answer 42)
  (check-equal? ei-shout "z!"))

(test-case "only-in drops the names it does not list"
  (check-rackton-compile-error
   (require (only-in "require-subforms-cross-module-lib.rkt" shout))
   (: bad Integer)
   (define bad answer)))

(test-case "except-in drops the names it excludes"
  (check-rackton-compile-error
   (require (except-in "require-subforms-cross-module-lib.rkt" answer))
   (: bad Integer)
   (define bad answer)))

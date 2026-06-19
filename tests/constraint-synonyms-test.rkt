#lang racket/base

;; Constraint synonyms (Feature 4, Phase 1): `(define-constraint (C p…)
;; k…)` names a conjunction of constraints.  A `(C T…)` constraint
;; expands to its components — as a GOAL (the caller must satisfy each
;; component) and as a HYPOTHESIS (a `(C a) =>` signature provides each
;; component to the body).

(require rackunit
         (for-syntax racket/base)
         "../main.rkt")

(define-syntax-rule (compile-error-message form ...)
  (with-handlers ([exn:fail? exn-message])
    (eval #'(rackton form ...)
          (variable-reference->namespace (#%variable-reference)))
    (fail "expected a compile error but the program compiled")))

;; ----- a synonym provides its components to a constrained body ------

(rackton
  (define-constraint (Stringy a) (Show a) (Eq a))

  ;; The body uses `show`, which needs `(Show a)` — provided by the
  ;; `(Stringy a)` hypothesis (synonym expanded on the hypothesis side).
  (: described ((Stringy a) => (-> a String)))
  (define (described x) (show x))

  ;; Integer has both Show and Eq, so `(Stringy Integer)` is satisfied
  ;; (synonym expanded on the goal side).
  (: out String)
  (define out (described 5))
  (: shown String)
  (define shown (show 5)))

(test-case "a constraint synonym provides and demands its components"
  (check-pred string? out)
  (check-equal? out shown))

;; ----- a synonym demands ALL its components at the use site ---------

(test-case "a use missing a component instance is rejected"
  ;; NoShow has neither Show nor Eq, so (Stringy NoShow) is unsatisfiable.
  (define msg (compile-error-message
               (define-constraint (Stringy2 a) (Show a) (Eq a))
               (: f ((Stringy2 a) => (-> a String)))
               (define (f x) (show x))
               (data NoShow MkNoShow)
               (: bad String)
               (define bad (f (MkNoShow)))))
  (check-regexp-match #rx"Show|Eq|instance" msg))

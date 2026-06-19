#lang racket/base

;; Regression test for examples/thih.rkt — the Hindley–Milner type
;; inferencer for a small lambda calculus, written in Rackton.
;;
;; The example's `main` runs at module load (via `run-io`), printing one
;; inferred type per demo term.  We instantiate the module for effect,
;; capture stdout, and pin every reported type and type-error so a
;; regression in the example (or in the language features it exercises —
;; user-defined monad + do-notation, Map/Set, let-polymorphism,
;; unification with occurs check) fails this test.

(require rackunit
         racket/port
         racket/string
         racket/runtime-path)

(define-runtime-path thih-example "../examples/thih.rkt")

;; Collapse runs of whitespace so the checks pin the reported content,
;; not the exact column padding of the demo labels.
(define (squeeze s) (regexp-replace* #px"\\s+" s " "))

(define output
  (squeeze
   (with-output-to-string
     (lambda () (dynamic-require thih-example #f)))))

(define (reports? line)
  (test-case line
    (check-true (string-contains? output (squeeze line))
                (string-append "expected in thih.rkt output: " line))))

(test-case "well-typed terms get their principal types"
  ;; identity and self-applied identity are both forall a. a -> a
  (reports? "\\x. x                       :: forall a. a -> a")
  (reports? "let id=\\x.x in id id        :: forall a. a -> a")
  ;; const drops its second argument
  (reports? "\\x. \\y. x                    :: forall a b. a -> b -> a")
  ;; function composition
  (reports? "\\f. \\g. \\x. f (g x)          :: forall a b c. (a -> b) -> (c -> a) -> c -> b")
  ;; let-polymorphism: id used at Bool and Int, whole thing is Int
  (reports? "let id=.. in if id#t..      :: Int")
  (reports? "if #t then 1 else 0         :: Int"))

(test-case "ill-typed terms report the expected failure"
  ;; applying a non-function
  (reports? "(1 2)                       :: TYPE ERROR — cannot unify Int with Int -> t0")
  ;; conditional branches must agree
  (reports? "if #t then 1 else #f        :: TYPE ERROR — cannot unify Int with Bool")
  ;; self-application fails the occurs check
  (reports? "\\x. x x                      :: TYPE ERROR — occurs check: t0 occurs in t0 -> t1"))

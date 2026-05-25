#lang racket/base

;; Phase 60: REPL polish — multi-line input + history + tab
;; completion.  The user-visible loop wraps stdin with readline,
;; but the testable surface is small: a `rackton-read-form` that
;; accumulates lines until the parens balance, plus a
;; `rackton-repl-completions` helper that lists session-env names
;; matching a prefix.

(require rackunit
         "../private/repl.rkt")

;; ----- 60.1 multi-line accumulation -----------------------------

(test-case "single complete line returns the form"
  (define p (open-input-string "(define x 5)\n"))
  (check-equal? (rackton-read-form p (lambda (_) ""))
                '(define x 5)))

(test-case "incomplete line continues on next prompt"
  (define p (open-input-string "(define x\n  (+ 1 2))\n"))
  (check-equal? (rackton-read-form p (lambda (_) ""))
                '(define x (+ 1 2))))

(test-case "three-line continuation closes when parens balance"
  (define p
    (open-input-string "(define\n  (f x)\n  (+ x x))\n"))
  (check-equal? (rackton-read-form p (lambda (_) ""))
                '(define (f x) (+ x x))))

(test-case "eof returns eof"
  (define p (open-input-string ""))
  (check-true (eof-object? (rackton-read-form p (lambda (_) "")))))

;; ----- 60.2 tab completion against session env -----------------

(test-case "completions return identifiers matching prefix"
  (define state (rackton-repl-init))
  (define-values (state* _) (rackton-repl-step state '(define foobar 7)))
  (define cands (rackton-repl-completions state* "foo"))
  (check-not-false (member "foobar" cands)
                   (format "expected `foobar` in completions, got: ~v" cands)))

(test-case "completions filter to only matching prefix"
  (define state (rackton-repl-init))
  (define-values (s1 _) (rackton-repl-step state '(define alpha 1)))
  (define-values (s2 __) (rackton-repl-step s1 '(define beta 2)))
  (define cands (rackton-repl-completions s2 "alp"))
  (check-not-false (member "alpha" cands))
  (check-false (member "beta" cands)
               (format "beta should not match `alp`, got: ~v" cands)))

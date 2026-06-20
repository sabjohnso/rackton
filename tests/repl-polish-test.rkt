#lang racket/base

;; REPL polish — multi-line input + history + tab
;; completion.  The user-visible loop wraps stdin with readline,
;; but the testable surface is small: a `rackton-read-form` that
;; accumulates lines until the parens balance, plus a
;; `rackton-repl-completions` helper that lists session-env names
;; matching a prefix.

(require rackunit
         "../private/repl.rkt")

;; The reader now returns syntax (read with `read-syntax`, so the
;; bracket/brace literals' paren-shape survives); compare on its datum.
(define (read-form p)
  (define f (rackton-read-form p (lambda (_) "")))
  (if (eof-object? f) f (syntax->datum f)))

;; ----- 60.1 multi-line accumulation -----------------------------

(test-case "single complete line returns the form"
  (define p (open-input-string "(define x 5)\n"))
  (check-equal? (read-form p) '(define x 5)))

(test-case "incomplete line continues on next prompt"
  (define p (open-input-string "(define x\n  (+ 1 2))\n"))
  (check-equal? (read-form p) '(define x (+ 1 2))))

(test-case "three-line continuation closes when parens balance"
  (define p
    (open-input-string "(define\n  (f x)\n  (+ x x))\n"))
  (check-equal? (read-form p) '(define (f x) (+ x x))))

(test-case "eof returns eof"
  (define p (open-input-string ""))
  (check-true (eof-object? (read-form p))))

;; ----- comma-prefixed commands ----------------------------------

(test-case "a comma command line reads as an (unquote ...) form"
  (define p (open-input-string ",quit\n"))
  (check-equal? (read-form p) '(unquote quit)))

(test-case "a bare comma reads as the (unquote) no-op"
  (define p (open-input-string ",\n"))
  (check-equal? (read-form p) '(unquote)))

(test-case "a comma command carries its argument expression"
  (define p (open-input-string ",type (lambda (x) x)\n"))
  (check-equal? (read-form p) '(unquote type (lambda (x) x))))

(test-case "a comma command keeps a bracket-literal argument's shape"
  ;; ,type [1 2 3] — the argument's paren-shape must survive the reader,
  ;; so the kernel can read it as a list literal rather than (1 2 3).
  (define p (open-input-string ",type [1 2 3]\n"))
  (define f (rackton-read-form p (lambda (_) "")))
  (check-equal? (syntax->datum f) '(unquote type (1 2 3)))
  (check-equal? (syntax-property (caddr (syntax->list f)) 'paren-shape) #\[))

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

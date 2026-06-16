#lang racket/base

;; REPL pretty-printing: values and types in REPL output wrap at the
;; terminal width through the shared document engine, lists render with
;; the `(list …)` surface syntax, and function types print in the
;; readable n-ary arrow form (the same form the type-error diagnostics
;; use).  These tests drive the kernel directly, like
;; repl-usability-test.rkt; `current-type-columns` is the width budget,
;; so a small value forces the wrap path deterministically.

(require rackunit
         (only-in racket/string string-split)
         "../private/repl.rkt"
         (only-in "../private/types.rkt" current-type-columns))

(define (drive-session inputs)
  (for/fold ([state (rackton-repl-init)] [out '()] #:result (reverse out))
            ([form (in-list inputs)])
    (define-values (state* o) (rackton-repl-step state form))
    (values state* (cons o out))))

(define (last-output inputs)
  (car (reverse (drive-session inputs))))

;; ----- lists render with (list …) ---------------------------------

(test-case "a Cons/Nil chain prints as (list …)"
  (define out (last-output '((Cons 1 (Cons 2 (Cons 3 Nil))))))
  (check-regexp-match #rx"\\(list 1 2 3\\)" out)
  (check-false (regexp-match #rx"Cons" out)))

(test-case "the empty list prints as (list)"
  (define out (last-output '(Nil)))
  (check-regexp-match #rx"\\(list\\)" out))

(test-case "nested data inside a list renders recursively"
  (define out (last-output '((Cons (Pair 1 1) (Cons (Pair 2 4) Nil)))))
  (check-regexp-match #rx"\\(list \\(Pair 1 1\\) \\(Pair 2 4\\)\\)" out))

;; ----- wrapping ----------------------------------------------------

(test-case "a value too wide for the budget breaks across lines"
  (parameterize ([current-type-columns 20])
    (define out (last-output '((Cons 1 (Cons 2 (Cons 3 (Cons 4 (Cons 5 Nil))))))))
    ;; more than just the trailing newline
    (check-true (> (length (regexp-match* #rx"\n" out)) 1))))

(test-case "a short value stays on one line"
  (define out (last-output '((Cons 1 (Cons 2 Nil)))))
  ;; only the trailing newline
  (check-equal? (length (regexp-match* #rx"\n" out)) 1)
  (check-regexp-match #rx"\\(list 1 2\\) :: \\(List Integer\\)" out))

;; ----- ,info long lines wrap --------------------------------------

(define (max-line-width s)
  (apply max 0 (map string-length (string-split s "\n"))))

(test-case ",info wraps a long instances list across lines"
  (parameterize ([current-type-columns 20])
    (define out (last-output '((unquote info Functor))))
    (check-regexp-match #rx"instances:\n" out)
    (check-regexp-match #rx"\\(Functor List\\)" out)))

(test-case ",info keeps every line near the width budget"
  ;; the composition law and the instances list are each far wider than
  ;; 20 columns flat; wrapping must bring every line close to the budget
  ;; (a single unbreakable token may still poke past it).
  (parameterize ([current-type-columns 20])
    (define out (last-output '((unquote info Functor))))
    (check-regexp-match #rx"composition:" out)
    (check-true (< (max-line-width out) 45))))

;; ----- n-ary type form --------------------------------------------

(test-case "function types print in the n-ary arrow form"
  (define out (last-output '((lambda (x y) x))))
  (check-regexp-match #rx"\\(-> a b a\\)" out)
  (check-false (regexp-match #rx"-> a \\(-> b a\\)" out)))

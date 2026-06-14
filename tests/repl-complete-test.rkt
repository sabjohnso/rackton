#lang racket/base

;; REPL: the ,complete command.
;;
;; ,complete PREFIX prints the completion candidates for PREFIX — the
;; session env's vars, data constructors, classes, and type
;; constructors, plus the surface keywords — one per line.  It is the
;; pipe transport for editor completion: `rackton-repl-completions`
;; already drives the terminal editor's Tab, and this command exposes
;; the same answers to a piped client.  These tests drive the kernel
;; directly.

(require rackunit
         racket/string
         "../private/repl.rkt")

(define (complete-output prefix)
  (define-values (_ o)
    (rackton-repl-step (rackton-repl-init) (list 'unquote 'complete prefix)))
  o)

(test-case ",complete lists matching prelude names and keywords"
  (define out (complete-output 'ma))
  (check-regexp-match #rx"(?m:^match$)" out)   ; a surface keyword
  (check-regexp-match #rx"(?m:^max$)" out))    ; a prelude binding

(test-case ",complete lists definition keywords for a 'de' prefix"
  (define out (complete-output 'de))
  (check-regexp-match #rx"(?m:^define$)" out)
  (check-regexp-match #rx"(?m:^delay$)" out))

(test-case ",complete prints one candidate per line"
  (define out (complete-output 'de))
  (for ([line (in-list (regexp-split #rx"\n" (string-trim out)))])
    (check-regexp-match #rx"^[^ \t]+$" line)))

(test-case ",complete with no matches prints nothing"
  (check-equal? (complete-output 'zzznotaname) ""))

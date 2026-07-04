#lang racket/base

;; The REPL scenario for first-class `rackton/prelude`: after
;; `(require (qualified-in p rackton/prelude))`, the prelude's items are
;; reachable under `p:` — so a session that has shadowed a prelude name
;; can still use the prelude version.

(require rackunit
         "../private/repl.rkt")

(define (drive-session inputs)
  (for/fold ([state (rackton-repl-init)] [out '()] #:result (reverse out))
            ([form (in-list inputs)])
    (define-values (state* o) (rackton-repl-step state form))
    (values state* (cons o out))))

;; `(require (qualified-in p rackton/prelude))` as a REPL datum.
(define require-prelude
  (list 'require (list 'qualified-in 'p 'rackton/prelude)))

(test-case "REPL: p:Cons / p:Nil build a prelude List, typed"
  (define outs
    (drive-session
     (list require-prelude
           (list 'unquote 'type '(p:Cons 1 (p:Cons 2 p:Nil))))))
  (check-regexp-match #rx"List Integer" (car (reverse outs))))

(test-case "REPL: qualified prelude function runs"
  (define outs
    (drive-session
     (list require-prelude
           '(p:length (p:Cons 1 (p:Cons 2 (p:Cons 3 p:Nil)))))))
  (check-regexp-match #rx"3" (car (reverse outs))))

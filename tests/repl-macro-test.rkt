#lang racket/base

;; REPL: user-defined macros.
;;
;; The `(rackton ...)` macro expands `define-syntax` / `define-syntax-rule`
;; before parsing; the REPL kernel must do the same one input at a time,
;; persisting macro bindings across inputs so a macro defined on one line
;; can be used on a later one.  These tests drive the kernel directly.

(require rackunit
         racket/list
         racket/runtime-path
         "../private/repl.rkt")

(define-runtime-path here-dir ".")

(define (drive-session inputs)
  ;; Returns (values final-state outputs), where outputs is the list
  ;; of per-step output strings.
  (for/fold ([state (rackton-repl-init)] [out '()] #:result (values state (reverse out)))
            ([form (in-list inputs)])
    (define-values (state* o) (rackton-repl-step state form))
    (values state* (cons o out))))

(test-case "REPL: define-syntax-rule then use it"
  (define-values (_ outs)
    (drive-session
     '((define-syntax-rule (twice x) (+ x x))
       (twice 21))))
  (check-regexp-match #rx"42" (last outs))
  (check-regexp-match #rx"Integer" (last outs)))

(test-case "REPL: define-syntax with syntax-rules"
  (define-values (_ outs)
    (drive-session
     '((define-syntax my-add
         (syntax-rules ()
           [(_ a b) (+ a b)]))
       (my-add 4 5))))
  (check-regexp-match #rx"9" (last outs))
  (check-regexp-match #rx"Integer" (last outs)))

(test-case "REPL: macro hygiene — introduced binder stays distinct"
  ;; The macro introduces its own `y`; a user `y` in the argument must not
  ;; be captured by it.  `(plus-one y)` should be `(+ user-y 1)` = 8, not
  ;; `(+ 1 1)`.
  (define-values (_ outs)
    (drive-session
     '((define-syntax-rule (plus-one v) (let ([y 1]) (+ v y)))
       (define y 7)
       (plus-one y))))
  (check-regexp-match #rx"8" (last outs))
  (check-regexp-match #rx"Integer" (last outs)))

(test-case "REPL: macro-introduced binding form over use-site binders"
  ;; The macro's template introduces the `let`; its binders and body come
  ;; from the use site.  The `let` keyword carries the macro scope, the
  ;; use-site binder/refs do not, so a renamed local must be emitted
  ;; scope-free for the binder and reference to still agree.
  (define-values (_ outs)
    (drive-session
     '((define-syntax-rule (eval-with body (binding ...)) (let (binding ...) body))
       (eval-with (+ x y) ([x 3] [y 4])))))
  (check-regexp-match #rx"7" (last outs))
  (check-regexp-match #rx"Integer" (last outs)))

(test-case "REPL: macro persists across a later unrelated input"
  (define-values (_ outs)
    (drive-session
     '((define-syntax-rule (twice x) (+ x x))
       (define k 10)
       (twice k))))
  (check-regexp-match #rx"20" (last outs))
  (check-regexp-match #rx"Integer" (last outs)))

(test-case "REPL: ,type expands a macro use before typing it"
  ;; `,type (twice 21)` must expand the macro, then report the type of the
  ;; expansion rather than failing on an unbound `twice`.
  (define-values (_ outs)
    (drive-session
     '((define-syntax-rule (twice x) (+ x x))
       (unquote type (twice 21)))))
  (check-regexp-match #rx"Integer" (last outs)))

(test-case "REPL: a required library's macro is usable"
  ;; macro-export-lib.rkt provides the pattern macro `double`; requiring it
  ;; in the session must bind the macro so a later `(double 21)` expands.
  (define-values (_ outs)
    (parameterize ([current-directory here-dir])
      (drive-session
       '((require "macro-export-lib.rkt")
         (double 21)))))
  (check-regexp-match #rx"42" (last outs))
  (check-regexp-match #rx"Integer" (last outs)))

(test-case "REPL: non-macro session is unaffected"
  ;; A session that never defines a macro must behave exactly as before.
  (define-values (_ outs)
    (drive-session
     '((define x 7)
       (+ x 1))))
  (check-regexp-match #rx"8" (last outs))
  (check-regexp-match #rx"Integer" (last outs)))

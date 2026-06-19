#lang racket/base

;; Variadic functions (homogeneous, list-collapse).
;;
;; A `...` in a type signature marks the last argument before the result
;; as repeated zero-or-more times; a dotted parameter binds the gathered
;; trailing arguments as a list.  `(-> A C ... R)` desugars to the binary
;; core type `(-> A (-> (List C) R))`, and a direct call gathers its
;; trailing args into a `(List C)`.  See CLAUDE.md / the guide.

(require rackunit
         (for-syntax racket/base)
         "../main.rkt"
         "../private/repl.rkt")

(define-syntax-rule (check-rackton-compile-error form ...)
  (check-exn
   exn:fail?
   (lambda ()
     (eval #'(rackton form ...)
           (variable-reference->namespace (#%variable-reference))))))

;; ----- single-module behavior --------------------------------------

(rackton
  ;; No fixed prefix: every argument is gathered.
  (: sum-all (-> Integer ... Integer))
  (define (sum-all . xs) (foldr + 0 xs))

  (: r-many Integer)
  (define r-many (sum-all 1 2 3 4))      ; ⇒ (sum-all (Cons 1 … Nil)) ⇒ 10

  (: r-zero Integer)
  (define r-zero (sum-all))              ; ⇒ (sum-all Nil) ⇒ 0

  (: r-one Integer)
  (define r-one (sum-all 7))             ; ⇒ 7

  ;; A fixed prefix, then a repeated tail.
  (: label-all (-> String String ... String))
  (define (label-all sep . parts) (string-join sep parts))

  (: r-pre String)
  (define r-pre (label-all "," "a" "b" "c"))   ; ⇒ "a,b,c"

  (: r-pre-empty String)
  (define r-pre-empty (label-all ","))          ; ⇒ ""

  ;; No signature: the rest parameter is inferred as a (List a).
  (define (collect . xs) xs)
  (: r-list Boolean)
  (define r-list (== (collect 1 2 3) (Cons 1 (Cons 2 (Cons 3 Nil)))))

  ;; A local binding shadows the variadic name: the inner `sum-all` is an
  ;; ordinary unary function, so `(sum-all 5)` must NOT gather (it would
  ;; otherwise apply a unary lambda to a list and fail to type).
  (: r-shadow Integer)
  (define r-shadow
    (let ([sum-all (lambda (n) (* n 2))])
      (sum-all 5))))

(test-case "variadic call gathers trailing args"
  (check-equal? r-many 10))

(test-case "variadic call with zero rest args"
  (check-equal? r-zero 0))

(test-case "variadic call with one rest arg"
  (check-equal? r-one 7))

(test-case "fixed prefix then repeated tail"
  (check-equal? r-pre "a,b,c"))

(test-case "fixed prefix with empty tail"
  (check-equal? r-pre-empty ""))

(test-case "unsignatured rest parameter is a list"
  (check-true r-list))

(test-case "a local binding shadows the variadic name"
  (check-equal? r-shadow 10))

;; ----- ill-typed: rest args must share a type ----------------------

;; A dotted define (no `...` token) lets the rejection case avoid the
;; ellipsis, which `check-rackton-compile-error`'s own `form ...`
;; template would otherwise mis-read.  `(collect 1 "x")` gathers to
;; `(Cons 1 (Cons "x" Nil))`, whose elements fail to unify.
(test-case "heterogeneous rest args are rejected"
  (check-rackton-compile-error
   (define (collect . xs) xs)
   (define bad (collect 1 "x"))))

;; ----- cross-module: arity recovered from the sidecar --------------

(rackton
  (require "variadic-cross-module-lib.rkt")
  (: r-import Integer)
  (define r-import (sum-all 10 20 30)))      ; imported variadic gathers here

(test-case "imported variadic function gathers at the call site"
  (check-equal? r-import 60))

;; ----- REPL shows the surface `...` form ---------------------------
;; A variadic binding's type is displayed in the form the user wrote,
;; not its desugared `(List C)` core type.

(define (repl-last-output inputs)
  (for/fold ([state (rackton-repl-init)] [out ""] #:result out)
            ([form (in-list inputs)])
    (define-values (state* o) (rackton-repl-step state form))
    (values state* o)))

(test-case "REPL echoes a variadic definition in surface ... form"
  (define out (repl-last-output
               (list '(: sum (-> Integer ... Integer))
                     '(define (sum . xs) (foldr + 0 xs)))))
  (check-regexp-match #rx"-> Integer [.][.][.] Integer" out))

(test-case "REPL shows the fixed prefix before the repeated type"
  (define out (repl-last-output
               (list '(define (label sep . parts) (string-join sep parts)))))
  (check-regexp-match #rx"-> String String [.][.][.] String" out))

(test-case ",info on a variadic function uses the surface ... form"
  ;; `,info sum` is the last input, so its output is what we inspect.
  (define out (repl-last-output
               (list '(: sum (-> Integer ... Integer))
                     '(define (sum . xs) (foldr + 0 xs))
                     (list 'unquote 'info 'sum))))
  (check-regexp-match #rx"-> Integer [.][.][.] Integer" out))

(test-case "REPL leaves a non-variadic definition unchanged"
  (define out (repl-last-output
               (list '(define (twice x) (* x 2)))))
  (check-regexp-match #rx"-> Integer Integer" out)
  (check-false (regexp-match? #rx"[.][.][.]" out)))

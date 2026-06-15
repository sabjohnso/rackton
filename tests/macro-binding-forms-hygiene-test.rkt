#lang racket/base

;; Binding-form hygiene coverage.  When a block defines macros, the parser
;; α-renames local binders for hygiene.  This must hold for EVERY binding
;; form, not just plain `let` and lambda: a binder introduced by `let*`,
;; `letrec`, a named `let`, or a `match` pattern must still shadow an outer
;; binding of the same name.  Each case nests a same-named binder inside an
;; outer (already-hygienic) `let`; the inner reference must see the inner
;; binder (value 2), not leak to the outer one (value 1).
;;
;; The `noop` macro merely turns this block into a macro block so hygiene is
;; active; the bug it guards against is purely about the parser's handling of
;; the inner binding forms.

(require rackunit
         "../main.rkt")

(rackton
  (define-syntax-rule (noop x) x)

  (: r-let* Integer)
  (define r-let* (let ([x 1]) (let* ([x 2]) x)))

  (: r-letrec Integer)
  (define r-letrec (let ([x 1]) (letrec ([x 2]) x)))

  (: r-named Integer)
  (define r-named (let ([x 1]) (let loop ([x 2]) x)))

  (: r-match Integer)
  (define r-match (let ([x 1]) (match 2 [x x])))

  ;; Monadic / applicative binding forms, over Maybe.  The outer `let` binds
  ;; `x` to 99; the inner monadic binder must shadow it (unwrapped value 2).
  (: r-do (Maybe Integer))
  (define r-do (let ([x 99]) (do [x <- (Some 2)] (Some x))))

  (: r-let& (Maybe Integer))
  (define r-let& (let ([x 99]) (let& ([x (Some 2)]) (Some x))))

  (: r-let% (Maybe Integer))
  (define r-let% (let ([x 99]) (let% ([x (Some 2)]) (Some x))))

  (: r-let+ (Maybe Integer))
  (define r-let+ (let ([x 99]) (let+ ([x (Some 2)]) x)))

  ;; A macro whose TEMPLATE introduces the binding form, wrapping use-site
  ;; code passed as arguments.  Unlike every case above — where the user
  ;; writes the binder directly and all identifiers share one context — here
  ;; the `let` keyword carries the macro-introduction scope while the
  ;; use-site binders `x`/`y` and their references do not.  Binder and
  ;; reference are the same renamed gensym, but codegen must emit a renamed
  ;; local scope-free; otherwise the binder (scoped by the `let` form) and
  ;; the reference (scoped by the use-site identifier) disagree and the
  ;; reference fails to bind.
  (define-syntax-rule (eval-with body (binding ...))
    (let (binding ...) body))

  (: r-template-let Integer)
  (define r-template-let (eval-with (+ x y) ([x 3] [y 4]))))

(test-case "let* binder shadows hygienically"   (check-equal? r-let* 2))
(test-case "letrec binder shadows hygienically" (check-equal? r-letrec 2))
(test-case "named let binder shadows hygienically" (check-equal? r-named 2))
(test-case "match pattern var shadows hygienically" (check-equal? r-match 2))
(test-case "do binder shadows hygienically"   (check-equal? r-do (Some 2)))
(test-case "let& binder shadows hygienically" (check-equal? r-let& (Some 2)))
(test-case "let% binder shadows hygienically" (check-equal? r-let% (Some 2)))
(test-case "let+ binder shadows hygienically" (check-equal? r-let+ (Some 2)))
(test-case "macro-introduced binding form over use-site binders"
  (check-equal? r-template-let 7))

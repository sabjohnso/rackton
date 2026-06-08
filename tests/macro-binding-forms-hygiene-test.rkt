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
  (define r-let+ (let ([x 99]) (let+ ([x (Some 2)]) x))))

(test-case "let* binder shadows hygienically"   (check-equal? r-let* 2))
(test-case "letrec binder shadows hygienically" (check-equal? r-letrec 2))
(test-case "named let binder shadows hygienically" (check-equal? r-named 2))
(test-case "match pattern var shadows hygienically" (check-equal? r-match 2))
(test-case "do binder shadows hygienically"   (check-equal? r-do (Some 2)))
(test-case "let& binder shadows hygienically" (check-equal? r-let& (Some 2)))
(test-case "let% binder shadows hygienically" (check-equal? r-let% (Some 2)))
(test-case "let+ binder shadows hygienically" (check-equal? r-let+ (Some 2)))

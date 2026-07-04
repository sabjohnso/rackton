#lang racket/base

;; `proc` (arrow) notation must bind to the Category/Arrow/Product METHODS
;; it desugars to (`comp`, `fanout`, `ident`, `arr`, `mk-prod`, …) and to
;; the prelude `Pair` for its environment tuples — never to a same-named
;; local shadow.  This block shadows the internally-emitted names with
;; type-INCOMPATIBLE bindings and checks a `proc` still runs correctly.
;; (`arr` is written explicitly by the user in `feed (arr …)`, so it is
;; NOT shadowed here — a user shadow of it is an ordinary, intended one.)

(require rackunit)

(module proc-blk rackton
  (provide result)

  (: comp (-> a b Boolean))        ; incompatible shadow (Category.comp)
  (define (comp g f) #t)
  (: fanout (-> a b Boolean))      ; incompatible shadow (Arrow.fanout)
  (define (fanout a b) #t)
  (: ident Boolean)                ; incompatible shadow (Category.ident)
  (define ident #t)
  (data (Foo a) (Pair a))          ; shadow the prelude Pair (env tuples)

  (: inc (-> Integer Integer))
  (define (inc x) (+ x 1))
  (: dbl (-> Integer Integer))
  (define (dbl x) (* x 2))

  ;; A binding proc: emits comp / fanout / ident / Pair-pattern internally.
  (: p (-> Integer Integer))
  (define p (proc (x)
              [y <- (feed (arr inc) x)]
              (feed (arr dbl) y)))
  (: result Integer)
  (define result (p 5)))           ; (5 + 1) * 2 = 12

;; ArrowChoice path: an `if` command emits fanin / inj-left / inj-right.
(module choice-blk rackton
  (provide result)
  (: fanin (-> a b Boolean))       ; positional method shadow
  (define (fanin a b) #t)
  (: inj-left (-> a Boolean))      ; return-typed method shadow
  (define (inj-left a) #t)
  (: inj-right (-> a Boolean))     ; return-typed method shadow
  (define (inj-right a) #t)
  (: inc (-> Integer Integer))
  (define (inc x) (+ x 1))
  (: dbl (-> Integer Integer))
  (define (dbl x) (* x 2))
  (: p (-> Integer Integer))
  (define p (proc (x)
              (if (< x 0)
                  (feed (arr inc) x)
                  (feed (arr dbl) x))))
  (: result Integer)
  (define result (p 5)))           ; 5 >= 0 → 5 * 2 = 10

(require (submod "." proc-blk)
         (prefix-in ch: (submod "." choice-blk)))

(test-case "proc notation binds to arrow methods / prelude Pair under shadows"
  (check-equal? result 12))

(test-case "proc if-command binds to fanin / inj-left / inj-right methods under shadows"
  (check-equal? ch:result 10))

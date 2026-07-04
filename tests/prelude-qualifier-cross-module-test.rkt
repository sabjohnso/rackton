#lang racket/base

;; First-class `rackton/prelude`: `(require (qualified-in p rackton/prelude))`
;; binds every prelude item under the `p:` prefix, at both the type level
;; (via rackton/prelude's `rackton-schemes` sidecar) and runtime.  The
;; motivating case: a module may locally shadow a prelude constructor
;; (its own `Cons`) and still name the prelude's `Cons`/`Nil` as
;; `p:Cons`/`p:Nil`.

(require rackunit
         (for-syntax racket/base)
         "../main.rkt")

(rackton
  (require (qualified-in p rackton/prelude))

  ;; The module defines its OWN Cons (a Nonempty-List constructor),
  ;; shadowing the prelude's.  `p:Cons`/`p:Nil` still name the prelude's.
  (data (Nonempty-List a)
    (Sole a)
    (Cons a (Nonempty-List a)))

  ;; Bare Cons builds a Nonempty-List.
  (: ne (Nonempty-List Integer))
  (define ne (Cons 1 (Cons 2 (Sole 3))))

  (: ne-length (-> (Nonempty-List a) Integer))
  (define (ne-length xs)
    (match xs
      [(Sole _)    1]
      [(Cons _ ys) (+ 1 (ne-length ys))]))

  (: ne-len Integer)
  (define ne-len (ne-length ne))

  ;; p:Cons / p:Nil build a prelude List; p:length consumes it.
  (: pl (List Integer))
  (define pl (p:Cons 10 (p:Cons 20 p:Nil)))

  (: pl-len Integer)
  (define pl-len (p:length pl))

  ;; Qualified higher-order prelude functions.
  (: doubled (List Integer))
  (define doubled (p:fmap (lambda (x) (p:* x 2)) pl))

  (: doubled-len Integer)
  (define doubled-len (p:length doubled))

  (: bigs (List Integer))
  (define bigs (p:filter (lambda (x) (p:> x 15)) doubled))

  (: bigs-len Integer)
  (define bigs-len (p:length bigs)))

(test-case "own Cons builds Nonempty-List"
  (check-equal? ne-len 3))

(test-case "p:Cons / p:Nil build a prelude List, p:length consumes it"
  (check-equal? pl-len 2))

(test-case "qualified higher-order prelude functions (p:fmap, p:filter)"
  (check-equal? doubled-len 2)
  ;; doubled = (20 40); keep those > 15 → both
  (check-equal? bigs-len 2))

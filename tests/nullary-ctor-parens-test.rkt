#lang racket/base

;; A nullary data constructor may optionally be parenthesized, wherever it
;; appears: in a data declaration, as an expression, and as a pattern.
;; `(None)` and `None` must denote exactly the same constructor.

(require rackunit
         "../main.rkt")

(rackton
  (data Color
    (Red)   ; declared with parens
    Green   ; declared bare
    Blue)

  (: red-as-expr Color)
  (define red-as-expr (Red))   ; constructed with parens

  (: green-as-expr Color)
  (define green-as-expr Green) ; constructed bare

  (: name (-> Color String))
  (define (name c)
    (match c
      [(Red)   "red"]           ; matched with parens
      [Green   "green"]         ; matched bare
      [Blue    "blue"]))

  (: blue-value Color)
  (define blue-value Blue))

(check-equal? (name red-as-expr) "red")
(check-equal? (name green-as-expr) "green")
(check-equal? (name blue-value) "blue")

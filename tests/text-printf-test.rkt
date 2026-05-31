#lang racket/base

;; rackton/text/printf — type-safe formatting combinators.  A Format is
;; composed from typed directives; the argument function type is built
;; up and checked at compile time (no runtime format string).

(require rackunit
         "../main.rkt")

(rackton
  (require rackton/text/printf)

  ;; multi-directive: type is inferred as (-> Integer (-> String String))
  (: msg (-> Integer (-> String String)))
  (define msg
    (sprintf (fmt-cat (fmt-lit "x=")
             (fmt-cat fmt-int
              (fmt-cat (fmt-lit ", y=") fmt-str)))))
  (: out String) (define out ((msg 5) "hi"))

  ;; literal only — no args
  (: lit-only String) (define lit-only (sprintf (fmt-lit "hello")))

  ;; float directive
  (: flt-out String)
  (define flt-out ((sprintf (fmt-cat (fmt-lit "pi~") fmt-flt)) 3.5))

  ;; generic Show directive
  (: show-out String)
  (define show-out ((sprintf (fmt-cat (fmt-lit "n=") fmt-show)) 42)))

;; ---------- assertions ---------------------------------------

(test-case "multi-directive compose"
  (check-equal? out "x=5, y=hi"))

(test-case "literal only"
  (check-equal? lit-only "hello"))

(test-case "float directive"
  (check-equal? flt-out "pi~3.5"))

(test-case "generic Show directive"
  (check-equal? show-out "n=42"))

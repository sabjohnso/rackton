#lang rackton

;; Symbol primitive: quote-form literals, Eq/Ord/Show, String conversions,
;; and literal patterns.

(require "../unit.rkt")

;; ----- Symbol literal + Eq/Ord/Show ---------------------
(: sym-foo Symbol)
(define sym-foo 'foo)

(: sym-bar Symbol)
(define sym-bar 'bar)

(: foo=foo Boolean)
(define foo=foo (== sym-foo 'foo))

(: foo/=bar Boolean)
(define foo/=bar (/= sym-foo sym-bar))

(: bar<foo Boolean)
(define bar<foo (< sym-bar sym-foo))

(: show-foo String)
(define show-foo (show sym-foo))

;; ----- Symbol ↔ String ----------------------------------
(: foo-as-string String)
(define foo-as-string (symbol->string 'foo))

(: hi-as-symbol Symbol)
(define hi-as-symbol (string->symbol "hi"))

;; ----- Symbol literal pattern ---------------------------
(: classify (-> Symbol String))
(define (classify s)
  (match s
    ('foo "got foo")
    ('bar "got bar")
    (_    "other")))

(: matched-foo String)
(define matched-foo (classify 'foo))

(: matched-other String)
(define matched-other (classify 'baz))

;; ---------- assertions -----------------------------------

(: suite (List Test))
(define suite
  (list
   (it "Symbol Eq + /="
       (all-checks
        (list (check-true foo=foo)
              (check-true foo/=bar))))
   (it "Symbol Ord"
       (check-true bar<foo))
   (it "Symbol Show is round-trippable"
       (check-equal? show-foo "'foo"))
   (it "symbol->string"
       (check-equal? foo-as-string "foo"))
   (it "string->symbol round-trip"
       (check-equal? hi-as-symbol 'hi))
   (it "symbol literal patterns"
       (all-checks
        (list (check-equal? matched-foo   "got foo")
              (check-equal? matched-other "other"))))))

(: main Unit)
(define main (run-io (run-suite "symbol" suite)))

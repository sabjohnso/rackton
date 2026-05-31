#lang racket/base

;; rackton/text/string — additional Data.Text/Data.List-style String ops:
;; affix predicates, take/drop, padding, repeat, replace.

(require rackunit
         "../main.rkt")

(rackton
  (require rackton/text/string)

  (: p1 Boolean) (define p1 (is-prefix? "he" "hello"))
  (: p2 Boolean) (define p2 (is-prefix? "lo" "hello"))
  (: s1 Boolean) (define s1 (is-suffix? "lo" "hello"))
  (: s2 Boolean) (define s2 (is-suffix? "he" "hello"))
  (: i1 Boolean) (define i1 (is-infix? "ell" "hello"))
  (: i2 Boolean) (define i2 (is-infix? "xyz" "hello"))
  (: i3 Boolean) (define i3 (is-infix? "" "hello"))

  (: tk String) (define tk (take-string 3 "hello"))
  (: dp String) (define dp (drop-string 3 "hello"))

  (: pl  String) (define pl  (pad-left 5 #\* "ab"))
  (: pl2 String) (define pl2 (pad-left 2 #\* "abcd"))
  (: pr  String) (define pr  (pad-right 5 #\* "ab"))

  (: rp  String) (define rp  (repeat-string 3 "ab"))
  (: rp0 String) (define rp0 (repeat-string 0 "x"))

  (: re  String) (define re  (replace "ll" "LL" "hello"))
  (: re2 String) (define re2 (replace "o" "0" "foo boo"))
  (: re3 String) (define re3 (replace "x" "y" "abc")))

;; ---------- assertions ---------------------------------------

(test-case "affix predicates"
  (check-true p1) (check-false p2)
  (check-true s1) (check-false s2)
  (check-true i1) (check-false i2) (check-true i3))

(test-case "take / drop"
  (check-equal? tk "hel")
  (check-equal? dp "lo"))

(test-case "padding"
  (check-equal? pl "***ab")
  (check-equal? pl2 "abcd")
  (check-equal? pr "ab***"))

(test-case "repeat"
  (check-equal? rp "ababab")
  (check-equal? rp0 ""))

(test-case "replace"
  (check-equal? re "heLLo")
  (check-equal? re2 "f00 b00")
  (check-equal? re3 "abc"))

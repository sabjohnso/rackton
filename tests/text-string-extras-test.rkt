#lang rackton

;; rackton/text/string — additional Data.Text/Data.List-style String ops:
;; affix predicates, take/drop, padding, repeat, replace.

(require rackton/text/string
         "../unit.rkt")

(: p1 Boolean) (define p1 (string-prefix? "he" "hello"))
(: p2 Boolean) (define p2 (string-prefix? "lo" "hello"))
(: s1 Boolean) (define s1 (string-suffix? "lo" "hello"))
(: s2 Boolean) (define s2 (string-suffix? "he" "hello"))
(: i1 Boolean) (define i1 (string-infix? "ell" "hello"))
(: i2 Boolean) (define i2 (string-infix? "xyz" "hello"))
(: i3 Boolean) (define i3 (string-infix? "" "hello"))

(: tk String) (define tk (take-string 3 "hello"))
(: dp String) (define dp (drop-string 3 "hello"))

(: pl  String) (define pl  (pad-left 5 #\* "ab"))
(: pl2 String) (define pl2 (pad-left 2 #\* "abcd"))
(: pr  String) (define pr  (pad-right 5 #\* "ab"))

(: rp  String) (define rp  (repeat-string 3 "ab"))
(: rp0 String) (define rp0 (repeat-string 0 "x"))

(: re  String) (define re  (replace "ll" "LL" "hello"))
(: re2 String) (define re2 (replace "o" "0" "foo boo"))
(: re3 String) (define re3 (replace "x" "y" "abc"))

(: suite (List Test))
(define suite
  (list
    (it "affix predicates"
        (all-checks
          (list (check-true p1) (check-false p2)
                (check-true s1) (check-false s2)
                (check-true i1) (check-false i2) (check-true i3))))
    (it "take / drop"
        (all-checks
          (list (check-equal? tk "hel")
                (check-equal? dp "lo"))))
    (it "padding"
        (all-checks
          (list (check-equal? pl "***ab")
                (check-equal? pl2 "abcd")
                (check-equal? pr "ab***"))))
    (it "repeat"
        (all-checks
          (list (check-equal? rp "ababab")
                (check-equal? rp0 ""))))
    (it "replace"
        (all-checks
          (list (check-equal? re "heLLo")
                (check-equal? re2 "f00 b00")
                (check-equal? re3 "abc"))))))

(: test-main (IO Unit))
(define test-main (run-suite "rackton/text/string extras" suite))

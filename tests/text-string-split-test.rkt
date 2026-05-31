#lang racket/base

;; rackton/text/string — substring splitting: split-on (keeps empties),
;; break-on (split at first occurrence), index-of.

(require rackunit
         "../main.rkt")

(rackton
  (require rackton/text/string)

  ;; split-on, joined back with "|" for easy assertion
  (: so1 String) (define so1 (string-join "|" (split-on "," "a,b,c")))
  (: so2 String) (define so2 (string-join "|" (split-on "," "a,,b")))
  (: so3 String) (define so3 (string-join "|" (split-on "," "abc")))
  (: so4 String) (define so4 (string-join "|" (split-on "::" "x::y::z")))

  ;; break-on: components of the pair
  (: bo1l String) (define bo1l (match (break-on "::" "a::b::c") [(MkPair a _) a]))
  (: bo1r String) (define bo1r (match (break-on "::" "a::b::c") [(MkPair _ b) b]))
  (: bo2l String) (define bo2l (match (break-on "x" "abc") [(MkPair a _) a]))
  (: bo2r String) (define bo2r (match (break-on "x" "abc") [(MkPair _ b) b]))

  ;; index-of (-1 for None)
  (: io1 Integer) (define io1 (match (index-of "lo" "hello") [(Some i) i] [(None) -1]))
  (: io2 Integer) (define io2 (match (index-of "z"  "hello") [(Some i) i] [(None) -1]))
  (: io3 Integer) (define io3 (match (index-of ""   "abc")   [(Some i) i] [(None) -1])))

;; ---------- assertions ---------------------------------------

(test-case "split-on keeps empties"
  (check-equal? so1 "a|b|c")
  (check-equal? so2 "a||b")
  (check-equal? so3 "abc")
  (check-equal? so4 "x|y|z"))

(test-case "break-on at first occurrence"
  (check-equal? bo1l "a")
  (check-equal? bo1r "::b::c")
  (check-equal? bo2l "abc")
  (check-equal? bo2r ""))

(test-case "index-of"
  (check-equal? io1 3)
  (check-equal? io2 -1)
  (check-equal? io3 0))

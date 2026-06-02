#lang rackton

;; rackton/text/string — substring splitting: split-on (keeps empties),
;; break-on (split at first occurrence), index-of.

(require rackton/text/string
         "../unit.rkt")

;; split-on, joined back with "|" for easy assertion
(: so1 String) (define so1 (string-join "|" (split-on "," "a,b,c")))
(: so2 String) (define so2 (string-join "|" (split-on "," "a,,b")))
(: so3 String) (define so3 (string-join "|" (split-on "," "abc")))
(: so4 String) (define so4 (string-join "|" (split-on "::" "x::y::z")))

;; break-on: components of the pair
(: bo1l String) (define bo1l (match (break-on "::" "a::b::c") [(Pair a _) a]))
(: bo1r String) (define bo1r (match (break-on "::" "a::b::c") [(Pair _ b) b]))
(: bo2l String) (define bo2l (match (break-on "x" "abc") [(Pair a _) a]))
(: bo2r String) (define bo2r (match (break-on "x" "abc") [(Pair _ b) b]))

;; index-of (-1 for None)
(: io1 Integer) (define io1 (match (index-of "lo" "hello") [(Some i) i] [(None) -1]))
(: io2 Integer) (define io2 (match (index-of "z"  "hello") [(Some i) i] [(None) -1]))
(: io3 Integer) (define io3 (match (index-of ""   "abc")   [(Some i) i] [(None) -1]))

(: suite (List Test))
(define suite
  (list
   (it "split-on keeps empties"
       (all-checks
        (list (check-equal? so1 "a|b|c")
              (check-equal? so2 "a||b")
              (check-equal? so3 "abc")
              (check-equal? so4 "x|y|z"))))
   (it "break-on at first occurrence"
       (all-checks
        (list (check-equal? bo1l "a")
              (check-equal? bo1r "::b::c")
              (check-equal? bo2l "abc")
              (check-equal? bo2r ""))))
   (it "index-of"
       (all-checks
        (list (check-equal? io1 3)
              (check-equal? io2 -1)
              (check-equal? io3 0))))))

(: _ran Unit)
(define _ran (run-io (run-suite "rackton/text/string split" suite)))

#lang rackton

;; The lazy-function arrow `LFun` (rackton/data/arrow-lazy) is the first
;; arrow with a runnable `ArrowLoop`: a `proc rec` over it ties a
;; value-recursion knot that the strict `(->)` arrow cannot.  These tests
;; exercise (1) ordinary `proc` over `LFun` (feed / bind / if — the
;; Category/Arrow/ArrowChoice instances), and (2) the payoff: a productive
;; `proc rec` that builds a self-referential infinite stream and reads a
;; finite prefix of it.

(require "../unit.rkt"
         rackton/data/arrow-lazy)

;; ----- ordinary proc over LFun (no recursion) -----------------------

(: inc (-> Integer Integer))
(define (inc n) (+ n 1))

(: dbl (-> Integer Integer))
(define (dbl n) (* n 2))

;; single feed: run a lifted function on the input
(: p-inc (LFun Integer Integer))
(define p-inc (proc (x) (feed (arr inc) x)))

;; bind then use the bound value
(: p-bind (LFun Integer Integer))
(define p-bind
  (proc (x)
    [y <- (feed (arr dbl) x)]
    (feed (arr inc) y)))            ; inc (dbl x)

;; if: route through the coproduct (ArrowChoice LFun) and fan back in
(: p-if (LFun Integer Integer))
(define p-if
  (proc (x)
    (if (< x 0)
        (feed (arr (lambda (n) (- 0 n))) x)   ; negate
        (feed (arr dbl) x))))                 ; double

;; ----- the payoff: a productive proc rec ----------------------------
;; `s` is defined in terms of itself: `lcons 1` prepends 1 and drops the
;; feedback `s` into the new stream's lazy tail, so the knot ties to the
;; infinite stream 1,1,1,…  `lcons` (not `arr`) keeps it productive.
(: ones (Stream Integer))
(define ones
  (run-lfun
   (proc (_)
     (rec [s <- (feed (lcons 1) s)])
     (feed (arr (lambda (z) z)) s))
   0))

(: suite (List Test))
(define suite
  (list
   (it "single feed runs the lifted function"
       (check-equal? (run-lfun p-inc 5) 6))
   (it "bind keeps the bound value in scope for a later feed"
       (check-equal? (run-lfun p-bind 3) 7))      ; inc (dbl 3) = 7
   (it "if routes through the lazy coproduct"
       (all-checks
        (list (check-equal? (run-lfun p-if 5) 10)
              (check-equal? (run-lfun p-if -3) 3))))
   (it "proc rec ties a productive value-recursion knot (stream head)"
       (check-equal? (stream-head ones) (Some 1)))
   (it "the looped stream really is infinitely many 1s (finite prefix)"
       (check-equal? (stream-take 5 ones)
                     (Cons 1 (Cons 1 (Cons 1 (Cons 1 (Cons 1 Nil)))))))))

(: _ran Unit)
(define _ran (run-io (run-suite "lazy-arrow" suite)))

#lang rackton

;; End-to-end tests for `proc` arrow notation — Rackton's point-free
;; command syntax, desugared at parse time (like `do`) into the
;; Category/Arrow combinators.  Built up command-by-command; each `proc`
;; is checked against its hand-written combinator equivalent over the
;; `(->)` instance.
;;
;; Command vocabulary (non-infix, matching the combinator names):
;;   (feed arr e)        run arrow `arr` on expression `e`        (Haskell  arr -< e)
;;   [v <- (feed arr e)] bind the result of a command to `v`
;;   (feed-apply af e)   run a computed arrow `af` on `e`         (Haskell  af  -<< e)
;;   (let ([v e] …) …)   pure let-binding in the command stream
;;   (if t c1 c2)        choose a command by a Boolean test       (ArrowChoice)
;;   (match e [pat c] …) choose a command by matching `e`         (ArrowChoice)

(require "../unit.rkt")

(: inc (-> Integer Integer))
(define (inc x) (+ x 1))

(: dbl (-> Integer Integer))
(define (dbl x) (* x 2))

;; ----- a single feed command --------------------------------------

(: p-inc (-> Integer Integer))
(define p-inc (proc (x) (feed (arr inc) x)))

;; ----- bind, and using earlier bindings ----------------------------

(: p-bind (-> Integer Integer))
(define p-bind
  (proc (x)
    [y <- (feed (arr inc) x)]
    (feed (arr dbl) y)))

;; both the proc input and a bound variable are in scope downstream.
(: add-pair (-> (Pair Integer Integer) Integer))
(define (add-pair p) (match p [(Pair a b) (+ a b)]))

(: p-sum (-> Integer Integer))
(define p-sum
  (proc (x)
    [y <- (feed (arr inc) x)]
    (feed (arr add-pair) (Pair x y))))

;; ----- let in the command stream -----------------------------------

(: p-let (-> Integer Integer))
(define p-let
  (proc (x)
    (let ([y (* x 2)]))
    (feed (arr inc) y)))

;; ----- arrow if ----------------------------------------------------

(: p-if (-> Integer Integer))
(define p-if
  (proc (x)
    (if (< x 0)
        (feed (arr negate) x)
        (feed (arr dbl) x))))

;; ----- feed-apply (the arrow is read from the environment) ---------

(: p-apply (-> (Pair (-> Integer Integer) Integer) Integer))
(define p-apply
  (proc ((Pair f n))
    (feed-apply f n)))

;; ----- via (banana brackets): combine sub-commands with an op ------

(: p-via (-> Integer (Pair Integer Integer)))
(define p-via
  (proc (x)
    (via fanout (feed (arr inc) x) (feed (arr dbl) x))))

;; ----- arrow match (case) — 2-way and 3-way ------------------------

(: classify (-> Integer (Maybe Integer)))
(define (classify n) (if (< n 0) None (Some n)))

(: p-case (-> Integer Integer))
(define p-case
  (proc (x)
    (match (classify x)
      [(None)   (feed (arr negate) x)]   ; uses the proc input
      [(Some y) (feed (arr dbl) y)])))    ; uses the branch binding

(data Sign Neg Zero Pos)

(: sign-of (-> Integer Sign))
(define (sign-of n) (if (< n 0) Neg (if (== n 0) Zero Pos)))

(: p-sign (-> Integer Integer))
(define p-sign
  (proc (x)
    (match (sign-of x)
      [Neg  (feed (arr negate) x)]
      [Zero (feed (arr inc) x)]
      [Pos  (feed (arr dbl) x)])))

;; ----- rec / ArrowLoop ---------------------------------------------
;;
;; `rec` desugars to `arrow-loop`, which has no `(->)` instance (strict
;; functions can't tie the recursive knot — see typecheck-error-test for
;; the rejection over `(->)`).  So we exercise the translation's
;; type-correctness with a builder polymorphic over ANY ArrowLoop arrow:
;; if this definition elaborates, `rec` desugars to a well-typed
;; combinator expression.  It is wrapped in a function so the polymorphic
;; (needs-dict) body is never forced here — it is never run (no concrete
;; instance exists to run it on).

(: build-feedback ((ArrowLoop cat p) => (-> Boolean (cat Integer Integer))))
(define (build-feedback _ignore)
  (proc (x)
    (rec [s <- (feed (arr inc) s)])
    (feed (arr dbl) s)))

(: suite (List Test))
(define suite
  (list
   (it "proc with one feed runs the arrow on the bound input"
       (all-checks
        (list (check-equal? (p-inc 5) 6)
              (check-equal? (p-inc 0) 1))))
   (it "bind threads a command's result to a later feed"
       (check-equal? (p-bind 3) 8))       ; dbl (inc 3) = 8
   (it "both the input and bound vars stay in scope"
       (check-equal? (p-sum 3) 7))        ; x=3, y=inc 3=4, x+y=7
   (it "let extends the environment for following commands"
       (check-equal? (p-let 5) 11))       ; y = 5*2 = 10, inc 10 = 11
   (it "arrow if selects a branch by a Boolean test"
       (all-checks
        (list (check-equal? (p-if 5)  10)  ; dbl 5
              (check-equal? (p-if -3) 3)))) ; negate -3
   (it "feed-apply runs an environment-supplied arrow"
       (check-equal? (p-apply (Pair inc 5)) 6))
   (it "via applies an arrow combinator to sub-commands"
       (check-equal? (p-via 3) (Pair 4 6)))
   (it "arrow match chooses a branch (2-way) and binds branch vars"
       (all-checks
        (list (check-equal? (p-case 5)  10)   ; Some 5 → dbl 5
              (check-equal? (p-case -3) 3))))  ; None    → negate -3
   (it "arrow match nests correctly (3-way)"
       (all-checks
        (list (check-equal? (p-sign -3) 3)    ; Neg  → negate -3
              (check-equal? (p-sign 0)  1)    ; Zero → inc 0
              (check-equal? (p-sign 4)  8)))))) ; Pos  → dbl 4

(: _ran Unit)
(define _ran (run-io (run-suite "arrow-notation" suite)))

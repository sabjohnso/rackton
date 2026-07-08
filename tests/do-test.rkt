#lang rackton

;; do-notation desugars to flatmap chains over any Monad.

(require "../unit.rkt")

(: triple-some (-> Integer (Maybe Integer)))
(define (triple-some n)
  (do [a <- (Some n)]
    [b <- (Some (+ a 1))]
    [c <- (Some (+ b 1))]
    (Some (+ (+ a b) c))))

(: short-circuit (-> Integer (Maybe Integer)))
(define (short-circuit n)
  (do [_ <- (Some n)]
    [_ <- None]            ;; short-circuits
    (Some 99)))

(: divide-or-fail (-> Integer (-> Integer (Either String Integer))))
(define (divide-or-fail x y)
  (do [v <- (if (== y 0)
              (Left "divide by zero")
              (Right (racket Integer (x y) (quotient x y))))]
    (Right v)))

;; Bare-expression clauses: sequenced without binding the result.
;; (do (Some 1) [x <- (Some 2)] (Some (+ x 10))) ≡
;; (do [_ <- (Some 1)] [x <- (Some 2)] (Some (+ x 10)))
(: bare-clause-seq (Maybe Integer))
(define bare-clause-seq
  (do (Some 1)
    [x <- (Some 2)]
    (Some (+ x 10))))

;; A bare clause that short-circuits.
(: bare-clause-short (Maybe Integer))
(define bare-clause-short
  (do (Some 1)
    None
    (Some 99)))

;; ----- checks ------

(: suite (List Test))
(define suite
  (list
    (it "do over Maybe accumulates binds"
        (all-checks
          (list (check-equal? (triple-some 1) (Some 6))   ; 1+2+3
                (check-equal? (triple-some 5) (Some 18))))) ; 5+6+7
    (it "None in the middle short-circuits the chain"
        (check-equal? (short-circuit 7) None))
    (it "do over Either propagates Left"
        (all-checks
          (list (check-equal? (divide-or-fail 10 2) (Right 5))
                (check-equal? (divide-or-fail 10 0) (Left "divide by zero")))))
    (it "bare-expression do clauses sequence without binding"
        (all-checks
          (list (check-equal? bare-clause-seq (Some 12))
                (check-equal? bare-clause-short None))))))

(: test-main (IO Unit))
(define test-main (run-suite "do" suite))

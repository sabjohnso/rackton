#lang racket/base

;; do-notation desugars to >>= chains over any Monad.

(require rackunit
         "../main.rkt")

(rackton
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

  (: divide-or-fail (-> Integer (-> Integer (Result String Integer))))
  (define (divide-or-fail x y)
    (do [v <- (if (== y 0)
                  (Err "divide by zero")
                  (Ok (racket Integer (x y) (quotient x y))))]
      (Ok v)))

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
      (Some 99))))

;; ----- checks ------

(test-case "do over Maybe accumulates binds"
  (check-equal? (triple-some 1) (Some 6))   ; 1+2+3
  (check-equal? (triple-some 5) (Some 18))) ; 5+6+7

(test-case "None in the middle short-circuits the chain"
  (check-equal? (short-circuit 7) None))

(test-case "do over Result propagates Err"
  (check-equal? (divide-or-fail 10 2) (Ok 5))
  (check-equal? (divide-or-fail 10 0) (Err "divide by zero")))

(test-case "bare-expression do clauses sequence without binding"
  (check-equal? bare-clause-seq (Some 12))
  (check-equal? bare-clause-short None))

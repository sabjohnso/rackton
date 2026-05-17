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
      (Ok v))))

;; ----- checks ------

(test-case "do over Maybe accumulates binds"
  (check-equal? (triple-some 1) (Some 6))   ; 1+2+3
  (check-equal? (triple-some 5) (Some 18))) ; 5+6+7

(test-case "None in the middle short-circuits the chain"
  (check-equal? (short-circuit 7) None))

(test-case "do over Result propagates Err"
  (check-equal? (divide-or-fail 10 2) (Ok 5))
  (check-equal? (divide-or-fail 10 0) (Err "divide by zero")))

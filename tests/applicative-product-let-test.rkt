#lang rackton

;; Regression: `let+` and `product` over Applicatives that use the
;; default `product = liftA2 Pair`.
;;
;; The default applied the raw 2-arg `Pair` constructor in curried,
;; one-argument-at-a-time style; n-ary data constructors do not curry
;; as first-class values, so every such use threw
;;
;;   $val:Pair: arity mismatch; expected: 2  given: 1
;;
;; This reproduced on the prelude's own Result and Maybe (a two-binding
;; `let+` desugars through `product`).  Pins the fix.

(require rackton/data/result
         "../unit.rkt")

;; two-binding let+ desugars through `product`
(: r-sum (Result String Integer))
(define r-sum (let+ ([a (Ok 3)] [b (Ok 4)]) (+ a b)))

(: r-err (Result String Integer))
(define r-err
  (let+ ([a (Ok 3)]
         [b (ann (Err "boom") (Result String Integer))])
    (+ a b)))

(: m-sum (Maybe Integer))
(define m-sum (let+ ([a (Some 3)] [b (Some 4)]) (+ a b)))

;; direct product builds the pair
(: r-prod (Result String (Pair Integer Integer)))
(define r-prod (product (Ok 3) (Ok 4)))

(: suite (List Test))
(define suite
  (list
    (it "let+ over Result (two binds, via product)"
        (all-checks
          (list (check-true (match r-sum [(Ok n) (== n 7)] [(Err _) #f])))))

    (it "let+ over Result short-circuits on Err"
        (all-checks
          (list (check-true (match r-err [(Err e) (== e "boom")] [(Ok _) #f])))))

    (it "let+ over Maybe (two binds)"
        (all-checks
          (list (check-true (match m-sum [(Some n) (== n 7)] [(None) #f])))))

    (it "product over Result builds the pair"
        (all-checks
          (list (check-true (match r-prod
                              [(Ok (Pair a b)) (and (== a 3) (== b 4))]
                              [(Err _) #f])))))))

(: test-main (IO Unit))
(define test-main (run-suite "applicative product/let+" suite))

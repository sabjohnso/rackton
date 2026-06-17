#lang racket/base

;; A length-indexed stack indexed by the BUILT-IN Nat (kind Nat, literals
;; + `+`), rather than a hand-rolled unary `(data Nat Z (S Nat))`.  This
;; is the payoff of unit-coefficient elimination in the Nat solver: a pop
;; on a provably-non-empty stack is TOTAL — the empty case is excluded at
;; the type level, so there is no `panic` arm.
;;
;;   - matching VPush against `(VStack (+ n 1))` solves `(+ m 1) ~ (+ n 1)`
;;     (two unknowns) by `m := n`;
;;   - VEmpty : `(VStack 0)` is impossible against `(VStack (+ n 1))`
;;     because `0 ~ (+ n 1)` has no Nat solution, so the match is
;;     exhaustive with the VPush arm alone.

(require rackunit
         "../main.rkt")

(rackton
  (data (VStack n)
    (VEmpty : (VStack 0))
    (VPush  : (-> Integer (VStack n) (VStack (+ n 1)))))

  ;; Total pop on a non-empty (depth ≥ 1) stack: only VPush is reachable,
  ;; so this single-arm match is exhaustive — no VEmpty / panic case.
  (: vtop (-> (VStack (+ n 1)) Integer))
  (define (vtop s)
    (match s
      [(VPush x _) x]))

  (: vpop (-> (VStack (+ n 1)) (VStack n)))
  (define (vpop s)
    (match s
      [(VPush _ rest) rest]))

  ;; Build (VStack 2), peek, pop to (VStack 1), peek again.
  (: s2 (VStack 2))
  (define s2 (VPush 10 (VPush 20 VEmpty)))

  (: top2 Integer) (define top2 (vtop s2))         ; 10
  (: top1 Integer) (define top1 (vtop (vpop s2))))  ; 20

(test-case "a Nat-indexed stack pops totally (no panic arm)"
  (check-equal? top2 10)
  (check-equal? top1 20))

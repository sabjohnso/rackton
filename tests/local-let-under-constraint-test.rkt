#lang racket/base

;; A polymorphic function with a class constraint (`(Eq a) =>`) whose
;; body contains an inner `let`/named-`let` that uses the constrained
;; method must type-check.  The declared signature skolemizes its
;; constraints into rigid givens (`(Eq $skolem.a)`); those givens are
;; in scope throughout the body, so an inner binding generalized
;; mid-body may assume them.  Before the fix, the inner generalization
;; ran with an empty hypothesis set and rejected the ground skolem
;; constraint as "no instance for (Eq a)", even though the signature
;; was valid (and was itself produced by Rackton's own tooling).

(require rackunit
         "../main.rkt")

;; ----- Piece 1: single declared def, inner named-let -----------

(rackton
  ;; `assoc` over an association list, exactly as the signature
  ;; printer emits it: an `(Eq a)` context discharged inside an inner
  ;; `let loop`.
  (: assoc (All (a b) ((Eq a) => (-> a (-> (List (Pair a b)) (Maybe (Pair a b)))))))
  (define (assoc key xs)
    (let loop ([xs xs])
      (match xs
        [(Cons (Pair k v) _) #:when (== k key) (Some (Pair k v))]
        [(Cons _ xs) (loop xs)]
        [Nil None])))

  ;; Call it at a concrete element type so the runtime dict actually
  ;; threads through the inner loop.
  (: table (List (Pair Integer String)))
  (define table
    (Cons (Pair 1 "one")
          (Cons (Pair 2 "two")
                (Cons (Pair 3 "three") Nil))))

  (: hit (Maybe (Pair Integer String)))
  (define hit (assoc 2 table))

  (: miss (Maybe (Pair Integer String)))
  (define miss (assoc 9 table))

  (provide hit miss))

(test-case "inner let under a constraint: found"
  (check-equal? hit (Some (Pair 2 "two"))))

(test-case "inner let under a constraint: not found"
  (check-equal? miss None))

;; ----- Piece 2: minimal repro, inner loop closes over outer var ---

(rackton
  (: same? (All (a) ((Eq a) => (-> a (-> a Boolean)))))
  (define (same? x y)
    (let loop ([z y]) (== z x)))

  (: t-true Boolean)
  (define t-true (same? 5 5))
  (: t-false Boolean)
  (define t-false (same? 5 6))

  (provide t-true t-false))

(test-case "inner loop closing over a constrained outer var: equal"
  (check-true t-true))

(test-case "inner loop closing over a constrained outer var: unequal"
  (check-false t-false))

;; ----- Piece 3: mutually-recursive declared defs ------------------

(rackton
  ;; `g` carries the constraint and the inner loop; `h` is in the same
  ;; declared SCC and calls `g`.  Both are processed by the declared
  ;; path, so each must see its own skolem givens.
  (: g (All (a) ((Eq a) => (-> a (-> a Boolean)))))
  (define (g x y)
    (let loop ([z y]) (== z x)))

  (: h (All (a) ((Eq a) => (-> a (-> a Boolean)))))
  (define (h x y)
    (if (g x y) #t (g y x)))

  (: r-mutual Boolean)
  (define r-mutual (h 4 4))

  (provide r-mutual))

(test-case "mutually-recursive declared defs with an inner loop"
  (check-true r-mutual))

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
         (for-syntax racket/base)
         "../main.rkt")

;; Expand a rackton block at runtime in this namespace, so a
;; type-check failure raises where `check-not-exn` can observe it.
;; Used for the law case, which type-checks but yields no runtime
;; value to assert against.
(define-syntax-rule (compile-rackton form ...)
  (eval #'(rackton form ...)
        (variable-reference->namespace (#%variable-reference))))

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

;; ----- Piece 4: existential `open` body ---------------------------
;; The packed `(Eq a)` context is a given inside the `open` body, so an
;; inner loop generalized there may assume it.

(rackton
  (define-alias EqPair (Exists (a) ((Eq a) => (Pair a a))))

  (: both-same (-> EqPair Boolean))
  (define (both-same d)
    (open d (a p)
      (match p
        [(Pair x y) (let loop ([z x]) (== z y))])))

  (: o-true Boolean)
  (define o-true (both-same (ann (Pair 7 7) EqPair)))
  (: o-false Boolean)
  (define o-false (both-same (ann (Pair 7 8) EqPair)))

  (provide o-true o-false))

(test-case "inner loop under an existential open's packed Eq: equal"
  (check-true o-true))

(test-case "inner loop under an existential open's packed Eq: unequal"
  (check-false o-false))

;; ----- Piece 5: existential-constructor `match` arm ---------------
;; A data constructor that packs `#:where (Eq a)` brings that given into
;; the matching arm; an inner loop in the arm may assume it.

(rackton
  (data EqBox
    (PackEq #:forall (a) #:where (Eq a) (Pair a a)))

  (: box-same (-> EqBox Boolean))
  (define (box-same e)
    (match e
      [(PackEq (Pair x y)) (let loop ([z x]) (== z y))]))

  (: m-true Boolean)
  (define m-true (box-same (PackEq (Pair 3 3))))
  (: m-false Boolean)
  (define m-false (box-same (PackEq (Pair 3 4))))

  (provide m-true m-false))

(test-case "inner loop under an existential-ctor arm's packed Eq: equal"
  (check-true m-true))

(test-case "inner loop under an existential-ctor arm's packed Eq: unequal"
  (check-false m-false))

;; ----- Piece 6: `#:laws` body -------------------------------------
;; A law's `=>` context is skolemized as a given while the law body is
;; checked.  An inner loop in the law body may assume it.  The law
;; type-checks but produces no runtime value, so assert that the block
;; compiles.

(test-case "inner loop under a law's => context type-checks"
  (check-not-exn
   (lambda ()
     (compile-rackton
      (protocol (MySemi a)
        (: combine (-> a (-> a a)))
        #:laws
          ([self-same ((Eq a) =>
            (All ([x : a])
              (let loop ([z x]) (== z x))))]))))))

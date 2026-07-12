#lang rackton

;; Decomposing Num into an algebraic lattice (additive + multiplicative
;; magma/semigroup/unital-magma/monoid/loop/group/…).  These pin the
;; motivating behaviours before the lattice exists:
;;
;;   - `zero` / `one` are return-typed identity methods that resolve at
;;     the call site by the expected type, for BOTH Float (a commutative
;;     loop / commutative unital magma — non-associative) and Integer (an
;;     abelian group / commutative monoid).
;;
;; Polymorphic `sum` seeding from `zero` is a separate task and lives in
;; its own test.

(require "../unit.rkt")

;; ----- zero / one resolve by return type --------------------

(: fz Float)
(define fz zero)

(: iz Integer)
(define iz zero)

(: fo Float)
(define fo one)

(: io Integer)
(define io one)

;; zero is a two-sided additive identity; one is a multiplicative one.
(: id-add-float Float)
(define id-add-float (+ zero 5.0))

(: id-mul-int Integer)
(define id-mul-int (* one 7))

;; ----- zero / one across the rest of the tower --------------

(: rz Rational) (define rz zero)
(: ro Rational) (define ro one)
(: cz Complex)  (define cz zero)
(: co Complex)  (define co one)
(: xz ComplexExact) (define xz zero)
(: xo ComplexExact) (define xo one)

;; ----- a consumer of the Additive-Abelian-Group meet synonym ----
;; `gsum` needs `+` (from Additive-Magma) and `negate` (from
;; Additive-Loop), both supplied by the abelian-group synonym through
;; superclass entailment — this validates that the trimmed synonym
;; (Semigroup + Commutative-Loop) still reaches the deeper nodes.  It is
;; exercised at the exact types (Integer, Rational), which ARE abelian
;; groups.  (A bare return-typed `zero` in the body is deliberately
;; avoided: a user-written polymorphic body cannot yet resolve one — the
;; same needs-dict restriction that defers polymorphic `sum`.)
(: gsum ((Additive-Abelian-Group a) => (-> a (-> a a))))
(define (gsum x y) (+ x (negate (negate y))))

(: gsum-int Integer)
(define gsum-int (gsum 3 4))

(: gsum-rat Rational)
(define gsum-rat (gsum (make-rational 1 2) (make-rational 1 3)))

;; `gprod` needs `*` (from Multiplicative-Magma) supplied by the
;; multiplicative commutative-monoid synonym through superclass
;; entailment — the multiplicative-side counterpart to `gsum`.
(: gprod ((Multiplicative-Commutative-Monoid a) => (-> a (-> a a))))
(define (gprod x y) (* x y))

(: gprod-int Integer)
(define gprod-int (gprod 6 7))

;; `sum` seeds from the polymorphic `zero`, so it admits any additive
;; unital magma — Float as well as Integer — not just Integer.
(: float-sum Float)
(define float-sum (sum (Cons 1.0 (Cons 2.0 (Cons 3.0 Nil)))))

(: int-sum Integer)
(define int-sum (sum (Cons 1 (Cons 2 (Cons 3 Nil)))))

;; The empty fold returns the threaded seed directly, so it witnesses
;; that the elaborator prepended the CORRECT polymorphic `zero` (Float's
;; 0.0, not a stray Integer 0) — a non-empty list's inner `+` would mask
;; a wrong seed.
(: empty-float-sum Float)
(define empty-float-sum (sum (ann Nil (List Float))))

(: empty-int-sum Integer)
(define empty-int-sum (sum (ann Nil (List Integer))))

;; sum reaches an exact non-Integer tower member (Rational), backing the
;; "any additive unital magma" claim beyond Float/Integer.
(: rat-sum Rational)
(define rat-sum (sum (Cons (make-rational 1 2) (Cons (make-rational 1 3) Nil))))

;; A polymorphic wrapper keeps `a` open and re-calls `sum`, so the
;; resolved `zero` must thread through a second needs-dict boundary
;; rather than resolve at a leaf literal.
(: total ((Additive-Unital-Magma a) (Foldable t) => (-> (t a) a)))
(define (total xs) (sum xs))

(: multihop-int Integer)
(define multihop-int (total (Cons 4 (Cons 5 Nil))))

(: multihop-float Float)
(define multihop-float (total (Cons 1.5 (Cons 2.5 Nil))))

(: suite (List Test))
(define suite
  (list
    (it "zero resolves at Float and Integer"
        (all-checks
          (list (check-equal? fz 0.0)
                (check-equal? iz 0))))
    (it "one resolves at Float and Integer"
        (all-checks
          (list (check-equal? fo 1.0)
                (check-equal? io 1))))
    (it "identities are neutral"
        (all-checks
          (list (check-equal? id-add-float 5.0)
                (check-equal? id-mul-int 7))))
    (it "zero / one resolve across the numeric tower"
        (all-checks
          (list (check-equal? rz (make-rational 0 1))
                (check-equal? ro (make-rational 1 1))
                (check-equal? cz (make-complex 0.0 0.0))
                (check-equal? co (make-complex 1.0 0.0))
                (check-equal? xz (make-complex-exact 0 0))
                (check-equal? xo (make-complex-exact 1 0)))))
    (it "meet synonyms drive their operations via entailment"
        (all-checks
          (list (check-equal? gsum-int 7)
                (check-equal? gsum-rat (make-rational 5 6))
                (check-equal? gprod-int 42))))
    (it "sum seeds from zero, admitting Float and Integer"
        (all-checks
          (list (check-equal? float-sum 6.0)
                (check-equal? int-sum 6)
                (check-equal? empty-float-sum 0.0)
                (check-equal? empty-int-sum 0)
                (check-equal? rat-sum (make-rational 5 6)))))
    (it "sum's zero threads through a polymorphic wrapper (multi-hop)"
        (all-checks
          (list (check-equal? multihop-int 9)
                (check-equal? multihop-float 4.0))))))

(: test-main (IO Unit))
(define test-main (run-suite "additive lattice" suite))

#lang rackton

;; Tests for rackton/mono.  Increment 1: the order-theory protocols and
;; their algebraic laws, verified on the shipped Boolean instance via the
;; auto-generated <Class>-laws bundles, plus a direct property test for the
;; bounded law (whose return-typed `bot` keeps it out of the auto-bundle).

(require "../unit.rkt"
         "../mono.rkt"
         rackton/data/set)

(: gen-bool (Gen Boolean))
(define gen-bool (fmap (lambda (n) (== n 0)) (int-range 0 1)))

(: gen-bb (Gen (Pair Boolean Boolean)))
(define gen-bb (gen-pair gen-bool gen-bool))

;; ----- increment 1: order-theory laws on Boolean --------------------
(: order-laws Test)
(define order-laws
  (group-of "rackton/mono — order-theory laws (Boolean)"
            (list
              (Poset-laws gen-bool)
              (JoinSemilattice-laws gen-bool)
              (it-prop "BoundedJoinSemilattice bot-identity: bot lub x = x"
                       (for-all gen-bool (lambda (x) (== (lub bot x) x)))))))

;; ----- increment 2: the Mono combinators, EXTENSIONALLY (Mono is a -----
;; sealed function wrapper with no Eq, so compare outputs on the carrier).
;; Concrete monos over Boolean: identity, the two constants, and `or-with`
;; (lub against a fixed value) built from the closed combinators.
(: or-true (Mono Boolean Boolean))
(define or-true (mono-join mono-id (mono-const #t)))   ; always #t
(: or-false (Mono Boolean Boolean))
(define or-false (mono-join mono-id (mono-const #f)))  ; identity (x ∨ ⊥)

(: combinator-laws Test)
(define combinator-laws
  (group-of "rackton/mono — Mono combinators (Boolean)"
            (list
              (it-prop "comp left identity: id . f = f"
                       (for-all gen-bool (lambda (x)
                                           (== (app-mono (mono-comp mono-id or-false) x) (app-mono or-false x)))))
              (it-prop "comp right identity: f . id = f"
                       (for-all gen-bool (lambda (x)
                                           (== (app-mono (mono-comp or-false mono-id) x) (app-mono or-false x)))))
              (it-prop "comp associativity: (h.g).f = h.(g.f)"
                       (for-all gen-bool (lambda (x)
                                           (== (app-mono (mono-comp (mono-comp or-true or-false) mono-id) x)
                                               (app-mono (mono-comp or-true (mono-comp or-false mono-id)) x)))))
              (it-prop "mono-join is pointwise lub"
                       (for-all gen-bool (lambda (x)
                                           (== (app-mono (mono-join or-false (mono-const #t)) x) (lub x #t)))))
              (it-prop "mono-pair / mono-fst / mono-snd project"
                       (for-all gen-bool (lambda (x)
                                           (and (== (app-mono (mono-comp mono-fst (mono-pair or-false or-true)) x)
                                                    (app-mono or-false x))
                                                (== (app-mono (mono-comp mono-snd (mono-pair or-false or-true)) x)
                                                    (app-mono or-true x))))))
              (it-prop "mono-fst / mono-snd on a pair"
                       (for-all gen-bb (lambda (p) (match p [(Pair a b)
                                                             (and (== (app-mono mono-fst (Pair a b)) a)
                                                                  (== (app-mono mono-snd (Pair a b)) b))]))))
              (it-prop "every Mono is monotone: x <= y => f x <= f y  (f = or-true)"
                       (for-all gen-bb (lambda (p) (match p [(Pair x y)
                                                             (if (leq x y) (leq (app-mono or-true x) (app-mono or-true y)) #t)])))))))

;; ----- increment 3: the payoff — transitive closure as a monotone -----
;; least fixpoint (a one-rule Datalog program).  The carrier is a NOMINAL
;; relation type (a foreign Set carries no dispatch tag, so wrap it).
(data Rel (MkRel (Set (Pair Integer Integer))))
(: edges (-> Rel (Set (Pair Integer Integer))))
(define (edges r) (match r [(MkRel s) s]))
(: rel (-> (List (Pair Integer Integer)) Rel))
(define (rel xs) (MkRel (set-from-list xs)))

;; relations ordered by inclusion: ⊑ = ⊆, ⊔ = ∪, ⊥ = ∅
(instance (Eq Rel)
  (define (== a b)
    (if (set-subset? (edges a) (edges b)) (set-subset? (edges b) (edges a)) #f)))
(instance (Poset Rel)
  (define (leq a b) (set-subset? (edges a) (edges b))))
(instance (JoinSemilattice Rel)
  (define (lub a b) (MkRel (set-union (edges a) (edges b)))))
(instance (BoundedJoinSemilattice Rel)
  (define bot (MkRel empty-set)))

;; relational composition with a fixed base — monotone in R; the trusted leaf
(: compose-pairs (-> (List (Pair Integer Integer))
                     (List (Pair Integer Integer))
                     (List (Pair Integer Integer))))
(define (compose-pairs bs rs)
  (foldr (lambda (b acc)
           (match b [(Pair x y)
                     (foldr (lambda (e acc2)
                              (match e [(Pair y2 z) (if (== y y2) (Cons (Pair x z) acc2) acc2)]))
                            acc rs)]))
         Nil bs))
(: compose-rel (-> Rel Rel Rel))
(define (compose-rel base r)
  (MkRel (set-from-list (compose-pairs (set-to-list (edges base))
                                       (set-to-list (edges r))))))

(: base Rel)
(define base (rel (list (Pair 1 2) (Pair 2 3) (Pair 3 4))))
;; step R = base ⊔ compose(base, R) — closed combinators + one trusted leaf
(: step (Mono Rel Rel))
(define step (mono-join (mono-const base) (unsafe-mono (compose-rel base))))
(: tc Rel)
(define tc (mono-fix step))
(: expected Rel)
(define expected
  (rel (list (Pair 1 2) (Pair 2 3) (Pair 3 4)
             (Pair 1 3) (Pair 2 4) (Pair 1 4))))

(: datalog Test)
(define datalog
  (group-of "rackton/mono — transitive closure (Datalog payoff)"
            (list
              (it "mono-fix computes the transitive closure"
                  (check-true (== tc expected)))
              (it "mono-fix result is a fixed point of the step"
                  (check-true (== (app-mono step tc) tc)))
              (it "mono-fix/fuel agrees given enough fuel"
                  (check-true (match (mono-fix/fuel 100 step)
                                [(Some r) (== r expected)] [None #f])))
              (it "mono-fix/fuel returns None when starved"
                  (check-true (match (mono-fix/fuel 1 step)
                                [(Some r) #f] [None #t])))
              ;; mono-fix-from: resuming from ⊥ matches mono-fix; resuming from the
              ;; answer itself is idempotent (the differential refinement is correct)
              (it "mono-fix-from bot agrees with mono-fix"
                  (check-true (== (mono-fix-from (MkRel empty-set) step) tc)))
              (it "mono-fix-from the fixpoint is idempotent"
                  (check-true (== (mono-fix-from tc step) tc))))))

(: suite Test)
(define suite (group-of "rackton/mono" (list order-laws combinator-laws datalog)))

(: test-main (IO Unit))
(define test-main (run-suite-tree suite))

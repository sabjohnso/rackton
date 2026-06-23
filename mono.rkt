#lang rackton

;; rackton/mono — Datafun-style MONOTONICITY as a library.
;;
;; Tenets / what this provides:
;;   - The order-theory protocols a monotone computation needs — `Poset`,
;;     `JoinSemilattice`, `BoundedJoinSemilattice` — each with algebraic
;;     `#:laws` (so an instance is machine-checkable against them) and a
;;     runnable `<Class>-laws` bundle for property-testing a user's own
;;     instance.
;;   - (added in later layers of this file) a SEALED monotone-map type
;;     `Mono a b` whose closed combinator set makes every constructible map
;;     monotone BY CONSTRUCTION, plus `mono-fix`, a least-fixpoint over a
;;     bounded join-semilattice (the typed-Datalog / dataflow payoff).
;;
;; The grade (monotonicity) rides the VALUE'S TYPE and a closed algebra,
;; not the typing context — so ordinary Hindley–Milner inference carries it
;; with no engine change.  See UserLevelModalities.org for the why.

(require "unit.rkt")

(provide (protocol-out Poset)
         (protocol-out JoinSemilattice)
         (protocol-out BoundedJoinSemilattice)
         ;; Auto-generated runnable law bundles.  BoundedJoinSemilattice's
         ;; only law uses the return-typed `bot`, which the bundle generator
         ;; skips (as for Monoid's `mempty`), so no bundle is emitted for it
         ;; — its `#:laws` still type-check at definition, and bot-identity
         ;; is property-tested directly in tests/mono-test.rkt.
         Poset-laws
         JoinSemilattice-laws
         ;; the sealed monotone arrow + its closed combinator algebra
         (data-out Mono)
         run-mono app-mono
         mono-id mono-comp mono-const mono-fst mono-snd mono-pair mono-join
         unsafe-mono
         mono-fix mono-fix/fuel)

;; ===== order-theory protocols =========================================

;; A partial order: a reflexive, antisymmetric, transitive `leq`.
(protocol (Poset a)
  (#:requires (Eq a))
  (: leq (-> a a Boolean))
  #:laws
    ([reflexive     (All ([x : a]) (leq x x))]
     [antisymmetric (All ([x : a] [y : a])
                      (if (leq x y) (if (leq y x) (== x y) #t) #t))]
     [transitive    (All ([x : a] [y : a] [z : a])
                      (if (leq x y) (if (leq y z) (leq x z) #t) #t))]))

;; A join-semilattice: a commutative, associative, idempotent least-upper-
;; bound `lub`, consistent with the order (x ⊑ x ⊔ y).
(protocol (JoinSemilattice a)
  (#:requires (Poset a))
  (: lub (-> a a a))
  #:laws
    ([commutative      (All ([x : a] [y : a]) (== (lub x y) (lub y x)))]
     [associative      (All ([x : a] [y : a] [z : a])
                         (== (lub (lub x y) z) (lub x (lub y z))))]
     [idempotent       (All ([x : a]) (== (lub x x) x))]
     [join-upper-bound (All ([x : a] [y : a]) (leq x (lub x y)))]))

;; A bounded join-semilattice: a least element `bot` (the ⊕-identity), the
;; seed for least-fixpoint iteration.  `bot` is return-typed (like `mempty`)
;; — re-exportable across modules as of Rackton 1.1.
(protocol (BoundedJoinSemilattice a)
  (#:requires (JoinSemilattice a))
  (: bot a)
  #:laws
    ([bot-identity (All ([x : a]) (== (lub bot x) x))]))

;; ===== a base instance: Boolean is the 2-point lattice ================
;; ⊑ is implication (false ⊑ true), ⊔ is ∨, ⊥ is false.

(instance (Poset Boolean)
  (define (leq a b) (if a b #t)))
(instance (JoinSemilattice Boolean)
  (define (lub a b) (if a #t b)))
(instance (BoundedJoinSemilattice Boolean)
  (define bot #f))

;; ===== the sealed monotone arrow ======================================
;; `Mono a b` is a monotone map.  Its constructor is `#:abstract`, so the
;; only way to obtain one is the closed combinator set below — each a
;; monotone map that ALSO preserves its arguments' monotonicity, so every
;; constructible `Mono` is monotone BY CONSTRUCTION.

(data (Mono a b) (MkMono (-> a b)) #:abstract)

(: run-mono (-> (Mono a b) (-> a b)))           ; the underlying function
(define (run-mono m) (match m [(MkMono f) f]))
(: app-mono (-> (Mono a b) a b))                ; apply a monotone map
(define (app-mono m x) ((run-mono m) x))

;; the closed set of monotone-map formers
(: mono-id (Mono a a))
(define mono-id (MkMono (lambda (x) x)))

(: mono-comp (-> (Mono b c) (Mono a b) (Mono a c)))         ; mono ∘ mono = mono
(define (mono-comp g f) (MkMono (lambda (x) (app-mono g (app-mono f x)))))

(: mono-const (-> b (Mono a b)))                            ; constants are monotone
(define (mono-const y) (MkMono (lambda (z) y)))

(: mono-fst (Mono (Pair a b) a))                            ; projections are monotone
(define mono-fst (MkMono (lambda (p) (match p [(Pair x y) x]))))
(: mono-snd (Mono (Pair a b) b))
(define mono-snd (MkMono (lambda (p) (match p [(Pair x y) y]))))

(: mono-pair (-> (Mono a b) (Mono a c) (Mono a (Pair b c))))
(define (mono-pair f g) (MkMono (lambda (x) (Pair (app-mono f x) (app-mono g x)))))

(: mono-join ((JoinSemilattice s) => (-> (Mono a s) (Mono a s) (Mono a s))))
(define (mono-join f g) (MkMono (lambda (x) (lub (app-mono f x) (app-mono g x)))))

;; The one trusted escape: lift a primitive the library cannot prove
;; monotone.  UNSAFE — the caller asserts monotonicity; the type system does
;; not check it.  Reviewable precisely because every such lift is visible.
(: unsafe-mono (-> (-> a b) (Mono a b)))
(define (unsafe-mono f) (MkMono f))

;; ===== the payoff: least fixpoint of a monotone endomap ===============
;; Kleene iteration from ⊥.  Sound as the LEAST fixpoint because the map is
;; monotone BY CONSTRUCTION and ⊥ is the least element.  `mono-fix` assumes
;; the carrier satisfies the ascending-chain condition (e.g. it is finite)
;; so the ascending chain ⊥ ⊑ f ⊥ ⊑ f² ⊥ ⊑ … stabilizes; on a carrier
;; without that guarantee, use `mono-fix/fuel`, which bounds the iterations
;; and returns `None` if no fixpoint is reached in time.

;; iterate to a fixed point (stop when a step makes no change)
(: fix-iter ((Eq a) => (-> (-> a a) a a)))
(define (fix-iter step x)
  (if (== (step x) x) x (fix-iter step (step x))))

(: mono-fix ((BoundedJoinSemilattice a) => (-> (Mono a a) a)))
(define (mono-fix f) (fix-iter (run-mono f) bot))

;; fuel-bounded: `Some` the least fixpoint if reached within `fuel` steps,
;; else `None`.
(: fix-iter/fuel ((Eq a) => (-> Integer (-> a a) a (Maybe a))))
(define (fix-iter/fuel fuel step x)
  (if (<= fuel 0)
      None
      (if (== (step x) x) (Some x) (fix-iter/fuel (- fuel 1) step (step x)))))

(: mono-fix/fuel ((BoundedJoinSemilattice a) => (-> Integer (Mono a a) (Maybe a))))
(define (mono-fix/fuel fuel f) (fix-iter/fuel fuel (run-mono f) bot))

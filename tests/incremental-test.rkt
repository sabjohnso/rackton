#lang rackton

;; Tests for rackton/incremental.  `scan-mono` runs a guarded stream whose
;; every tick is a monotone least fixpoint: productive across ticks (the
;; Later-tail guard), terminating within a tick (mono-fix / ACC).

(require "../unit.rkt"
         rackton/mono
         rackton/temporal
         rackton/incremental
         rackton/data/set)

;; ===== the relation lattice (⊑ = ⊆, ⊔ = ∪, ⊥ = ∅) ====================
(data Rel (MkRel (Set (Pair Integer Integer))))
(: edges-of (-> Rel (Set (Pair Integer Integer))))
(define (edges-of r) (match r [(MkRel s) s]))
(: rel (-> (List (Pair Integer Integer)) Rel))
(define (rel xs) (MkRel (set-from-list xs)))

(instance (Eq Rel)
  (define (== a b)
    (if (set-subset? (edges-of a) (edges-of b)) (set-subset? (edges-of b) (edges-of a)) #f)))
(instance (Poset Rel)
  (define (leq a b) (set-subset? (edges-of a) (edges-of b))))
(instance (JoinSemilattice Rel)
  (define (lub a b) (MkRel (set-union (edges-of a) (edges-of b)))))
(instance (BoundedJoinSemilattice Rel)
  (define bot (MkRel empty-set)))

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
(: compose-with (-> Rel Rel Rel))
(define (compose-with base r)
  (MkRel (set-from-list (compose-pairs (set-to-list (edges-of base))
                                       (set-to-list (edges-of r))))))

;; the per-tick step: fold the new batch into the running closure, then the
;; new value is the transitive closure of that — a monotone least fixpoint.
(: closure-step (-> Rel Rel (Mono Rel Rel)))
(define (closure-step prev batch)
  (mono-join (mono-const (lub prev batch))
             (unsafe-mono (compose-with (lub prev batch)))))

;; edges arrive over time, then nothing
(: batches (Signal Rel))
(define batches
  (SigCons (rel (list (Pair 1 2)))
           (next (SigCons (rel (list (Pair 2 3)))
                          (next (SigCons (rel (list (Pair 3 4)))
                                         (next (sig-repeat (MkRel empty-set)))))))))

(: closures (Signal Rel))
(define closures (scan-mono closure-step (MkRel empty-set) batches))

(: sizes (List Integer))
(define sizes (fmap (lambda (r) (set-size (edges-of r))) (sig-take 4 closures)))

;; ===== a tiny second lattice: Boolean latch (running OR) =============
(: bools (Signal Boolean))
(define bools (SigCons #f (next (SigCons #t (next (sig-repeat #f))))))

;; step ignores the fixpoint structure: the new value is just prev ⊔ input,
;; so the output latches to #t and stays there.
(: latch (Signal Boolean))
(define latch (scan-mono (lambda (p i) (mono-const (lub p i))) #f bools))

;; ===== differential: resume each tick from the previous output =======
(: closures-diff (Signal Rel))
(define closures-diff (scan-mono-diff closure-step (MkRel empty-set) batches))
(: sizes-diff (List Integer))
(define sizes-diff (fmap (lambda (r) (set-size (edges-of r))) (sig-take 4 closures-diff)))

;; count iterations a Mono takes to reach its fixpoint from a given seed
(: count-iter (-> (Mono Rel Rel) Rel Integer))
(define (count-iter f x)
  (if (== (app-mono f x) x) 0 (+ 1 (count-iter f (app-mono f x)))))

(: full-base Rel)
(define full-base (rel (list (Pair 1 2) (Pair 2 3) (Pair 3 4))))
(: full-map (Mono Rel Rel))
(define full-map (mono-join (mono-const full-base) (unsafe-mono (compose-with full-base))))

;; ===== assertions ====================================================
(: suite Test)
(define suite
  (group-of "rackton/incremental — scan-mono (monotone x temporal)"
            (list
              (it "incremental transitive closure: sizes 1,3,6,6"
                  (check-equal? sizes (list 1 3 6 6)))
              (it "sig-take terminates: each adv is a terminating mono-fix (ACC)"
                  (check-equal? (length (sig-take 20 closures)) 20))
              (it "Boolean latch (running OR): #f then latched #t"
                  (check-equal? (sig-take 4 latch) (list #f #t #t #t)))
              ;; the differential refinement: same answer, less work
              (it "scan-mono-diff agrees with scan-mono (same sizes)"
                  (check-equal? sizes-diff (list 1 3 6 6)))
              (it "mono-fix-from a lower bound reaches the same fixpoint"
                  (check-true (== (mono-fix-from full-base full-map) (mono-fix full-map))))
              (it "resuming from a closer seed does FEWER iterations"
                  (check-true (< (count-iter full-map full-base)
                                 (count-iter full-map (MkRel empty-set))))))))

(: test-main (IO Unit))
(define test-main (run-suite-tree suite))

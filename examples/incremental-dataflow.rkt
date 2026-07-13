#lang rackton

;; incremental-dataflow.rkt — monotone x temporal, with rackton/incremental.
;;
;; Edges arrive over time as a guarded `Signal` of batches; `scan-mono` maps
;; them to a `Signal` of transitive closures, recomputing each as a MONOTONE
;; LEAST FIXPOINT (`mono-fix`).  Two well-founded guarantees compose:
;;
;;   - across ticks: the Later-tail makes the stream productive;
;;   - within a tick: mono-fix terminates by the ascending-chain condition,
;;     so every step yields its value finitely — incremental dataflow.
;;
;; Run it with `racket examples/incremental-dataflow.rkt`.

(require rackton/mono
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

;; per-tick step: fold the new batch into the running closure, then the new
;; value is the transitive closure of that — a monotone least fixpoint.
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

;; ===== output ======================================================
(: ints->str (-> (List Integer) String))
(define (ints->str xs)
  (foldr (lambda (n acc) (string-append (integer->string n) (string-append " " acc))) "" xs))

(: main (IO Unit))
(define main (let& ([_ (println "Incremental transitive closure over time (rackton/incremental)")]
                    [_ (println "")]
                    [_ (println "Edges arrive:  tick0 +{1->2}, tick1 +{2->3}, tick2 +{3->4}, then none.")]
                    [_ (println "Each tick's closure is a monotone least fixpoint (mono-fix), recomputed")]
                    [_ (println "as the graph grows; mono-fix terminates by ACC, so every step is finite.")]
                    [_ (println "")]
                    [_ (println (string-append "closure sizes per tick:  " (ints->str sizes)))]
                    [_ (println "")]
                    [_ (println "(1 -> 3 -> 6 as paths accumulate, then steady once edges stop arriving.)")])
               (pure Unit)))

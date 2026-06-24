#lang rackton

;; rackton/incremental — the monotone × temporal bridge.
;;
;; A guarded stream whose every tick is a MONOTONE LEAST FIXPOINT —
;; incremental / differential dataflow.  `scan-mono` threads a state through
;; a `Signal`, computing each tick's value as `mono-fix` of a monotone map
;; built from the previous value and the current input.  Two well-founded
;; guarantees compose, with no overlap:
;;
;;   - ACROSS ticks, productivity is the guard: the `Signal`'s tail is a
;;     `Later`, so `sig-take n` forces one tick at a time and its outer `n`
;;     descends.
;;   - WITHIN a tick, termination is the ascending-chain condition: each
;;     emitted value is a `mono-fix`, which stabilizes on an ACC (e.g.
;;     finite) lattice — so every `adv` yields its SigCons FINITELY, by
;;     theorem, not by trust.
;;
;; Neither core module depends on the other; this is the only bridge.

(require rackton/mono
         rackton/temporal)

(provide scan-mono scan-mono-diff)

;; From the previous output and the current input, `step` builds a monotone
;; endomap whose least fixpoint is this tick's output; that output is then
;; threaded forward as the next `seed`.  (mono-fix iterates from ⊥; the
;; previous output feeds the MAP, e.g. an accumulation `lub prev input`.)
(: scan-mono ((BoundedJoinSemilattice s) =>
   (-> (-> s in (Mono s s)) s (Signal in) (Signal s))))
(define (scan-mono step seed ins)
  ;; compute this tick's fixpoint ONCE, then emit it and thread it forward
  (scan-mono/emit step (mono-fix (step seed (sig-head ins))) (sig-tail ins)))

;; emit an already-computed output, with the rest of the inputs deferred
(: scan-mono/emit ((BoundedJoinSemilattice s) =>
   (-> (-> s in (Mono s s)) s (Later (Signal in)) (Signal s))))
(define (scan-mono/emit step out later-ins)
  (SigCons out
           (map-later (lambda (rest) (scan-mono step out rest)) later-ins)))

;; DIFFERENTIAL variant: resume each tick's fixpoint from the PREVIOUS output
;; (`mono-fix-from`) instead of from ⊥, doing incremental rather than
;; from-scratch work.  Sound — gives the same stream as `scan-mono` — WHEN
;; the per-tick maps grow monotonically (each tick's map ⊒ the last), since
;; then the previous output is below the new least fixpoint.  That holds for
;; accumulating dataflow (state folded forward).  The caller's initial `seed`
;; must be a valid lower bound (e.g. ⊥).
(: scan-mono-diff ((Eq s) =>
   (-> (-> s in (Mono s s)) s (Signal in) (Signal s))))
(define (scan-mono-diff step prev ins)
  (scan-diff/emit step (mono-fix-from prev (step prev (sig-head ins))) (sig-tail ins)))

(: scan-diff/emit ((Eq s) =>
   (-> (-> s in (Mono s s)) s (Later (Signal in)) (Signal s))))
(define (scan-diff/emit step out later-ins)
  (SigCons out
           (map-later (lambda (rest) (scan-mono-diff step out rest)) later-ins)))

#lang racket/base

;; Phase 1: a pure, seeded, splittable PRNG.  Property testing needs
;; reproducible randomness — no IO — so a failing case can be replayed
;; from a printed seed.  We verify three things: determinism (same seed
;; ⇒ same draw), that `split-seed` yields two independent sub-seeds, and
;; that `seed-int-range` stays within its inclusive bounds.

(require rackunit
         "../main.rkt")

(rackton
  (require "../unit/prng.rkt")

  (: in-0-9? (-> Integer Boolean))
  (define (in-0-9? v) (if (>= v 0) (<= v 9) #f))

  ;; Determinism: two independent draws from the same starting seed.
  (: draw-a Integer)
  (define draw-a (seed-int-range (seed-from 42) 0 100))
  (: draw-b Integer)
  (define draw-b (seed-int-range (seed-from 42) 0 100))
  (: deterministic Boolean)
  (define deterministic (== draw-a draw-b))

  ;; Split yields two distinct sub-seeds.
  (: split-distinct Boolean)
  (define split-distinct
    (match (split-seed (seed-from 1))
      [(MkPair sa sb) (/= (seed-value sa) (seed-value sb))]))

  ;; Range bound across three successive seeds.
  (: all-in-range Boolean)
  (define all-in-range
    (let ([s0 (seed-from 1)])
      (let ([s1 (next-seed s0)])
        (let ([s2 (next-seed s1)])
          (if (in-0-9? (seed-int-range s0 0 9))
              (if (in-0-9? (seed-int-range s1 0 9))
                  (in-0-9? (seed-int-range s2 0 9))
                  #f)
              #f)))))

  ;; Successive seeds actually move (the generator advances).
  (: advances Boolean)
  (define advances
    (let ([s0 (seed-from 7)])
      (/= (seed-value s0) (seed-value (next-seed s0))))))

(test-case "same seed produces the same draw"
  (check-true deterministic))

(test-case "split-seed yields two distinct sub-seeds"
  (check-true split-distinct))

(test-case "seed-int-range stays within [lo, hi]"
  (check-true all-in-range))

(test-case "next-seed advances the generator"
  (check-true advances))

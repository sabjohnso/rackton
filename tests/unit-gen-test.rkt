#lang racket/base

;; Phase 2: the generator with integrated shrinking.
;;
;; A `Gen a` is `size -> seed -> Tree a`, where `Tree a` carries a
;; generated value plus a lazy stream of progressively smaller shrink
;; candidates.  `fmap`/`flatmap` map the value AND its shrinks together
;; (Hedgehog-style interleaving), so every generator shrinks for free.
;;
;; We verify: determinism, range bounds, that shrink children move
;; toward the low bound, and that the Functor/Monad instances (and
;; `do`-notation) run over `Gen`.

(require rackunit
         "../main.rkt")

(rackton
  (require "../unit/gen.rkt")
  (require "../unit/prng.rkt")
  (require "../unit/lazy.rkt")

  ;; Same size+seed ⇒ identical draw.
  (: draw-det Boolean)
  (define draw-det
    (== (tree-value (gen-tree (int-range 0 10) 0 (seed-from 99)))
        (tree-value (gen-tree (int-range 0 10) 0 (seed-from 99)))))

  ;; Draw stays within the inclusive range.
  (: draw-in-range Boolean)
  (define draw-in-range
    (let ([v (tree-value (gen-tree (int-range 0 10) 0 (seed-from 99)))])
      (if (>= v 0) (<= v 10) #f)))

  ;; The most-aggressive shrink child of a non-minimal value is the low
  ;; bound (0).  If the draw is already 0, that is vacuously fine.
  (: first-shrink-is-lo Boolean)
  (define first-shrink-is-lo
    (let ([t (gen-tree (int-range 0 10) 0 (seed-from 99))])
      (if (== (tree-value t) 0)
          #t
          (match (tree-children t)
            [(SNil)      #f]
            [(SCons c _) (== (tree-value c) 0)]))))

  ;; Functor: int-range 0 0 always yields 0; +100 ⇒ 100.
  (: mapped Integer)
  (define mapped
    (tree-value (gen-tree (fmap (lambda (x) (+ x 100)) (int-range 0 0))
                         0 (seed-from 1))))

  ;; Monad / do-notation: 5 + 3 = 8 (both generators are point ranges,
  ;; so the result is deterministic regardless of seed).
  (: bound Integer)
  (define bound
    (tree-value
     (gen-tree (let& ([x (int-range 5 5)]
                      [y (int-range 3 3)])
                 (constant (+ x y)))
              0 (seed-from 2)))))

(test-case "same size+seed produces the same draw"
  (check-true draw-det))

(test-case "draw stays within [lo, hi]"
  (check-true draw-in-range))

(test-case "shrink moves toward the low bound"
  (check-true first-shrink-is-lo))

(test-case "fmap maps the generated value"
  (check-equal? mapped 100))

(test-case "do-notation / flatmap runs over Gen"
  (check-equal? bound 8))

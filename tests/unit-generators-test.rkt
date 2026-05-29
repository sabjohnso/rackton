#lang racket/base

;; Phase 3: default and compound generators, exercised from a SEPARATE
;; module (this test file) that requires the library — proving the
;; cross-module path works.
;;
;; The composition relies on `flatmap` over `Gen`, which dispatches on
;; its generator argument (a positional method) through the global
;; runtime table, so it resolves across modules.  (A return-typed
;; `Arbitrary` class member would NOT — that is the documented v1
;; limitation, which is why property testing uses these explicit
;; generators rather than type-directed `arbitrary`.)

(require rackunit
         "../main.rkt")

(rackton
  (require "../unit/gen.rkt")
  (require "../unit/prng.rkt")
  (require "../unit/lazy.rkt")

  ;; gen-pair composes two component generators.
  (: pair-ok Boolean)
  (define pair-ok
    (match (tree-value (gen-tree (gen-pair (int-range 1 1) (int-range 2 2))
                                 0 (seed-from 5)))
      [(MkPair a b) (if (== a 1) (== b 2) #f)]))

  ;; replicate-gen yields exactly n elements …
  (: rep-len Integer)
  (define rep-len
    (length (tree-value (gen-tree (replicate-gen 4 (int-range 7 7))
                                  0 (seed-from 9)))))

  ;; … each drawn from the element generator (4 × 7 = 28).
  (: rep-sum Integer)
  (define rep-sum
    (sum (tree-value (gen-tree (replicate-gen 4 (int-range 7 7))
                               0 (seed-from 9)))))

  ;; gen-list length stays within its 0–8 bound.
  (: list-bounded Boolean)
  (define list-bounded
    (let ([n (length (tree-value (gen-tree (gen-list gen-boolean)
                                           3 (seed-from 11))))])
      (if (>= n 0) (<= n 8) #f))))

(test-case "gen-pair composes two generators across a module boundary"
  (check-true pair-ok))

(test-case "replicate-gen yields exactly n elements"
  (check-equal? rep-len 4))

(test-case "replicate-gen draws from the element generator"
  (check-equal? rep-sum 28))

(test-case "gen-list length stays within bounds"
  (check-true list-bounded))

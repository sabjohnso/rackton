#lang rackton

;; rackton/unit — properties and shrinking.
;;
;; A `Property` is an opaque "run N cases from a seed and report"
;; computation; the element type it quantifies over is hidden inside the
;; closure (only its `Show` rendering escapes, as a String).  `run-property`
;; drives it.  On the first failing case the shrink loop descends into the
;; smallest still-failing child of the shrink tree until none remains —
;; yielding the minimal counterexample.  Generation is pure and seeded,
;; so the reported start seed replays the failure exactly.
;;
;; Public API: Property, PropOutcome (PropPassed / PropFailed),
;; for-all-gen, run-property.

(require "lazy.rkt"
         "gen.rkt"
         "prng.rkt")

(provide (data-out Property)
         (data-out PropOutcome)
         for-all-gen
         for-all
         run-property
         ;; Re-export the generator surface so a consumer can `(require
         ;; "property.rkt")` alone.  Instances escape via a single path
         ;; (this module imports `gen` once), avoiding the import-diamond
         ;; that module-level instance coherence would otherwise reject.
         (data-out Gen)
         (data-out Tree)
         run-gen
         gen-tree
         tree-value
         tree-children
         tree-map
         tree-bind
         constant
         int-range
         bool
         gen-integer
         gen-boolean
         gen-pair
         replicate-gen
         gen-list
         element-of
         gen-string)

;; numTests -> startSeed -> outcome
(data Property (MkProperty (-> Integer (-> Integer PropOutcome))))

(data PropOutcome
  (PropPassed Integer)            ;; number of cases that passed
  (PropFailed String Integer))    ;; shown minimal counterexample, start seed

;; ----- Shrinking (generic; no Show needed) --------------------------

;; The first child whose value still FAILS the predicate (pred ⇒ #f),
;; or None if every child passes.
(: first-failing (-> (-> a Boolean) (-> (Stream (Tree a)) (Maybe (Tree a)))))
(define (first-failing pred s)
  (match s
    [(SNil) None]
    [(SCons c rest)
     (if (pred (tree-value c))
         (first-failing pred (force-lazy rest))
         (Some c))]))

;; Descend into the smallest still-failing child until none remains.
;; Bounded so a large/recursive shrink tree always terminates.
(: shrink-tree (-> (-> a Boolean) (-> (Tree a) a)))
(define (shrink-tree pred t)
  (letrec ([go (lambda (cur budget)
                 (if (<= budget 0)
                     (tree-value cur)
                     (match (first-failing pred (tree-children cur))
                       [(None)   (tree-value cur)]
                       [(Some c) (go c (- budget 1))])))])
    (go t 1000)))

;; ----- Property construction & running ------------------------------

;; Build a property from a generator, a predicate, and an explicit
;; renderer for counterexamples.  The renderer is passed explicitly
;; (rather than via a `Show` constraint) because the dictionary for a
;; class constraint does not thread into the closure captured by
;; `MkProperty`; passing `show` as a first-class value sidesteps that.
(: for-all-gen (-> (-> a String) (-> (Gen a) (-> (-> a Boolean) Property))))
(define (for-all-gen render g pred)
  (MkProperty
   (lambda (num-tests start-seed)
     (letrec ([loop (lambda (i s)
                      (if (>= i num-tests)
                          (PropPassed num-tests)
                          (let ([t (gen-tree g i s)])
                            (if (pred (tree-value t))
                                (loop (+ i 1) (next-seed s))
                                (PropFailed (render (shrink-tree pred t))
                                            start-seed)))))])
       (loop 0 (seed-from start-seed))))))

;; Convenience: render counterexamples with the `Show` instance.  Note
;; `show` is referenced as a first-class value and handed to
;; `for-all-gen`; it is not called inside a constraint-carrying closure.
(: for-all ((Show a) => (-> (Gen a) (-> (-> a Boolean) Property))))
(define (for-all g pred)
  (for-all-gen show g pred))

(: run-property (-> Integer (-> Integer (-> Property PropOutcome))))
(define (run-property num-tests start-seed prop)
  (match prop
    [(MkProperty f) (f num-tests start-seed)]))

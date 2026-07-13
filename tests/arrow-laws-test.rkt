#lang rackton

;; Extensional tests for the Arrow-family laws (stated intensionally as
;; `:laws` on the protocols).  No shipped arrow type has decidable arrow
;; equality, so we verify the laws EXTENSIONALLY: compose concrete arrows
;; and compare their OUTPUTS on sampled inputs.  Arrow / ArrowChoice /
;; ArrowApply are checked on the strict `(->)` arrow (product `Pair`,
;; coproduct `Either`); ArrowLoop has no `(->)` instance (tying the knot
;; needs laziness), so it is checked on the lazy-function arrow `LFun`.

(require "../unit.rkt"
         rackton/data/arrow-lazy)

(: gi (Gen Integer))
(define gi (int-range -50 50))

;; concrete (->) arrows
(: inc (-> Integer Integer)) (define inc (lambda (n) (+ n 1)))
(: dbl (-> Integer Integer)) (define dbl (lambda (n) (* n 2)))
(: dec3 (-> Integer Integer)) (define dec3 (lambda (n) (- n 3)))

;; `arr fst` on `(->)`, pinned (arr is return-typed)
(: fst-arr (-> (Pair Integer Integer) Integer))
(define fst-arr (ann (arr (lambda (q) (prod-fst q)))
                     (-> (Pair Integer Integer) Integer)))

;; ----- Arrow on (->) ------------------------------------------------

(: suite-arrow Test)
(define suite-arrow
  (describe "Arrow (->)"
            (it-prop "on-first acts on the first component only"
                     (for-all (gen-pair gi gi)
                              (lambda (q) (match q [(Pair a c)
                                                    (== ((on-first inc) (Pair a c)) (Pair (inc a) c))]))))
            (it-prop "split acts componentwise"
                     (for-all (gen-pair gi gi)
                              (lambda (q) (match q [(Pair a c)
                                                    (== ((split inc dbl) (Pair a c)) (Pair (inc a) (dbl c)))]))))
            (it-prop "fanout duplicates the input"
                     (for-all gi (lambda (a) (== ((fanout inc dbl) a) (Pair (inc a) (dbl a))))))
            (it-prop "first-composition: first (g . f) = first g . first f"
                     (for-all (gen-pair gi gi)
                              (lambda (q) (== ((on-first (comp dbl inc)) q)
                                              ((comp (on-first dbl) (on-first inc)) q)))))
            (it-prop "first-projection: first f >>> fst = fst >>> f"
                     (for-all (gen-pair gi gi)
                              (lambda (q) (== ((comp fst-arr (on-first inc)) q)
                                              ((comp inc fst-arr) q)))))))

;; ----- ArrowChoice on (->) ------------------------------------------

(: suite-choice Test)
(define suite-choice
  (describe "ArrowChoice (->)"
            (it-prop "on-left runs the arrow on Left, passes Right through"
                     (for-all gi
                              (lambda (a)
                                (and (== ((on-left inc) (ann (Left a) (Either Integer Integer)))
                                         (ann (Left (inc a)) (Either Integer Integer)))
                                     (== ((on-left inc) (ann (Right a) (Either Integer Integer)))
                                         (ann (Right a) (Either Integer Integer)))))))
            (it-prop "left-composition: left (g . f) = left g . left f"
                     (for-all gi
                              (lambda (a)
                                (== ((on-left (comp dbl inc)) (ann (Left a) (Either Integer Integer)))
                                    ((comp (on-left dbl) (on-left inc)) (ann (Left a) (Either Integer Integer)))))))
            (it-prop "left-injection: f >>> Left = Left >>> left f"
                     (for-all gi
                              (lambda (a)
                                (== ((comp (on-left inc) (ann (arr (lambda (v) (inj-left v)))
                                                              (-> Integer (Either Integer Integer)))) a)
                                    ((comp (ann (arr (lambda (v) (inj-left v)))
                                                (-> Integer (Either Integer Integer))) inc) a)))))))

;; ----- ArrowApply on (->) -------------------------------------------

(: app-arr (-> (Pair (-> Integer Integer) Integer) Integer))
(define app-arr (ann arrow-app
                     (-> (Pair (-> Integer Integer) Integer) Integer)))

(: suite-apply Test)
(define suite-apply
  (describe "ArrowApply (->)"
            (it-prop "arrow-app applies the paired arrow"
                     (for-all gi (lambda (x) (== (app-arr (Pair inc x)) (inc x)))))
            (it-prop "apply-pairing: app . (\\a -> (f, a)) = f"
                     (for-all gi
                              (lambda (x)
                                (== ((comp app-arr
                                           (ann (arr (lambda (av) (Pair inc av)))
                                                (-> Integer (Pair (-> Integer Integer) Integer))))
                                     x)
                                    (inc x)))))))

;; ----- ArrowLoop on the lazy-function arrow LFun --------------------
;; left-tightening: loop (first h >>> f) = h >>> loop f.
;; `f` outputs a CONSTANT feedback component (0), so the knot converges
;; immediately and the loop terminates.  With h = (+1) and f (e,_) =
;; (e*2, 0), both sides compute (a+1)*2.

(: h-loop (LFun Integer Integer))
(define h-loop (arr inc))

(: f-loop (LFun (LPair Integer Integer) (LPair Integer Integer)))
(define f-loop (arr (lambda (q) (mk-prod (dbl (prod-fst q)) 0))))

(: lhs-loop (LFun Integer Integer))
(define lhs-loop (arrow-loop (comp f-loop (on-first h-loop))))

(: rhs-loop (LFun Integer Integer))
(define rhs-loop (comp (arrow-loop f-loop) h-loop))

(: suite-loop Test)
(define suite-loop
  (describe "ArrowLoop (LFun)"
            (it-prop "left-tightening: loop (first h >>> f) = h >>> loop f"
                     (for-all gi
                              (lambda (a) (== (run-lfun lhs-loop a) (run-lfun rhs-loop a)))))))

;; ----- run ----------------------------------------------------------

(: test-main (IO Unit))
(define test-main (let& ([_ (run-suite-tree suite-arrow)]
                         [_ (run-suite-tree suite-choice)]
                         [_ (run-suite-tree suite-apply)]
                         [_ (run-suite-tree suite-loop)])
                    (pure Unit)))

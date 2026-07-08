#lang rackton

;; Feature test for the Arrow/ArrowChoice protocol DEFAULTS: a
;; user-defined arrow that supplies only the PRIMITIVES (Category
;; ident/comp, Arrow arr/on-first, ArrowChoice on-left) and inherits
;; on-second/split/fanout and on-right/fork/fanin from the class
;; defaults.
;;
;; This exercises inherited-default return-typed resolution end to end:
;; the defaults call the return-typed `arr` / `mk-prod` / `inj-left` /
;; `inj-right` over the abstract carrier, which must resolve against
;; THIS instance's concrete arrow / product / coproduct.  Distinct from
;; category-arrow-test, whose `(->)` combinators are served by
;; hand-written runtime impls in prelude-runtime.rkt — here the derived
;; code is what actually runs.

(require "../unit.rkt")

(: inc (-> Integer Integer))
(define (inc x) (+ x 1))

(: dbl (-> Integer Integer))
(define (dbl x) (* x 2))

;; A strict function arrow distinct from the prelude's `(->)`, reusing
;; the prelude Pair / Either tensors.
(data (Fn a b) (Fn (-> a b)))

(: run-fn (-> (Fn a b) (-> a b)))
(define (run-fn af x) (match af [(Fn f) (f x)]))

(instance (Category Fn)
  (define ident (Fn (lambda (x) x)))
  (define (comp x y)
    (match x [(Fn g) (match y [(Fn f) (Fn (lambda (a) (g (f a))))])])))

;; Only the primitives — on-second / split / fanout come from defaults.
(instance (Arrow Fn Pair)
  (define (arr h) (Fn h))
  (define (on-first x)
    (match x
      [(Fn f) (Fn (lambda (p) (match p [(Pair a c) (Pair (f a) c)])))])))

;; Only on-left — on-right / fork / fanin come from defaults.
(instance (ArrowChoice Fn Pair Either)
  (define (on-left x)
    (match x
      [(Fn f) (Fn (lambda (e) (match e
                                [(Left a)  (Left (f a))]
                                [(Right c) (Right c)])))])))

;; A second arrow whose Arrow instance supplies `split` INSTEAD of
;; `on-first` — the other valid primitive.  on-first / on-second /
;; fanout must derive from `split` (and `ident`).
(data (Gn a b) (Gn (-> a b)))

(: run-gn (-> (Gn a b) (-> a b)))
(define (run-gn ag x) (match ag [(Gn f) (f x)]))

(instance (Category Gn)
  (define ident (Gn (lambda (x) x)))
  (define (comp x y)
    (match x [(Gn g) (match y [(Gn f) (Gn (lambda (a) (g (f a))))])])))

(instance (Arrow Gn Pair)
  (define (arr h) (Gn h))
  (define (split x y)
    (match x
      [(Gn f) (match y
                [(Gn g) (Gn (lambda (p) (match p
                                          [(Pair a c) (Pair (f a) (g c))])))])])))

;; ArrowChoice for Gn supplies `fork` INSTEAD of `on-left` — the other
;; valid primitive.  on-left / on-right / fanin must derive from `fork`.
(instance (ArrowChoice Gn Pair Either)
  (define (fork x y)
    (match x
      [(Gn f) (match y
                [(Gn g) (Gn (lambda (e) (match e
                                          [(Left a)  (Left (f a))]
                                          [(Right c) (Right (g c))])))])])))

;; --- exercise the DERIVED combinators -------------------------------

(: arr-inc (Fn Integer Integer))
(define arr-inc (arr inc))

(: arr-dbl (Fn Integer Integer))
(define arr-dbl (arr dbl))

(: second-inc (-> (Pair Integer Integer) (Pair Integer Integer)))
(define (second-inc p) (run-fn (on-second arr-inc) p))

(: split-incdbl (-> (Pair Integer Integer) (Pair Integer Integer)))
(define (split-incdbl p) (run-fn (split arr-inc arr-dbl) p))

(: fanout-incdbl (-> Integer (Pair Integer Integer)))
(define (fanout-incdbl n) (run-fn (fanout arr-inc arr-dbl) n))

(: right-inc (-> (Either Integer Integer) (Either Integer Integer)))
(define (right-inc e) (run-fn (on-right arr-inc) e))

(: fork-incdbl (-> (Either Integer Integer) (Either Integer Integer)))
(define (fork-incdbl e) (run-fn (fork arr-inc arr-dbl) e))

(: fanin-incdbl (-> (Either Integer Integer) Integer))
(define (fanin-incdbl e) (run-fn (fanin arr-inc arr-dbl) e))

;; Gn: split is the primitive; on-first / on-second / fanout derive.
(: g-inc (Gn Integer Integer))
(define g-inc (arr inc))
(: g-dbl (Gn Integer Integer))
(define g-dbl (arr dbl))

(: g-first (-> (Pair Integer Integer) (Pair Integer Integer)))
(define (g-first p) (run-gn (on-first g-inc) p))

(: g-second (-> (Pair Integer Integer) (Pair Integer Integer)))
(define (g-second p) (run-gn (on-second g-inc) p))

(: g-fanout (-> Integer (Pair Integer Integer)))
(define (g-fanout n) (run-gn (fanout g-inc g-dbl) n))

;; Gn ArrowChoice: fork is the primitive; on-left/on-right/fanin derive.
(: g-onleft (-> (Either Integer Integer) (Either Integer Integer)))
(define (g-onleft e) (run-gn (on-left g-inc) e))

(: g-onright (-> (Either Integer Integer) (Either Integer Integer)))
(define (g-onright e) (run-gn (on-right g-inc) e))

(: g-fanin (-> (Either Integer Integer) Integer))
(define (g-fanin e) (run-gn (fanin g-inc g-dbl) e))

;; ----- suite -------------------------------------------------------

(: suite (List Test))
(define suite
  (list
    (it "derived on-second maps the second component"
        (check-equal? (second-inc (Pair 3 5)) (Pair 3 6)))
    (it "derived split runs both arrows on the two halves"
        (check-equal? (split-incdbl (Pair 3 5)) (Pair 4 10)))
    (it "derived fanout feeds one input to both arrows"
        (check-equal? (fanout-incdbl 3) (Pair 4 6)))
    (it "derived on-right maps the Right branch"
        (check-equal? (right-inc (Right 3)) (Right 4)))
    (it "derived on-right passes the Left branch through"
        (check-equal? (right-inc (Left 9)) (Left 9)))
    (it "derived fork maps the Left branch with the first arrow"
        (check-equal? (fork-incdbl (Left 3)) (Left 4)))
    (it "derived fork maps the Right branch with the second arrow"
        (check-equal? (fork-incdbl (Right 5)) (Right 10)))
    (it "derived fanin collapses Left with the first arrow"
        (check-equal? (fanin-incdbl (Left 3)) 4))
    (it "derived fanin collapses Right with the second arrow"
        (check-equal? (fanin-incdbl (Right 5)) 10))
    ;; split-as-primitive arrow: the other combinators derive from split.
    (it "on-first derives from split"
        (check-equal? (g-first (Pair 3 5)) (Pair 4 5)))
    (it "on-second derives from split"
        (check-equal? (g-second (Pair 3 5)) (Pair 3 6)))
    (it "fanout derives from split"
        (check-equal? (g-fanout 3) (Pair 4 6)))
    ;; fork-as-primitive ArrowChoice: the rest derive from fork.
    (it "on-left derives from fork (maps Left)"
        (check-equal? (g-onleft (Left 3)) (Left 4)))
    (it "on-left derives from fork (passes Right)"
        (check-equal? (g-onleft (Right 9)) (Right 9)))
    (it "on-right derives from fork (maps Right)"
        (check-equal? (g-onright (Right 3)) (Right 4)))
    (it "fanin derives from fork (Left → first arrow)"
        (check-equal? (g-fanin (Left 3)) 4))
    (it "fanin derives from fork (Right → second arrow)"
        (check-equal? (g-fanin (Right 5)) 10))))

(: test-main (IO Unit))
(define test-main (run-suite "arrow-default-derivation" suite))

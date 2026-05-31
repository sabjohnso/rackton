#lang rackton

;; `#:deriving` extended to records (via struct) and Foldable
;; derivation.  Also exercises newtype and parametric-ADT deriving as
;; regression coverage.

(require "../unit.rkt")

;; ----- 35.A non-parametric record ---------------------------
;; All four currently-supported classes on a simple record.

(struct Point
  [x : Integer]
  [y : Integer]
  #:deriving Eq Show Ord)

(: same-pt Boolean)
(define same-pt   (== (Point 1 2) (Point 1 2)))

(: diff-pt Boolean)
(define diff-pt   (== (Point 1 2) (Point 1 3)))

(: lt-pt Boolean)
(define lt-pt     (< (Point 1 2) (Point 1 3)))

(: shown-pt String)
(define shown-pt  (show (Point 7 9)))

;; ----- 35.B parametric record + Functor + Foldable -----------

(struct (Box a)
  [value : a]
  #:deriving Eq Show Functor Foldable)

(: mapped-box (Box Integer))
(define mapped-box (fmap (lambda (n) (+ n 1)) (Box 41)))

(: summed-box Integer)
(define summed-box (foldr + 0 (Box 100)))

(: listed-box (List Integer))
(define listed-box (to-list (Box 5)))

;; ----- 35.C parametric ADT with constraint propagation -------
;; `Eq (Pair2 a b)` needs `(Eq a) (Eq b) =>`.  Two non-Eq fields
;; would prevent the synth from firing — the test below uses two
;; Integers, so Eq applies.

(data (Pair2 a b)
  (MkPair2 a b)
  #:deriving Eq Show)

(: pair2-eq Boolean)
(define pair2-eq (== (MkPair2 1 "a") (MkPair2 1 "a")))

(: pair2-show String)
(define pair2-show (show (MkPair2 7 "x")))

;; ----- 35.D recursive parametric ADT + Foldable --------------
;; A small tree; foldr should visit every leaf left-to-right.

(data (Tree a)
  Leaf
  (Branch (Tree a) a (Tree a))
  #:deriving Eq Show Foldable)

(: example-tree (Tree Integer))
(define example-tree
  (Branch
   (Branch Leaf 1 (Branch Leaf 2 Leaf))
   3
   (Branch Leaf 4 Leaf)))

(: tree-sum  Integer)
(define tree-sum  (foldr + 0 example-tree))

(: tree-list (List Integer))
(define tree-list (to-list example-tree))

;; ----- 35.E newtype deriving (regression) --------------------

(newtype Wrap (MkWrap Integer)
  #:deriving Eq Show)

(: wrap-eq Boolean)
(define wrap-eq   (== (MkWrap 1) (MkWrap 1)))

(: wrap-show String)
(define wrap-show (show (MkWrap 42)))

(newtype (Idiom a) (MkIdiom a)
  #:deriving Eq Show Functor)

(: id-mapped (Idiom Integer))
(define id-mapped (fmap (lambda (n) (* n 2)) (MkIdiom 21)))

(: suite (List Test))
(define suite
  (list
   (it "non-parametric record derives Eq/Show/Ord"
       (all-checks
        (list (check-true  same-pt)
              (check-false diff-pt)
              (check-true  lt-pt)
              (check-equal? shown-pt "(Point 7 9)"))))
   (it "parametric record derives Functor + Foldable"
       (all-checks
        (list (check-equal? mapped-box (Box 42))
              (check-equal? summed-box 100)
              (check-equal? listed-box (Cons 5 Nil)))))
   (it "parametric ADT Eq + Show with constraint propagation"
       (all-checks
        (list (check-true   pair2-eq)
              (check-equal? pair2-show "(MkPair2 7 \"x\")"))))
   (it "recursive ADT Foldable"
       (all-checks
        (list (check-equal? tree-sum  10)
              (check-equal? tree-list (Cons 1 (Cons 2 (Cons 3 (Cons 4 Nil))))))))
   (it "newtype deriving (Eq/Show/Functor)"
       (all-checks
        (list (check-true   wrap-eq)
              (check-equal? wrap-show "(MkWrap 42)")
              (check-equal? id-mapped (MkIdiom 42)))))))

(: _ran Unit)
(define _ran (run-io (run-suite "deriving records + Foldable" suite)))

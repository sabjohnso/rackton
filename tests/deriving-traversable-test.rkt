#lang rackton

;; Regression coverage for `:deriving Traversable` — which the rest of
;; the suite never exercised (existing traversable tests use the
;; prelude's hand-written instances).  Covers each arity branch of the
;; synthesizer and a recursive type, traversing into the Maybe
;; applicative.  Two bugs this guards against:
;;   - recursive types crashed (`$dict-pure-…: unbound identifier`) from
;;     the shared-syntax-object resolution collision, and
;;   - constructors with >= 3 fields crashed (`arity mismatch`) because
;;     the applicative composition assumed a curried constructor while
;;     Rackton constructors are n-ary.

(require "../unit.rkt")

(data (One a)   (MkOne a)       :deriving Functor Foldable Traversable Eq Show)
(data (Two a)   (MkTwo a a)     :deriving Functor Foldable Traversable Eq Show)
(data (Three a) (MkThree a a a) :deriving Functor Foldable Traversable Eq Show)
(data (Tree a)  (Leaf a) (Node (Tree a) (Tree a))
  :deriving Functor Foldable Traversable Eq Show)

(: pos (-> Integer (Maybe Integer)))
(define (pos n) (if (> n 0) (Some n) None))

;; All-positive traversals succeed (Some …); a non-positive element
;; short-circuits the whole structure to None.
(: r1   (Maybe (One Integer)))   (define r1   (traverse pos (MkOne 1)))
(: r2   (Maybe (Two Integer)))   (define r2   (traverse pos (MkTwo 1 2)))
(: r3   (Maybe (Three Integer))) (define r3   (traverse pos (MkThree 1 2 3)))
(: r3b  (Maybe (Three Integer))) (define r3b  (traverse pos (MkThree 1 0 3)))
(: tree (Tree Integer))          (define tree (Node (Leaf 1) (Node (Leaf 2) (Leaf 3))))
(: rt   (Maybe (Tree Integer)))  (define rt   (traverse pos tree))
(: rtb  (Maybe (Tree Integer)))
(define rtb (traverse pos (Node (Leaf 1) (Leaf -2))))

(: suite (List Test))
(define suite
  (list
    (it "arity 1 (fmap path)"
        (check-equal? r1 (Some (MkOne 1))))
    (it "arity 2 (liftA2 path)"
        (check-equal? r2 (Some (MkTwo 1 2))))
    (it "arity 3 (fmap+fapply chain over an n-ary ctor)"
        (all-checks (list (check-equal? r3  (Some (MkThree 1 2 3)))
                          (check-equal? r3b None))))
    (it "recursive type, all-Some"
        (check-equal? rt (Some (Node (Leaf 1) (Node (Leaf 2) (Leaf 3))))))
    (it "recursive type, short-circuits to None"
        (check-equal? rtb None))))

(: test-main (IO Unit))
(define test-main (run-suite "deriving Traversable" suite))

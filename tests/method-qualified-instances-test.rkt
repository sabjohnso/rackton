#lang rackton

;; Method-qual dict-threading.  Extends the instance-qual skolem
;; mechanism (which handles instance-level `=>` contexts) to
;; method-local quantifiers like `(Applicative f) =>` on `traverse`.
;; Unblocks user-defined and derived Traversable instances.

(require "../unit.rkt")

;; ----- user-written Traversable instance --------------

(data (Tree a)
  Leaf
  (Branch (Tree a) a (Tree a))
  :deriving Eq Show)

;; A small curried helper that re-builds Branch — auto-curry
;; would help if we called `Branch` directly, but passing it
;; through liftA2 / fapply needs the curried form.
(: branchC (-> (Tree a) (-> a (-> (Tree a) (Tree a)))))
(define (branchC l) (lambda (v) (lambda (r) (Branch l v r))))

(instance (Traversable Tree)
  (define (traverse f t)
    (match t
      [(Leaf) (pure Leaf)]
      [(Branch l v r)
       (fapply (fapply (fapply (pure branchC) (traverse f l))
                       (f v))
               (traverse f r))])))

(: positive? (-> Integer (Maybe Integer)))
(define (positive? n) (if (> n 0) (Some n) None))

(: hand-trav-ok (Maybe (Tree Integer)))
(define hand-trav-ok
  (traverse positive?
            (Branch Leaf 1 (Branch Leaf 2 Leaf))))

(: hand-trav-fail (Maybe (Tree Integer)))
(define hand-trav-fail
  (traverse positive?
            (Branch Leaf 1 (Branch Leaf -2 Leaf))))

;; ----- 39.B derived Traversable on a simpler ADT ------------

(data (Box a) (MkBox a)
  :deriving Functor Foldable Traversable Eq Show)

(: derived-trav-ok (Maybe (Box Integer)))
(define derived-trav-ok (traverse positive? (MkBox 5)))

(: derived-trav-fail (Maybe (Box Integer)))
(define derived-trav-fail (traverse positive? (MkBox -5)))

;; ---------- assertions ---------------------------------------

(: suite (List Test))
(define suite
  (list
    (it "user-written Traversable Tree (success)"
        (check-equal? hand-trav-ok
                      (Some (Branch Leaf 1 (Branch Leaf 2 Leaf)))))
    (it "user-written Traversable Tree (None short-circuits)"
        (check-equal? hand-trav-fail None))
    (it "derived Traversable Box (success)"
        (check-equal? derived-trav-ok (Some (MkBox 5))))
    (it "derived Traversable Box (failure)"
        (check-equal? derived-trav-fail None))))

(: test-main (IO Unit))
(define test-main (run-suite "method-qualified-instances" suite))

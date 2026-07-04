#lang racket/base

;; Generated `deriving` instance bodies AND the class default methods they
;; rely on must bind to the METHODS / constructors they mean (`fmap`,
;; `foldr`, `bimap`, `mappend`, `mempty`, `Some`, …), not to a same-named
;; local shadow.  Each submodule shadows the referenced names with
;; type-INCOMPATIBLE bindings, derives instances, and exercises them via
;; the prelude method (qualified `p:`, since the bare name is shadowed).

(require rackunit)

(module functor-blk rackton
  (require (qualified-in p rackton/prelude))
  (provide same? sum)
  (: fmap  (-> a b Boolean))    (define (fmap f x) #t)
  (: foldr (-> a b c Boolean))  (define (foldr f z xs) #t)

  (data (Tree a) (Leaf a) (Branch (Tree a) (Tree a))
    :deriving Functor Foldable Eq Show)

  (: t (Tree Integer))
  (define t (Branch (Leaf 1) (Branch (Leaf 2) (Leaf 3))))
  (: mapped (Tree Integer))
  (define mapped (p:fmap (lambda (x) (* x 10)) t))
  (: same? Boolean)
  (define same? (== mapped (Branch (Leaf 10) (Branch (Leaf 20) (Leaf 30)))))
  ;; derived Foldable + its class-default `length`-family (uses foldr)
  (: sum Integer)
  (define sum (p:foldr (lambda (x acc) (+ x acc)) 0 t)))

(module bifunctor-blk rackton
  (require (qualified-in p rackton/prelude))
  (provide ok?)
  (: bimap (-> a b c Boolean)) (define (bimap f g x) #t)

  (data (Two a b) (MkTwo a b) :deriving Bifunctor Eq Show)

  (: v (Two Integer Integer)) (define v (MkTwo 3 4))
  (: w (Two Integer Integer))
  (define w (p:bimap (lambda (x) (+ x 1)) (lambda (y) (* y 2)) v))
  (: ok? Boolean)
  (define ok? (== w (MkTwo 4 8))))

(require (prefix-in f: (submod "." functor-blk))
         (prefix-in b: (submod "." bifunctor-blk)))

(test-case "derived Functor.fmap under an fmap shadow"
  (check-true f:same?))

(test-case "derived Foldable + its length-family default under a foldr shadow"
  (check-equal? f:sum 6))

(test-case "derived Bifunctor.bimap + first/second defaults under a bimap shadow"
  (check-true b:ok?))

#lang rackton

;; rackton/unit — generators with integrated shrinking.
;;
;; A `Tree a` is a generated value together with a lazy stream of
;; progressively smaller shrink candidates (each itself a `Tree`, so
;; shrinking is recursive).  A `Gen a` maps a size and a seed to such a
;; tree.  The Functor/Applicative/Monad instances thread the seed by
;; splitting and combine shrink trees via `tree-map` / `tree-bind`
;; (Hedgehog-style interleaving) — so every generator shrinks for free,
;; with no separate `shrink` method to write.
;;
;; Shrink streams are built from finite candidate Lists, guaranteeing
;; termination even though the stream type itself is lazy/infinite-capable.
;;
;; Public API: Tree, Gen, run-gen, tree-value, tree-children,
;; tree-map, tree-bind, int-range, bool (+ Functor/Applicative/Monad Gen).

(require "lazy.rkt"
         "prng.rkt")

(provide (data-out Tree)
         (data-out Gen)
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

;; ----- Shrink trees -------------------------------------------------

(data (Tree a) (Tree a (Lazy (Stream (Tree a)))))

(: tree-value (-> (Tree a) a))
(define (tree-value t)
  (match t [(Tree v _) v]))

(: tree-children (-> (Tree a) (Stream (Tree a))))
(define (tree-children t)
  (match t [(Tree _ ls) (force-lazy ls)]))

;; Map a function over a value and all of its shrinks.
(: tree-map (-> (-> a b) (-> (Tree a) (Tree b))))
(define (tree-map f t)
  (match t
    [(Tree v ls)
     (Tree (f v)
             (delay (stream-map (lambda (c) (tree-map f c))
                                (force-lazy ls))))]))

;; Monadic bind on trees: the children of the result interleave the
;; outer tree's shrinks (re-bound through `f`) with the inner tree's
;; shrinks — this is what makes shrinking compose through `flatmap`.
(: tree-bind (-> (-> a (Tree b)) (-> (Tree a) (Tree b))))
(define (tree-bind f t)
  (match t
    [(Tree v ls)
     (match (f v)
       [(Tree v2 ls2)
        (Tree v2
                (delay (stream-append
                        (stream-map (lambda (c) (tree-bind f c))
                                    (force-lazy ls))
                        (force-lazy ls2))))])]))

;; ----- Generators ---------------------------------------------------

(data (Gen a) (Gen (-> Integer (-> Seed (Tree a)))))

(: run-gen (-> (Gen a) (-> Integer (-> Seed (Tree a)))))
(define (run-gen g)
  (match g [(Gen f) f]))

;; Run a generator at a given size and starting seed, producing its
;; shrink tree.  The public way to "execute" a generator.
(: gen-tree (-> (Gen a) (-> Integer (-> Seed (Tree a)))))
(define (gen-tree g size seed)
  ((run-gen g) size seed))

;; A generator that always yields `x` with no shrinks.  This is the
;; same value as `pure` for `Gen`, but exported as a plain function:
;; return-typed method resolution (`pure`/`mempty`) is resolved at
;; compile time to a per-instance impl binding that does not currently
;; cross a module boundary, so code in OTHER modules should call
;; `constant` rather than `pure` over `Gen`.
(: constant (-> a (Gen a)))
(define (constant x)
  (Gen (lambda (size seed) (Tree x (delay-lazy SNil)))))

(instance (Functor Gen)
  (define (fmap f g)
    (Gen (lambda (size seed)
             (tree-map f (gen-tree g size seed))))))

(instance (Applicative Gen)
  (define (pure x) (constant x))
  (define (fapply gf gx)
    (Gen (lambda (size seed)
             (match (split-seed seed)
               [(Pair s1 s2)
                (tree-bind (lambda (f) (tree-map f (gen-tree gx size s2)))
                           (gen-tree gf size s1))])))))

(instance (Monad Gen)
  (define (flatmap f g)
    (Gen (lambda (size seed)
             (match (split-seed seed)
               [(Pair s1 s2)
                (tree-bind (lambda (a) (gen-tree (f a) size s2))
                           (gen-tree g size s1))])))))

;; ----- Integer shrinking --------------------------------------------

;; Candidate values approaching `v` from `target`, by repeatedly halving
;; the gap: target, then v-(gap/2), v-(gap/4), …  Returned target-first
;; (most aggressive shrink first).  Always finite ⇒ shrinking terminates.
(: halve-toward (-> Integer (-> Integer (List Integer))))
(define (halve-toward target v)
  (letrec ([go (lambda (delta acc)
                 (if (<= delta 0)
                     acc
                     (go (quot delta 2) (Cons (- v delta) acc))))])
    (reverse (go (- v target) Nil))))

(: candidates->stream (-> Integer (-> (List Integer) (Stream (Tree Integer)))))
(define (candidates->stream target cs)
  (match cs
    [(Nil) SNil]
    [(Cons c rest)
     (SCons (Tree c (delay (int-shrinks target c)))
            (delay (candidates->stream target rest)))]))

(: int-shrinks (-> Integer (-> Integer (Stream (Tree Integer)))))
(define (int-shrinks target v)
  (candidates->stream target (halve-toward target v)))

;; ----- Primitive generators -----------------------------------------

;; A uniform integer in [lo, hi], shrinking toward `lo`.
(: int-range (-> Integer (-> Integer (Gen Integer))))
(define (int-range lo hi)
  (Gen (lambda (size seed)
           (let ([v (seed-int-range seed lo hi)])
             (Tree v (delay (int-shrinks lo v)))))))

;; A boolean; #t shrinks to #f, #f is minimal.
(: bool (Gen Boolean))
(define bool
  (Gen (lambda (size seed)
           (if (== (seed-int-range seed 0 1) 0)
               (Tree #f (delay-lazy SNil))
               (Tree #t (delay (SCons (Tree #f (delay-lazy SNil))
                                      (delay-lazy SNil))))))))

;; ----- Default & compound generators --------------------------------
;;
;; These are plain functions / positional-method (`flatmap`)
;; compositions, so they resolve across module boundaries — unlike a
;; return-typed `arbitrary` class member would.  They are the
;; cross-module-safe API for property testing.

;; A default Integer generator over a moderate range.
(: gen-integer (Gen Integer))
(define gen-integer (int-range -1000 1000))

;; A default Boolean generator.
(: gen-boolean (Gen Boolean))
(define gen-boolean bool)

;; A pair, from two component generators.
(: gen-pair (-> (Gen a) (-> (Gen b) (Gen (Pair a b)))))
(define (gen-pair ga gb)
  (do [x <- ga]
      [y <- gb]
    (constant (Pair x y))))

;; Exactly `n` elements drawn from `g`.
(: replicate-gen (-> Integer (-> (Gen a) (Gen (List a)))))
(define (replicate-gen n g)
  (if (<= n 0)
      (constant Nil)
      (do [x  <- g]
          [xs <- (replicate-gen (- n 1) g)]
        (constant (Cons x xs)))))

;; A list of 0–8 elements from `g`.  Shrinks both length (toward 0, via
;; the int-range) and each element (via `g`'s shrink tree), interleaved
;; by the generator's monadic bind.
(: gen-list (-> (Gen a) (Gen (List a))))
(define (gen-list g)
  (do [n <- (int-range 0 8)]
    (replicate-gen n g)))

(: list-index (-> (List a) (-> Integer a)))
(define (list-index xs i)
  (match xs
    [(Nil)      (panic "element-of: index past end of list")]
    [(Cons h t) (if (<= i 0) h (list-index t (- i 1)))]))

;; Pick a uniformly-random element of a NON-EMPTY list; shrinks toward
;; the first element (the index shrinks toward 0).
(: element-of (-> (List a) (Gen a)))
(define (element-of xs)
  (fmap (lambda (i) (list-index xs i))
        (int-range 0 (- (length xs) 1))))

;; A small selection of sample strings, shrinking toward "".
(: gen-string (Gen String))
(define gen-string
  (element-of (Cons ""
                    (Cons "a"
                          (Cons "bc"
                                (Cons "hello"
                                      (Cons "xyz" Nil)))))))

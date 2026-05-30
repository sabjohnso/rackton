#lang rackton

;; rackton/data/list — list utilities moved out of the auto-prelude
;; (Phase 2 slim).  The core list ops (map/filter/foldr/length/reverse/
;; append/head/tail/null/elem) stay in the prelude; these less-core
;; helpers require `(require rackton/data/list)`.

(provide (all-defined-out))

(: zip (-> (List a) (-> (List b) (List (Pair a b)))))
(define (zip as bs)
  (match as
    [(Nil) Nil]
    [(Cons a at)
     (match bs
       [(Nil) Nil]
       [(Cons b bt)
        (Cons (MkPair a b) (zip at bt))])]))

(: take (-> Integer (-> (List a) (List a))))
(define (take n xs)
  (if (<= n 0)
      Nil
      (match xs
        [(Nil)        Nil]
        [(Cons h t)   (Cons h (take (- n 1) t))])))

(: drop (-> Integer (-> (List a) (List a))))
(define (drop n xs)
  (if (<= n 0)
      xs
      (match xs
        [(Nil)        Nil]
        [(Cons _ t)   (drop (- n 1) t)])))

(: find (-> (-> a Boolean) (-> (List a) (Maybe a))))
(define (find p xs)
  (match xs
    [(Nil)        None]
    [(Cons h t)   (if (p h) (Some h) (find p t))]))

(: concat-map (-> (-> a (List b)) (-> (List a) (List b))))
(define (concat-map f xs)
  (foldr (lambda (x acc) (append (f x) acc)) Nil xs))

;; --- Merge sort over (Ord a).  O(n log n) stable. -----------------

(: split-at (-> Integer (-> (List a) (Pair (List a) (List a)))))
(define (split-at n xs)
  (if (== n 0)
      (MkPair Nil xs)
      (match xs
        [(Nil) (MkPair Nil Nil)]
        [(Cons h t)
         (let ([rest (split-at (- n 1) t)])
           (MkPair (Cons h (fst rest)) (snd rest)))])))

(: merge-lists ((Ord a) => (-> (List a) (-> (List a) (List a)))))
(define (merge-lists xs ys)
  (match xs
    [(Nil) ys]
    [(Cons hx tx)
     (match ys
       [(Nil) xs]
       [(Cons hy ty)
        (if (< hx hy)
            (Cons hx (merge-lists tx ys))
            (Cons hy (merge-lists xs ty)))])]))

(: sort ((Ord a) => (-> (List a) (List a))))
(define (sort xs)
  (let ([n (length xs)])
    (if (< n 2)
        xs
        ;; integer halving via a racket/base escape (quotient).
        (let ([halves (split-at (racket Integer (n) (quotient n 2)) xs)])
          (merge-lists (sort (fst halves))
                       (sort (snd halves)))))))

;; ===== Data.List parity =============================================
;;
;; Kebab-case, curried like the prelude's list ops.  The partial
;; accessors return @racket[Maybe] (Haskell's @racket[head]/@racket[tail]
;; are partial; Rackton prefers total).

;; --- safe accessors ------------------------------------------------

(: head (-> (List a) (Maybe a)))
(define (head xs) (match xs [(Nil) None] [(Cons h _) (Some h)]))

(: tail (-> (List a) (Maybe (List a))))
(define (tail xs) (match xs [(Nil) None] [(Cons _ t) (Some t)]))

(: last (-> (List a) (Maybe a)))
(define (last xs)
  (match xs
    [(Nil)          None]
    [(Cons h (Nil)) (Some h)]
    [(Cons _ t)     (last t)]))

(: init (-> (List a) (Maybe (List a))))
(define (init xs)
  (match xs
    [(Nil)          None]
    [(Cons _ (Nil)) (Some Nil)]
    [(Cons h t)     (match (init t)
                      [(Some r) (Some (Cons h r))]
                      [(None)   None])]))

(: empty? (-> (List a) Boolean))
(define (empty? xs) (match xs [(Nil) #t] [(Cons _ _) #f]))

;; --- membership ----------------------------------------------------

(: elem ((Eq a) => (-> a (-> (List a) Boolean))))
(define (elem x xs)
  (match xs
    [(Nil)      #f]
    [(Cons h t) (if (== x h) #t (elem x t))]))

(: not-elem ((Eq a) => (-> a (-> (List a) Boolean))))
(define (not-elem x xs) (not (elem x xs)))

(: lookup ((Eq k) => (-> k (-> (List (Pair k v)) (Maybe v)))))
(define (lookup k xs)
  (match xs
    [(Nil) None]
    [(Cons (MkPair k0 v0) t) (if (== k k0) (Some v0) (lookup k t))]))

(: elem-index ((Eq a) => (-> a (-> (List a) (Maybe Integer)))))
(define (elem-index x xs) (find-index (lambda (y) (== x y)) xs))

(: find-index (-> (-> a Boolean) (-> (List a) (Maybe Integer))))
(define (find-index p xs)
  (let go ([ys xs] [i 0])
    (match ys
      [(Nil)      None]
      [(Cons h t) (if (p h) (Some i) (go t (+ i 1)))])))

;; --- building ------------------------------------------------------

(: concat (-> (List (List a)) (List a)))
(define (concat xss) (foldr (lambda (xs acc) (append xs acc)) Nil xss))

(: intersperse (-> a (-> (List a) (List a))))
(define (intersperse sep xs)
  (match xs
    [(Nil)          Nil]
    [(Cons h (Nil)) (Cons h Nil)]
    [(Cons h t)     (Cons h (Cons sep (intersperse sep t)))]))

(: intercalate (-> (List a) (-> (List (List a)) (List a))))
(define (intercalate sep xss) (concat (intersperse sep xss)))

(: replicate (-> Integer (-> a (List a))))
(define (replicate n x) (if (<= n 0) Nil (Cons x (replicate (- n 1) x))))

;; inclusive integer range [lo .. hi]
(: range (-> Integer (-> Integer (List Integer))))
(define (range lo hi) (if (> lo hi) Nil (Cons lo (range (+ lo 1) hi))))

;; --- slicing -------------------------------------------------------

(: take-while (-> (-> a Boolean) (-> (List a) (List a))))
(define (take-while p xs)
  (match xs
    [(Nil)      Nil]
    [(Cons h t) (if (p h) (Cons h (take-while p t)) Nil)]))

(: drop-while (-> (-> a Boolean) (-> (List a) (List a))))
(define (drop-while p xs)
  (match xs
    [(Nil)      Nil]
    [(Cons h t) (if (p h) (drop-while p t) xs)]))

(: span (-> (-> a Boolean) (-> (List a) (Pair (List a) (List a)))))
(define (span p xs) (MkPair (take-while p xs) (drop-while p xs)))

(: break (-> (-> a Boolean) (-> (List a) (Pair (List a) (List a)))))
(define (break p xs) (span (lambda (x) (not (p x))) xs))

(: partition (-> (-> a Boolean) (-> (List a) (Pair (List a) (List a)))))
(define (partition p xs)
  (foldr (lambda (x acc)
           (if (p x)
               (MkPair (Cons x (fst acc)) (snd acc))
               (MkPair (fst acc) (Cons x (snd acc)))))
         (MkPair Nil Nil)
         xs))

;; --- folding / predicates ------------------------------------------

(: fold-left (-> (-> b (-> a b)) (-> b (-> (List a) b))))
(define (fold-left f z xs)
  (match xs
    [(Nil)      z]
    [(Cons h t) (fold-left f (f z h) t)]))

(: all? (-> (-> a Boolean) (-> (List a) Boolean)))
(define (all? p xs)
  (match xs [(Nil) #t] [(Cons h t) (if (p h) (all? p t) #f)]))

(: any? (-> (-> a Boolean) (-> (List a) Boolean)))
(define (any? p xs)
  (match xs [(Nil) #f] [(Cons h t) (if (p h) #t (any? p t))]))

(: and-list (-> (List Boolean) Boolean))
(define (and-list xs) (all? (lambda (b) b) xs))

(: or-list (-> (List Boolean) Boolean))
(define (or-list xs) (any? (lambda (b) b) xs))

(: maximum ((Ord a) => (-> (List a) (Maybe a))))
(define (maximum xs)
  (match xs
    [(Nil)      None]
    [(Cons h t) (Some (fold-left (lambda (acc x) (max acc x)) h t))]))

(: minimum ((Ord a) => (-> (List a) (Maybe a))))
(define (minimum xs)
  (match xs
    [(Nil)      None]
    [(Cons h t) (Some (fold-left (lambda (acc x) (min acc x)) h t))]))

;; --- zipping / uniqueness ------------------------------------------

(: zip-with (-> (-> a (-> b c)) (-> (List a) (-> (List b) (List c)))))
(define (zip-with f as bs)
  (match as
    [(Nil) Nil]
    [(Cons a at)
     (match bs
       [(Nil)        Nil]
       [(Cons b bt)  (Cons (f a b) (zip-with f at bt))])]))

(: unzip (-> (List (Pair a b)) (Pair (List a) (List b))))
(define (unzip xs)
  (foldr (lambda (p acc)
           (MkPair (Cons (fst p) (fst acc)) (Cons (snd p) (snd acc))))
         (MkPair Nil Nil)
         xs))

(: nub ((Eq a) => (-> (List a) (List a))))
(define (nub xs)
  (match xs
    [(Nil)      Nil]
    [(Cons h t) (Cons h (nub (filter (lambda (y) (not (== h y))) t)))]))

(: nub-by (-> (-> a (-> a Boolean)) (-> (List a) (List a))))
(define (nub-by eq? xs)
  (match xs
    [(Nil)      Nil]
    [(Cons h t) (Cons h (nub-by eq? (filter (lambda (y) (not (eq? h y))) t)))]))

;; --- scans ---------------------------------------------------------

(: scanl (-> (-> b (-> a b)) (-> b (-> (List a) (List b)))))
(define (scanl f z xs)
  (Cons z (match xs
            [(Nil)      Nil]
            [(Cons h t) (scanl f (f z h) t)])))

(: scanr (-> (-> a (-> b b)) (-> b (-> (List a) (List b)))))
(define (scanr f z xs)
  (match xs
    [(Nil)      (Cons z Nil)]
    [(Cons h t) (match (scanr f z t)
                  [(Cons q qs) (Cons (f h q) (Cons q qs))]
                  [(Nil)       (Cons (f h z) Nil)])]))

;; --- grouping / sublists -------------------------------------------

;; consecutive runs of equal elements (Haskell `group`).
;; Uses take-while/drop-while directly rather than `let`-binding a
;; `span` result: let-generalizing a value whose type carries the
;; instance's own `(Eq a)` over the rigid skolem can't discharge the
;; constraint at the let.
(: group ((Eq a) => (-> (List a) (List (List a)))))
(define (group xs)
  (match xs
    [(Nil)      Nil]
    [(Cons h t) (Cons (Cons h (take-while (lambda (y) (== y h)) t))
                      (group (drop-while (lambda (y) (== y h)) t)))]))

(: inits (-> (List a) (List (List a))))
(define (inits xs)
  (Cons Nil (match xs
              [(Nil)      Nil]
              [(Cons h t) (fmap (lambda (i) (Cons h i)) (inits t))])))

(: tails (-> (List a) (List (List a))))
(define (tails xs)
  (Cons xs (match xs
             [(Nil)      Nil]
             [(Cons _ t) (tails t)])))

;; --- prefix / suffix / infix ---------------------------------------

(: prefix? ((Eq a) => (-> (List a) (-> (List a) Boolean))))
(define (prefix? ps xs)
  (match ps
    [(Nil)       #t]
    [(Cons p pt) (match xs
                   [(Nil)       #f]
                   [(Cons x xt) (if (== p x) (prefix? pt xt) #f)])]))

(: suffix? ((Eq a) => (-> (List a) (-> (List a) Boolean))))
(define (suffix? ss xs) (prefix? (reverse ss) (reverse xs)))

(: infix? ((Eq a) => (-> (List a) (-> (List a) Boolean))))
(define (infix? ns xs) (any? (lambda (t) (prefix? ns t)) (tails xs)))

(: strip-prefix ((Eq a) => (-> (List a) (-> (List a) (Maybe (List a))))))
(define (strip-prefix ps xs)
  (match ps
    [(Nil)       (Some xs)]
    [(Cons p pt) (match xs
                   [(Nil)       None]
                   [(Cons x xt) (if (== p x) (strip-prefix pt xt) None)])]))

;; --- transpose -----------------------------------------------------

(: transpose (-> (List (List a)) (List (List a))))
(define (transpose xss)
  (match xss
    [(Nil)             Nil]
    [(Cons (Nil) rest) (transpose rest)]
    [(Cons (Cons x xs) rest)
     (Cons (Cons x (concat-map (lambda (l) (match l [(Nil) Nil] [(Cons h _) (Cons h Nil)])) rest))
           (transpose (Cons xs (fmap (lambda (l) (match l [(Nil) Nil] [(Cons _ t) t])) rest))))]))

;; --- set-like ops (by element equality / order) -------------------

(: delete ((Eq a) => (-> a (-> (List a) (List a)))))
(define (delete x xs)
  (match xs
    [(Nil)      Nil]
    [(Cons h t) (if (== x h) t (Cons h (delete x t)))]))

;; insert into a sorted list, before the first strictly-greater element.
(: insert ((Ord a) => (-> a (-> (List a) (List a)))))
(define (insert x xs)
  (match xs
    [(Nil)      (Cons x Nil)]
    [(Cons h t) (if (> h x) (Cons x xs) (Cons h (insert x t)))]))

(: list-difference ((Eq a) => (-> (List a) (-> (List a) (List a)))))
(define (list-difference xs ys)
  (fold-left (lambda (acc y) (delete y acc)) xs ys))

(: union ((Eq a) => (-> (List a) (-> (List a) (List a)))))
(define (union xs ys)
  (append xs (filter (lambda (y) (not (elem y xs))) (nub ys))))

(: intersect ((Eq a) => (-> (List a) (-> (List a) (List a)))))
(define (intersect xs ys)
  (filter (lambda (x) (elem x ys)) xs))

;; --- sorting by comparator / key -----------------------------------

;; lt? is a strict less-than; the merge is stable (keeps left on ties).
(: merge-by (-> (-> a (-> a Boolean)) (-> (List a) (-> (List a) (List a)))))
(define (merge-by lt? xs ys)
  (match xs
    [(Nil) ys]
    [(Cons hx tx)
     (match ys
       [(Nil) xs]
       [(Cons hy ty)
        (if (lt? hy hx)
            (Cons hy (merge-by lt? xs ty))
            (Cons hx (merge-by lt? tx ys)))])]))

(: sort-by (-> (-> a (-> a Boolean)) (-> (List a) (List a))))
(define (sort-by lt? xs)
  (let ([n (length xs)])
    (if (< n 2)
        xs
        (let ([halves (split-at (racket Integer (n) (quotient n 2)) xs)])
          (merge-by lt? (sort-by lt? (fst halves)) (sort-by lt? (snd halves)))))))

(: sort-on ((Ord b) => (-> (-> a b) (-> (List a) (List a)))))
(define (sort-on key xs)
  (sort-by (lambda (p q) (< (key p) (key q))) xs))

;; --- folds with no seed (Maybe on empty) ---------------------------

(: foldl1 (-> (-> a (-> a a)) (-> (List a) (Maybe a))))
(define (foldl1 f xs)
  (match xs
    [(Nil)      None]
    [(Cons h t) (Some (fold-left f h t))]))

(: foldr1 (-> (-> a (-> a a)) (-> (List a) (Maybe a))))
(define (foldr1 f xs)
  (match xs
    [(Nil)          None]
    [(Cons h (Nil)) (Some h)]
    [(Cons h t)     (match (foldr1 f t)
                      [(Some r) (Some (f h r))]
                      [(None)   None])]))

;; --- generation ----------------------------------------------------

;; bounded `iterate`: [x, f x, f (f x), …] of length n.
(: iterate-n (-> Integer (-> (-> a a) (-> a (List a)))))
(define (iterate-n n f x)
  (if (<= n 0) Nil (Cons x (iterate-n (- n 1) f (f x)))))

;; n copies of xs concatenated.
(: cycle-n (-> Integer (-> (List a) (List a))))
(define (cycle-n n xs)
  (if (<= n 0) Nil (append xs (cycle-n (- n 1) xs))))

(: unfoldr (-> (-> b (Maybe (Pair a b))) (-> b (List a))))
(define (unfoldr f seed)
  (match (f seed)
    [(None)              Nil]
    [(Some (MkPair a b)) (Cons a (unfoldr f b))]))

;; --- combinatorial -------------------------------------------------

(: subsequences (-> (List a) (List (List a))))
(define (subsequences xs)
  (match xs
    [(Nil)      (Cons Nil Nil)]
    [(Cons h t) (let ([rest (subsequences t)])
                  (append rest (fmap (lambda (s) (Cons h s)) rest)))]))

;; each element paired with the list of the others (order preserved).
(: selections (-> (List a) (List (Pair a (List a)))))
(define (selections xs)
  (match xs
    [(Nil)      Nil]
    [(Cons h t) (Cons (MkPair h t)
                      (fmap (lambda (sel)
                              (match sel [(MkPair y ys) (MkPair y (Cons h ys))]))
                            (selections t)))]))

(: permutations (-> (List a) (List (List a))))
(define (permutations xs)
  (match xs
    [(Nil) (Cons Nil Nil)]
    [_     (concat-map (lambda (sel)
                         (match sel
                           [(MkPair x rest)
                            (fmap (lambda (p) (Cons x p)) (permutations rest))]))
                       (selections xs))]))

;; --- mapAccumL -----------------------------------------------------

(: map-accum-l (-> (-> s (-> a (Pair s b))) (-> s (-> (List a) (Pair s (List b))))))
(define (map-accum-l f s xs)
  (match xs
    [(Nil)      (MkPair s Nil)]
    [(Cons h t) (match (f s h)
                  [(MkPair s2 b)
                   (match (map-accum-l f s2 t)
                     [(MkPair s3 bs) (MkPair s3 (Cons b bs))])])]))

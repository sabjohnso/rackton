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

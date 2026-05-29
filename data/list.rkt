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

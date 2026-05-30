#lang racket/base

;; Data.List parity additions to rackton/data/list: safe accessors,
;; membership, building, slicing, folding, zipping, and uniqueness.
;; Results are computed in the Rackton block and compared structurally
;; against constructor-built expecteds (the ADT structs are transparent).

(require rackunit
         "../main.rkt")

(rackton
  (require rackton/data/list)

  ;; ----- accessors (safe, Maybe-returning) ---------------
  (: r-head (Maybe Integer))    (define r-head (head (list 1 2 3)))
  (: r-head0 (Maybe Integer))   (define r-head0 (head (ann Nil (List Integer))))
  (: r-last (Maybe Integer))    (define r-last (last (list 1 2 3)))
  (: r-tail (Maybe (List Integer)))  (define r-tail (tail (list 1 2 3)))
  (: r-init (Maybe (List Integer)))  (define r-init (init (list 1 2 3)))
  (: r-null0 Boolean)           (define r-null0 (empty? (ann Nil (List Integer))))
  (: r-null1 Boolean)           (define r-null1 (empty? (list 1)))

  ;; ----- membership --------------------------------------
  (: r-elem Boolean)            (define r-elem (elem 2 (list 1 2 3)))
  (: r-notelem Boolean)         (define r-notelem (not-elem 9 (list 1 2 3)))
  (: r-lookup (Maybe String))   (define r-lookup
                                  (lookup 2 (list (MkPair 1 "a") (MkPair 2 "b"))))
  (: r-elemidx (Maybe Integer)) (define r-elemidx (elem-index 3 (list 1 2 3)))
  (: r-findidx (Maybe Integer)) (define r-findidx
                                  (find-index (lambda (x) (> x 1)) (list 0 5 2)))

  ;; ----- building ----------------------------------------
  (: r-concat (List Integer))   (define r-concat
                                  (concat (list (list 1 2) (list 3) Nil (list 4))))
  (: r-intersperse (List Integer)) (define r-intersperse
                                  (intersperse 0 (list 1 2 3)))
  (: r-intercalate (List Integer)) (define r-intercalate
                                  (intercalate (list 0 0) (list (list 1) (list 2))))
  (: r-replicate (List Integer)) (define r-replicate (replicate 3 7))
  (: r-range (List Integer))    (define r-range (range 2 5))

  ;; ----- slicing -----------------------------------------
  (: r-takewhile (List Integer)) (define r-takewhile
                                  (take-while (lambda (x) (< x 3)) (list 1 2 3 1)))
  (: r-dropwhile (List Integer)) (define r-dropwhile
                                  (drop-while (lambda (x) (< x 3)) (list 1 2 3 1)))
  (: r-span (Pair (List Integer) (List Integer)))
  (define r-span (span (lambda (x) (< x 3)) (list 1 2 3 1)))
  (: r-break (Pair (List Integer) (List Integer)))
  (define r-break (break (lambda (x) (>= x 3)) (list 1 2 3 1)))
  (: r-partition (Pair (List Integer) (List Integer)))
  (define r-partition (partition (lambda (x) (> x 2)) (list 1 4 2 5)))

  ;; ----- folding / predicates ----------------------------
  (: r-fold-left Integer)           (define r-fold-left
                                  (fold-left (lambda (acc x) (- acc x)) 10 (list 1 2 3)))
  (: r-all Boolean)             (define r-all (all? (lambda (x) (> x 0)) (list 1 2 3)))
  (: r-any Boolean)             (define r-any (any? (lambda (x) (> x 2)) (list 1 2 3)))
  (: r-andl Boolean)            (define r-andl (and-list (list #t #t #f)))
  (: r-orl Boolean)             (define r-orl (or-list (list #f #f #t)))
  (: r-max (Maybe Integer))     (define r-max (maximum (list 3 1 4 1 5)))
  (: r-min (Maybe Integer))     (define r-min (minimum (list 3 1 4 1 5)))

  ;; ----- zipping / uniqueness ----------------------------
  (: r-zipwith (List Integer))  (define r-zipwith
                                  (zip-with (lambda (a b) (+ a b)) (list 1 2 3) (list 10 20)))
  (: r-unzip (Pair (List Integer) (List String)))
  (define r-unzip (unzip (list (MkPair 1 "a") (MkPair 2 "b"))))
  (: r-nub (List Integer))      (define r-nub (nub (list 1 2 1 3 2))))

;; ---------- assertions ---------------------------------------

(test-case "safe accessors"
  (check-equal? r-head  (Some 1))
  (check-equal? r-head0 None)
  (check-equal? r-last  (Some 3))
  (check-equal? r-tail  (Some (Cons 2 (Cons 3 Nil))))
  (check-equal? r-init  (Some (Cons 1 (Cons 2 Nil))))
  (check-equal? r-null0 #t)
  (check-equal? r-null1 #f))

(test-case "membership"
  (check-equal? r-elem    #t)
  (check-equal? r-notelem #t)
  (check-equal? r-lookup  (Some "b"))
  (check-equal? r-elemidx (Some 2))
  (check-equal? r-findidx (Some 1)))

(test-case "building"
  (check-equal? r-concat      (Cons 1 (Cons 2 (Cons 3 (Cons 4 Nil)))))
  (check-equal? r-intersperse (Cons 1 (Cons 0 (Cons 2 (Cons 0 (Cons 3 Nil))))))
  (check-equal? r-intercalate (Cons 1 (Cons 0 (Cons 0 (Cons 2 Nil)))))
  (check-equal? r-replicate   (Cons 7 (Cons 7 (Cons 7 Nil))))
  (check-equal? r-range       (Cons 2 (Cons 3 (Cons 4 (Cons 5 Nil))))))

(test-case "slicing"
  (check-equal? r-takewhile (Cons 1 (Cons 2 Nil)))
  (check-equal? r-dropwhile (Cons 3 (Cons 1 Nil)))
  (check-equal? r-span  (MkPair (Cons 1 (Cons 2 Nil)) (Cons 3 (Cons 1 Nil))))
  (check-equal? r-break (MkPair (Cons 1 (Cons 2 Nil)) (Cons 3 (Cons 1 Nil))))
  (check-equal? r-partition (MkPair (Cons 4 (Cons 5 Nil)) (Cons 1 (Cons 2 Nil)))))

(test-case "folding / predicates"
  (check-equal? r-fold-left 4)          ; ((10-1)-2)-3
  (check-equal? r-all  #t)
  (check-equal? r-any  #t)
  (check-equal? r-andl #f)
  (check-equal? r-orl  #t)
  (check-equal? r-max  (Some 5))
  (check-equal? r-min  (Some 1)))

(test-case "zipping / uniqueness"
  (check-equal? r-zipwith (Cons 11 (Cons 22 Nil)))
  (check-equal? r-unzip   (MkPair (Cons 1 (Cons 2 Nil)) (Cons "a" (Cons "b" Nil))))
  (check-equal? r-nub     (Cons 1 (Cons 2 (Cons 3 Nil)))))

;; ===== finishing batch ==============================================

(rackton
  (require rackton/data/list)

  ;; scans
  (: q-scanl (List Integer))
  (define q-scanl (scanl (lambda (acc x) (+ acc x)) 0 (list 1 2 3)))
  (: q-scanr (List Integer))
  (define q-scanr (scanr (lambda (x acc) (+ x acc)) 0 (list 1 2 3)))

  ;; grouping / sublists
  (: q-group (List (List Integer)))
  (define q-group (group (list 1 1 2 3 3)))
  (: q-inits (List (List Integer)))
  (define q-inits (inits (list 1 2)))
  (: q-tails (List (List Integer)))
  (define q-tails (tails (list 1 2)))

  ;; prefix / suffix / infix / strip
  (: q-prefix Boolean) (define q-prefix (prefix? (list 1 2) (list 1 2 3)))
  (: q-suffix Boolean) (define q-suffix (suffix? (list 2 3) (list 1 2 3)))
  (: q-infix Boolean)  (define q-infix  (infix?  (list 2 3) (list 1 2 3 4)))
  (: q-strip (Maybe (List Integer)))
  (define q-strip (strip-prefix (list 1 2) (list 1 2 3)))

  ;; transpose
  (: q-transpose (List (List Integer)))
  (define q-transpose (transpose (list (list 1 2 3) (list 4 5 6))))

  ;; set-like
  (: q-delete (List Integer)) (define q-delete (delete 2 (list 1 2 3 2)))
  (: q-insert (List Integer)) (define q-insert (insert 3 (list 1 2 4 5)))
  (: q-diff (List Integer))   (define q-diff (list-difference (list 1 2 3 2) (list 2)))
  (: q-union (List Integer))  (define q-union (union (list 1 2 3) (list 2 4)))
  (: q-intersect (List Integer)) (define q-intersect (intersect (list 1 2 3 4) (list 2 4 6)))

  ;; sorting
  (: q-sortby (List Integer))
  (define q-sortby (sort-by (lambda (a b) (> a b)) (list 3 1 2)))
  (: q-sorton (List Integer))
  (define q-sorton (sort-on (lambda (x) (- 10 x)) (list 1 2 3)))

  ;; folds
  (: q-foldl1 (Maybe Integer))
  (define q-foldl1 (foldl1 (lambda (a b) (- a b)) (list 10 1 2)))
  (: q-foldr1 (Maybe Integer))
  (define q-foldr1 (foldr1 (lambda (a b) (- a b)) (list 10 1 2)))

  ;; generation
  (: q-iterate (List Integer))
  (define q-iterate (iterate-n 4 (lambda (x) (* x 2)) 1))
  (: q-cycle (List Integer))
  (define q-cycle (cycle-n 3 (list 1 2)))
  (: q-unfoldr (List Integer))
  (define q-unfoldr (unfoldr (lambda (n) (if (> n 0) (Some (MkPair n (- n 1))) None)) 3))

  ;; nub-by / combinatorial / mapAccumL
  (: q-nubby (List Integer))
  (define q-nubby (nub-by (lambda (a b) (== (mod a 3) (mod b 3))) (list 1 4 2 5)))
  (: q-subseq (List (List Integer)))
  (define q-subseq (subsequences (list 1 2)))
  (: q-perms (List (List Integer)))
  (define q-perms (permutations (list 1 2)))
  (: q-mapaccum (Pair Integer (List Integer)))
  (define q-mapaccum
    (map-accum-l (lambda (s x) (MkPair (+ s x) (* x 10))) 0 (list 1 2 3))))

;; ---------- assertions (finishing batch) ----------------------------

;; racket helpers that build a rackton List from racket values
;; (can't use rackton's `foldr` here — it dispatches on a rackton
;; container, not a racket list).
(define (lst . xs)
  (let loop ([xs xs]) (if (null? xs) Nil (Cons (car xs) (loop (cdr xs))))))
(define llst lst)

(test-case "scans"
  (check-equal? q-scanl (lst 0 1 3 6))
  (check-equal? q-scanr (lst 6 5 3 0)))

(test-case "grouping / sublists"
  (check-equal? q-group (llst (lst 1 1) (lst 2) (lst 3 3)))
  (check-equal? q-inits (llst Nil (lst 1) (lst 1 2)))
  (check-equal? q-tails (llst (lst 1 2) (lst 2) Nil)))

(test-case "prefix / suffix / infix / strip"
  (check-equal? q-prefix #t)
  (check-equal? q-suffix #t)
  (check-equal? q-infix  #t)
  (check-equal? q-strip  (Some (lst 3))))

(test-case "transpose"
  (check-equal? q-transpose (llst (lst 1 4) (lst 2 5) (lst 3 6))))

(test-case "set-like"
  (check-equal? q-delete    (lst 1 3 2))
  (check-equal? q-insert    (lst 1 2 3 4 5))
  (check-equal? q-diff      (lst 1 3 2))
  (check-equal? q-union     (lst 1 2 3 4))
  (check-equal? q-intersect (lst 2 4)))

(test-case "sorting"
  (check-equal? q-sortby (lst 3 2 1))
  (check-equal? q-sorton (lst 3 2 1)))

(test-case "folds with seed-from-list"
  (check-equal? q-foldl1 (Some 7))     ; (10-1)-2
  (check-equal? q-foldr1 (Some 11)))   ; 10-(1-2)

(test-case "generation"
  (check-equal? q-iterate (lst 1 2 4 8))
  (check-equal? q-cycle   (lst 1 2 1 2 1 2))
  (check-equal? q-unfoldr (lst 3 2 1)))

(test-case "nub-by / combinatorial / mapAccumL"
  (check-equal? q-nubby   (lst 1 2))
  (check-equal? q-subseq  (llst Nil (lst 2) (lst 1) (lst 1 2)))
  (check-equal? q-perms   (llst (lst 1 2) (lst 2 1)))
  (check-equal? q-mapaccum (MkPair 6 (lst 10 20 30))))

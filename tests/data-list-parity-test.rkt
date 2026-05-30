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

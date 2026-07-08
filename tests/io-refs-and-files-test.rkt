#lang rackton

;; End-to-end: refs in IO, file I/O, list helpers, sort, pair
;; helpers, and `:deriving Functor`.

(require rackton/data/list
         rackton/data/tuple
         rackton/system
         "../unit.rkt")

;; Mutable counter via Ref.  loop-step must precede its caller.
(: loop-step (-> (Ref Integer) (-> Integer (IO Unit))))
(define (loop-step r stop)
  (do [n <- (read-ref r)]
    (if (< n stop)
      (do [_ <- (write-ref r (+ n 1))]
        (loop-step r stop))
      (pure-io Unit))))

(: count-from-to (-> Integer (-> Integer (IO Integer))))
(define (count-from-to start stop)
  (do [r <- (make-ref start)]
    [_ <- (loop-step r stop)]
    (read-ref r)))

;; List helpers
(define xs       (Cons 3 (Cons 1 (Cons 4 (Cons 1 (Cons 5 (Cons 9 Nil)))))))
(define rev      (reverse xs))
(define first-3  (take 3 xs))
(define last-3   (drop 3 xs))
(define sorted-xs (sort xs))
(define found    (find (lambda (n) (== n 5)) xs))
(define missing  (find (lambda (n) (== n 99)) xs))

;; Pair helpers
(define p     (Pair "key" 42))
(define p-fst (fst p))
(define p-snd (snd p))
(define p-sw  (swap p))

;; :deriving Functor on a single-tparam tree
(data (Tree a)
  Leaf
  (Node (Tree a) a (Tree a))
  :deriving Functor Eq Show)

(define t1     (Node Leaf 1 (Node Leaf 2 (Node Leaf 3 Leaf))))
(define t1-x2  (fmap (lambda (n) (* n 2)) t1))

;; File round-trip on a fixed temp path.
(: file-roundtrip (IO String))
(define file-roundtrip
  (do [_ <- (write-file "/tmp/rackton-fileio-test.txt" "rackton file io rules")]
    (read-file "/tmp/rackton-fileio-test.txt")))

;; ----- assertions -------------------------------------------------

(: suite (List Test))
(define suite
  (list
    (it "Ref counter loops 5 → 10"
        (check-equal? (run-io (count-from-to 5 10)) 10))
    (it "reverse"
        (check-equal? rev (Cons 9 (Cons 5 (Cons 1 (Cons 4 (Cons 1 (Cons 3 Nil))))))))
    (it "take / drop"
        (all-checks
          (list (check-equal? first-3 (Cons 3 (Cons 1 (Cons 4 Nil))))
                (check-equal? last-3  (Cons 1 (Cons 5 (Cons 9 Nil)))))))
    (it "sort"
        (check-equal? sorted-xs
                      (Cons 1 (Cons 1 (Cons 3 (Cons 4 (Cons 5 (Cons 9 Nil))))))))
    (it "find"
        (all-checks
          (list (check-equal? found   (Some 5))
                (check-equal? missing None))))
    (it "fst / snd / swap"
        (all-checks
          (list (check-equal? p-fst "key")
                (check-equal? p-snd 42)
                (check-equal? p-sw  (Pair 42 "key")))))
    (it "fmap over a tree (derived)"
        (check-equal?
          t1-x2
          (Node Leaf 2 (Node Leaf 4 (Node Leaf 6 Leaf)))))
    (it "round-trip file write / read"
        (check-equal? (run-io file-roundtrip) "rackton file io rules"))))

(: test-main (IO Unit))
(define test-main (run-suite "io-refs-and-files" suite))

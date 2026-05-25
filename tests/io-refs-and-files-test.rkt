#lang racket/base

;; End-to-end: refs in IO, file I/O, list helpers, sort, pair
;; helpers, and `#:deriving Functor`.

(require rackunit
         racket/file
         "../main.rkt")

(rackton
  ;; Mutable counter via Ref.  loop-step must precede its caller.
  (: loop-step (-> (Ref Integer) (-> Integer (IO Unit))))
  (define (loop-step r stop)
    (do [n <- (read-ref r)]
      (if (< n stop)
          (do [_ <- (write-ref r (+ n 1))]
            (loop-step r stop))
          (pure-io MkUnit))))

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
  (define p     (MkPair "key" 42))
  (define p-fst (fst p))
  (define p-snd (snd p))
  (define p-sw  (swap p))

  ;; #:deriving Functor on a single-tparam tree
  (define-data (Tree a)
    Leaf
    (Node (Tree a) a (Tree a))
    #:deriving Functor)

  (define t1     (Node Leaf 1 (Node Leaf 2 (Node Leaf 3 Leaf))))
  (define t1-x2  (fmap (lambda (n) (* n 2)) t1))

  ;; Bridges so Racket-side tests can call file I/O on temp paths.
  (: rackton-runtime-write (-> String (-> String (IO Unit))))
  (define (rackton-runtime-write path s) (write-file path s))

  (: rackton-runtime-read (-> String (IO String)))
  (define (rackton-runtime-read path) (read-file path)))

;; ----- mutable refs run via run-io --------------------------------

(test-case "Ref counter loops 5 → 10"
  (check-equal? (run-io (count-from-to 5 10)) 10))

;; ----- list helpers -----------------------------------------------

(test-case "reverse"
  (check-equal? rev (Cons 9 (Cons 5 (Cons 1 (Cons 4 (Cons 1 (Cons 3 Nil))))))))

(test-case "take / drop"
  (check-equal? first-3 (Cons 3 (Cons 1 (Cons 4 Nil))))
  (check-equal? last-3  (Cons 1 (Cons 5 (Cons 9 Nil)))))

(test-case "sort"
  (check-equal? sorted-xs
                (Cons 1 (Cons 1 (Cons 3 (Cons 4 (Cons 5 (Cons 9 Nil))))))))

(test-case "find"
  (check-equal? found   (Some 5))
  (check-equal? missing None))

;; ----- pair helpers -----------------------------------------------

(test-case "fst / snd / swap"
  (check-equal? p-fst "key")
  (check-equal? p-snd 42)
  (check-equal? p-sw  (MkPair 42 "key")))

;; ----- derived Functor --------------------------------------------

(test-case "fmap over a tree (derived)"
  (check-equal?
   t1-x2
   (Node Leaf 2 (Node Leaf 4 (Node Leaf 6 Leaf)))))

;; ----- file I/O ---------------------------------------------------

(test-case "round-trip file write / read"
  (define tmp (make-temporary-file "rackton-fileio-~a"))
  (run-io
   (rackton-runtime-write tmp "rackton file io rules"))
  (check-equal? (run-io (rackton-runtime-read tmp)) "rackton file io rules")
  (delete-file tmp))


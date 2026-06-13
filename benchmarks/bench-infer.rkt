#lang racket/base

;; Inference micro-benchmark.  Times infer-program on a fixed
;; representative user program against the prelude env, the same call the
;; rackton macro makes for every user module.  Workload uses prelude-stable
;; constructs so it runs identically on the pre-monad baseline and on HEAD.
;;
;; Run it directly:  racket benchmarks/bench-infer.rkt
;; The measurement lives in `(module+ main …)` so it does NOT run during
;; `raco test` / `raco setup`; baseline for A/B comparisons is 05d23e8.

(require "../private/infer.rkt"    ; infer-program
         "../private/prelude.rkt"  ; prelude-env
         "../private/surface.rkt") ; parse-top

(define WORKLOAD
  '(;; polymorphic recursive list functions (let-polymorphism, ADT patterns)
    (: bm-rev (-> (List a) (List a)))
    (define (bm-rev xs)
      (match xs
        [(Nil)      Nil]
        [(Cons h t) (append (bm-rev t) (Cons h Nil))]))

    (: bm-len (-> (List a) Integer))
    (define (bm-len xs)
      (match xs
        [(Nil)      0]
        [(Cons _ t) (+ 1 (bm-len t))]))

    (: bm-map (-> (-> a b) (List a) (List b)))
    (define (bm-map f xs)
      (match xs
        [(Nil)      Nil]
        [(Cons h t) (Cons (f h) (bm-map f t))]))

    (: bm-filter (-> (-> a Boolean) (List a) (List a)))
    (define (bm-filter p xs)
      (match xs
        [(Nil)      Nil]
        [(Cons h t) (if (p h) (Cons h (bm-filter p t)) (bm-filter p t))]))

    (: bm-sum (-> (List Integer) Integer))
    (define (bm-sum xs)
      (match xs
        [(Nil)      0]
        [(Cons h t) (+ h (bm-sum t))]))

    ;; concrete class-method dispatch (== / <) — exercises resolution
    (: bm-elem (-> Integer (List Integer) Boolean))
    (define (bm-elem x xs)
      (match xs
        [(Nil)      #f]
        [(Cons h t) (if (== x h) #t (bm-elem x t))]))

    (: bm-ilist-eq (-> (List Integer) (-> (List Integer) Boolean)))
    (define (bm-ilist-eq xs ys)
      (match xs
        [(Nil)       (match ys [(Nil) #t] [(Cons _ _) #f])]
        [(Cons a as) (match ys
                       [(Nil)       #f]
                       [(Cons b bs) (if (== a b) (bm-ilist-eq as bs) #f)])]))

    ;; ADT + polymorphic traversal
    (data (BTree a) BLeaf (BBranch (BTree a) a (BTree a)))

    (: bm-tsize (-> (BTree a) Integer))
    (define (bm-tsize t)
      (match t
        [(BLeaf)         0]
        [(BBranch l _ r) (+ 1 (+ (bm-tsize l) (bm-tsize r)))]))

    (: bm-tinsert (-> Integer (BTree Integer) (BTree Integer)))
    (define (bm-tinsert x t)
      (match t
        [(BLeaf)         (BBranch BLeaf x BLeaf)]
        [(BBranch l y r) (if (< x y)
                             (BBranch (bm-tinsert x l) y r)
                             (BBranch l y (bm-tinsert x r)))]))

    ;; higher-order composition + closures
    (: bm-compose (-> (-> b c) (-> a b) (-> a c)))
    (define (bm-compose f g) (lambda (x) (f (g x))))

    (: bm-twice (-> (-> a a) a a))
    (define (bm-twice f x) (f (f x)))))

(define forms (for/list ([f (in-list WORKLOAD)]) (parse-top (datum->syntax #f f))))

(define N 200)

(module+ main
  ;; sanity: the workload must type-check
  (void (infer-program forms prelude-env))

  ;; warm up, then measure N inference runs
  (for ([i (in-range 5)]) (void (infer-program forms prelude-env)))
  (collect-garbage) (collect-garbage) (collect-garbage)

  (define-values (_res cpu real gc)
    (time-apply (lambda ()
                  (for ([i (in-range N)]) (void (infer-program forms prelude-env))))
                '()))

  (printf "infer-program x ~a forms, N=~a iterations\n" (length forms) N)
  (printf "  cpu=~ams  real=~ams  gc=~ams\n" cpu real gc)
  (printf "  per-iter: cpu=~ams  gc=~ams\n"
          (exact->inexact (/ cpu N)) (exact->inexact (/ gc N))))

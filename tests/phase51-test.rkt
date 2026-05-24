#lang racket/base

;; Phase 51: full rank-N higher-rank polymorphism.  A function may
;; accept an argument whose declared type is itself polymorphic, and
;; call that argument at several distinct concrete types inside the
;; same body.  In a rank-1 system the argument's tvar would unify
;; with whichever concrete type appeared first, blocking re-use at
;; a different type later — that's the regression these tests guard
;; against.

(require rackunit
         "../main.rkt")

(rackton
  ;; ----- 51.A Classic rank-2 ----------------------------------
  ;; `pair-id` takes a polymorphic identity function and applies it
  ;; at BOTH Integer and String inside the same body.

  (: pair-id (-> (All (a) (-> a a)) (Pair Integer String)))
  (define (pair-id f)
    (MkPair (f 7) (f "hi")))

  (: r-pair (Pair Integer String))
  (define r-pair (pair-id (lambda (x) x)))

  ;; ----- 51.B rank-2 with non-identity --------------------------
  ;; A polymorphic argument can be a non-identity function — here a
  ;; const that depends on its first arg only.

  (: pair-fst (-> (All (a) (-> a (-> a a))) (Pair Integer String)))
  (define (pair-fst f)
    (MkPair (f 1 99) (f "yes" "no")))

  (: r-fst (Pair Integer String))
  (define r-fst (pair-fst (lambda (x y) x)))

  ;; ----- 51.C extracting field values ---------------------------
  ;; Re-use the same polymorphic id to feed two separately-typed
  ;; downstream consumers.

  (: int-twice (-> (All (a) (-> a a)) Integer))
  (define (int-twice f) (+ (f 3) (f 4)))

  (: r-twice Integer)
  (define r-twice (int-twice (lambda (x) x)))

  ;; ----- helper for value-level checks --------------------------

  (: pair-eq? (-> (Pair Integer String) (-> Integer (-> String Boolean))))
  (define (pair-eq? p a b)
    (match p
      [(MkPair x y) (and (= x a) (== y b))])))

;; ----- assertions -------------------------------------------------

(test-case "rank-2: polymorphic id at Integer and String"
  (check-true (pair-eq? r-pair 7 "hi")))

(test-case "rank-2: polymorphic non-identity at two types"
  (check-true (pair-eq? r-fst 1 "yes")))

(test-case "rank-2: polymorphic arg called twice at one return type"
  (check-equal? r-twice 7))

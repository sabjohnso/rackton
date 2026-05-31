#lang rackton

;; rackton/data/tuple — Data.Tuple.  `fst` / `snd` stay in the prelude;
;; `swap` moves here (Phase 2 slim).  `curry` / `uncurry` convert
;; between a Pair-taking function and its two-argument form.

(provide (all-defined-out))

(: swap (-> (Pair a b) (Pair b a)))
(define (swap p) (match p [(MkPair a b) (MkPair b a)]))

;; curry: turn a function on a Pair into a two-argument function.
(: curry (-> (-> (Pair a b) c) (-> a (-> b c))))
(define (curry f a b) (f (MkPair a b)))

;; uncurry: turn a two-argument function into one taking a Pair.
(: uncurry (-> (-> a (-> b c)) (-> (Pair a b) c)))
(define (uncurry f p) (match p [(MkPair a b) (f a b)]))

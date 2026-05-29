#lang rackton

;; rackton/data/tuple — Data.Tuple.  `fst` / `snd` stay in the prelude;
;; `swap` moves here (Phase 2 slim).

(provide (all-defined-out))

(: swap (-> (Pair a b) (Pair b a)))
(define (swap p) (match p [(MkPair a b) (MkPair b a)]))

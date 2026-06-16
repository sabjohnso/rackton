#lang racket/base

;; Phase 2b — `Pair` is the binary tuple.  `(Pair a b)` and the 2-element
;; `(Tuple a b)` are the SAME type (canonical head `Pair`), so:
;;   - a `Pair` value is `tref`-able and the tuple Eq/Ord/Show apply;
;;   - `(tuple a b)` and `(Pair a b)` are interchangeable as values and types;
;;   - `Pair` is still a binary type constructor, so the higher-kinded
;;     instances (Bifunctor, Prod) keep working on pairs.

(require rackunit
         (for-syntax racket/base)
         "../main.rkt")

;; ----- Pair and 2-Tuple are interchangeable ------------------------

(rackton
  ;; Built with `Pair`, annotated as a `Tuple`.
  (: p (Tuple Integer String))
  (define p (Pair 1 "a"))

  ;; Built with `tuple`, annotated as a `Pair`.
  (: q (Pair Integer String))
  (define q (tuple 1 "a"))

  ;; `tref` works on a Pair value; `fst`/`snd` work on a tuple value.
  (: p0 Integer) (define p0 (tref p 0))
  (: q-fst Integer) (define q-fst (fst q))
  (: q-snd String)  (define q-snd (snd q))

  ;; Same type both ways → comparable.
  (: same Boolean) (define same (== p q)))

(test-case "Pair and 2-Tuple interchange as values and types"
  (check-equal? p0 1)
  (check-equal? q-fst 1)
  (check-equal? q-snd "a")
  (check-true same))

;; ----- Eq / Ord / Show on pairs come from the tuple instances ------

(rackton
  (: eqp Boolean)  (define eqp (== (Pair 1 2) (Pair 1 2)))
  (: ltp Boolean)  (define ltp (< (Pair 1 2) (Pair 1 3)))
  (: shp String)   (define shp (show (Pair 1 2))))

(test-case "pair Eq/Ord/Show via tuple instances"
  (check-true eqp)
  (check-true ltp)
  (check-equal? shp "(1, 2)"))

;; ----- higher-kinded instances on Pair still work ------------------

(rackton
  (: bm (Pair Integer Integer))
  (define bm (bimap (lambda (x) (+ x 1)) (lambda (y) (* y 2)) (Pair 10 20)))
  (: bm0 Integer) (define bm0 (tref bm 0))
  (: bm1 Integer) (define bm1 (tref bm 1))

  (: pf Integer) (define pf (prod-fst (Pair 7 "x")))
  (: ps String)  (define ps (prod-snd (Pair 7 "x"))))

(test-case "Bifunctor / Prod on Pair survive the migration"
  (check-equal? bm0 11)
  (check-equal? bm1 40)
  (check-equal? pf 7)
  (check-equal? ps "x"))

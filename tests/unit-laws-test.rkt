#lang racket/base

;; Phase 7: algebraic-law bundles.
;;
;; The bundles turn a generator into a Test group of properties.  We
;; check that lawful instances pass (Eq/Ord on Integer; Monoid on String
;; with identity ""), and that a deliberately NON-associative Semigroup
;; is caught by semigroup-laws (subtraction is not associative).
;;
;; Everything is reached through the single `rackton/unit` entry point
;; (../unit.rkt), so instance coherence sees one import path.

(require (prefix-in ru: rackunit)
         racket/port
         "../main.rkt")

(rackton
  (require "../unit.rkt")

  ;; Lawful base-type instances.
  (: int-eq-summary (IO Summary))
  (define int-eq-summary  (run-tests (eq-laws (int-range -50 50))))

  (: int-ord-summary (IO Summary))
  (define int-ord-summary (run-tests (ord-laws (int-range -50 50))))

  (: string-monoid-summary (IO Summary))
  (define string-monoid-summary
    (run-tests (monoid-laws gen-string "")))

  ;; A non-associative Semigroup: (a mappend b) = a - b.  (x-y)-z ≠ x-(y-z).
  (data Broken (MkBroken Integer))

  (instance (Eq Broken)
    (define (== a b)
      (match a [(MkBroken x) (match b [(MkBroken y) (== x y)])])))

  (instance (Show Broken)
    (define (show a) (match a [(MkBroken x) (integer->string x)])))

  (instance (Semigroup Broken)
    (define (mappend a b)
      (match a [(MkBroken x) (match b [(MkBroken y) (MkBroken (- x y))])])))

  (: gen-broken (Gen Broken))
  (define gen-broken (fmap (lambda (n) (MkBroken n)) (int-range 1 20)))

  (: broken-summary (IO Summary))
  (define broken-summary (run-tests (semigroup-laws gen-broken))))

(define (run-counts io)
  (define s (run-io io))
  (cons (summary-passed s) (summary-failed s)))

;; Capture output so the runner's lines don't clutter the test log.
(define eq-counts     #f)
(define ord-counts    #f)
(define monoid-counts #f)
(define broken-counts #f)
(define captured-output
  (with-output-to-string
    (lambda ()
      (set! eq-counts     (run-counts int-eq-summary))
      (set! ord-counts    (run-counts int-ord-summary))
      (set! monoid-counts (run-counts string-monoid-summary))
      (set! broken-counts (run-counts broken-summary)))))

(ru:test-case "Eq laws hold for Integer"
  (ru:check-equal? (cdr eq-counts) 0)
  (ru:check-true   (> (car eq-counts) 0)))

(ru:test-case "Ord laws hold for Integer"
  (ru:check-equal? (cdr ord-counts) 0)
  (ru:check-true   (> (car ord-counts) 0)))

(ru:test-case "Monoid laws hold for String with identity \"\""
  (ru:check-equal? (cdr monoid-counts) 0)
  (ru:check-true   (> (car monoid-counts) 0)))

(ru:test-case "semigroup-laws catches a non-associative mappend"
  (ru:check-true (> (cdr broken-counts) 0)))

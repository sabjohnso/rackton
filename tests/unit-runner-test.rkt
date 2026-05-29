#lang racket/base

;; Phase 6: the BDD test tree and the IO runner.
;;
;; describe/it/context build an immutable Test value; run-tests walks it
;; in IO, printing a hierarchical ok/FAIL report and returning a Summary
;; of pass/fail counts.  A failing property reports its seed for replay.
;; A property that panics is contained (reported as a failure) rather
;; than aborting the whole run.

(require (prefix-in ru: rackunit)
         racket/port
         "../main.rkt")

;; Substring check via racket/base regexp (racket/string's
;; string-contains? collides with a name re-exported by main.rkt).
(define (has? haystack needle)
  (and (regexp-match? (regexp (regexp-quote needle)) haystack) #t))

(rackton
  (require "../unit/tree.rkt")

  ;; A suite with one passing and one failing unit check.
  (: arith-suite Test)
  (define arith-suite
    (describe "arithmetic"
      (it "one plus one" (check-equal? (+ 1 1) 2))
      (it "broken"       (check-equal? (+ 1 1) 3))))

  (: run-arith (IO Summary))
  (define run-arith (run-tests arith-suite))

  ;; A property that always fails; shrinks to 0 and reports its seed.
  (: failing-prop Test)
  (define failing-prop
    (it-prop "never negative"
             (for-all (int-range 0 100) (lambda (x) (< x 0)))))

  (: run-failing (IO Summary))
  (define run-failing (run-tests failing-prop))

  ;; A property whose body panics — must be contained as a failure.
  (: panic-prop Test)
  (define panic-prop
    (it-prop "panics"
             (for-all (int-range 0 10) (lambda (x) (panic "boom")))))

  (: run-panic (IO Summary))
  (define run-panic (run-tests panic-prop)))

;; ----- arithmetic suite: 1 pass, 1 fail, hierarchical output --------
(define arith-summary #f)
(define arith-out
  (with-output-to-string (lambda () (set! arith-summary (run-io run-arith)))))

(ru:test-case "summary counts one pass and one fail"
  (ru:check-equal? (summary-passed arith-summary) 1)
  (ru:check-equal? (summary-failed arith-summary) 1))

(ru:test-case "output is hierarchical with ok/FAIL lines"
  (ru:check-true (has? arith-out "arithmetic"))
  (ru:check-true (has? arith-out "ok - one plus one"))
  (ru:check-true (has? arith-out "FAIL - broken")))

;; ----- failing property: reports a seed ----------------------------
(define fail-out
  (with-output-to-string (lambda () (run-io run-failing))))

(ru:test-case "failing property reports a seed for replay"
  (ru:check-true (has? fail-out "FAIL - never negative"))
  (ru:check-true (has? fail-out "seed=")))

;; ----- panicking property: contained, not aborting -----------------
(define panic-summary (run-io run-panic))

(ru:test-case "a panicking property is contained as a failure"
  (ru:check-equal? (summary-failed panic-summary) 1)
  (ru:check-equal? (summary-passed panic-summary) 0))

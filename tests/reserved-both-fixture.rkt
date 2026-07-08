#lang rackton

;; Fixture for tests/reserved-entry-points-test.rkt — defines both
;; `main` and `test-main`, so both submodules must be emitted and
;; each must run independently of the other.

(: main (IO Unit))
(define main (println "both: main ran"))

(: test-main (IO Unit))
(define test-main (println "both: test-main ran"))

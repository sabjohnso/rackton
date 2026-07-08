#lang rackton

;; Fixture for tests/reserved-entry-points-test.rkt — defines only
;; `test-main`.  Rackton should emit `(module+ test (run-io
;; test-main))` for this file and no `main` submodule.

(: test-main (IO Unit))
(define test-main (println "test-main ran"))

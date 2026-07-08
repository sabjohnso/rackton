#lang rackton

;; Fixture for tests/reserved-entry-points-test.rkt — defines only
;; `main`.  Rackton should emit `(module+ main (run-io main))` for
;; this file and no `test` submodule.

(: main (IO Unit))
(define main (println "main ran"))

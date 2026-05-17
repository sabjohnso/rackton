#lang racket/base

;; Verifies that #lang rackton modules compile, run, and export their
;; definitions to other Racket modules.

(require rackunit
         "sample.rkt")

(test-case "id from #lang rackton sample"
  (check-equal? (id 42) 42)
  (check-equal? (id "x") "x"))

(test-case "fact from #lang rackton sample"
  (check-equal? (fact 5) 120))

(test-case "ADT + match from #lang rackton sample"
  (check-equal? (from-maybe 0 None)     0)
  (check-equal? (from-maybe 0 (Some 7)) 7))

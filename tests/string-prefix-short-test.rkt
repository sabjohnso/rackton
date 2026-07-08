#lang rackton

;; Regression: string-prefix? must return #f (not crash) when the
;; string is shorter than the prefix.
;;
;; The runtime used the strict Rackton `and` (a function) instead of
;; Racket's short-circuiting `and`, so the length guard did not protect
;; the substring and `(string-prefix? "--" "x")` threw
;;   substring: ending index is out of range.

(require "../unit.rkt")

(: suite (List Test))
(define suite
  (list
    (it "string shorter than prefix is #f, not a crash"
        (all-checks
          (list (check-false (string-prefix? "--" "x"))
                (check-false (string-prefix? "ab" ""))
                (check-true  (string-prefix? "--" "--count"))
                (check-true  (string-prefix? "" "anything")))))))

(: test-main (IO Unit))
(define test-main (run-suite "string-prefix? short strings" suite))

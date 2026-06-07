#lang racket/base

;; Terminal-width-aware error formatting (REPL only).
;;
;; The type-error pretty-printer wraps long types at a column budget.
;; That budget is the `current-type-columns` parameter (default 66, which
;; keeps batch compiles and the test suite deterministic).  At a live
;; REPL the budget is refreshed from the terminal width; everywhere else
;; it stays at the default.
;;
;; These tests pin the pure pieces (width arithmetic, the parameter
;; wiring, the read-interaction hook) without needing a real terminal —
;; the terminal query is injected.

(require rackunit
         "../private/term.rkt"
         "../private/types.rkt")

;; ----- pure: compute-display-columns --------------------------------
;; (compute-display-columns env-col terminal? stty-line) → cols | #f
;; env-col   : the raw `COLUMNS` value (string or #f)
;; terminal? : is the REPL input actually a terminal?
;; stty-line : the raw `stty size` output ("rows cols") or #f

(test-case "COLUMNS, when a positive integer, wins"
  (check-equal? (compute-display-columns "100" #t "24 80") 100))

(test-case "falls back to stty size when COLUMNS is unset, on a terminal"
  (check-equal? (compute-display-columns #f #t "24 80") 80))

(test-case "non-terminal with no COLUMNS yields #f (use the default)"
  (check-equal? (compute-display-columns #f #f "24 80") #f))

(test-case "junk COLUMNS is ignored, falling back to stty"
  (check-equal? (compute-display-columns "abc" #t "24 80") 80))

(test-case "non-positive COLUMNS is ignored"
  (check-equal? (compute-display-columns "0" #t "24 80") 80))

(test-case "terminal but no stty output yields #f"
  (check-equal? (compute-display-columns #f #t #f) #f))

(test-case "unparseable stty output yields #f"
  (check-equal? (compute-display-columns #f #t "garbage") #f))

;; ----- pure: columns->type-budget -----------------------------------
;; Budget = terminal columns minus the label width (13 = "  expected: "
;; plus a margin), floored so a tiny terminal never degrades to one
;; token per line.

(test-case "79-column terminal reproduces the historical budget of 66"
  (check-equal? (columns->type-budget 79) 66))

(test-case "wider terminal widens the budget"
  (check-equal? (columns->type-budget 120) 107))

(test-case "narrow terminal is floored, not driven below the minimum"
  (check-equal? (columns->type-budget 45) 40))

(test-case "very wide terminal"
  (check-equal? (columns->type-budget 200) 187))

;; ----- parameter wiring: format honors current-type-columns ---------

;; A long (eight-argument) arrow type — wide enough to wrap at a small
;; budget but to fit on one line at a large one.
(define long-ty
  (let loop ([n 8])
    (if (= n 0) t-bool (make-arrow t-int (loop (sub1 n))))))

(test-case "a wide budget keeps a long type on one line"
  (parameterize ([current-type-columns 500])
    (check-false (regexp-match? #rx"\n" (format-type long-ty)))))

(test-case "a narrow budget wraps a long type across lines"
  (parameterize ([current-type-columns 20])
    (check-true (regexp-match? #rx"\n" (format-type long-ty)))))

;; ----- refresh-type-columns! ----------------------------------------

(test-case "refresh sets the budget from the detected width"
  (parameterize ([current-type-columns 66])
    (refresh-type-columns! (lambda () 90))
    (check-equal? (current-type-columns) (columns->type-budget 90))))

(test-case "refresh is a no-op when the width can't be detected"
  (parameterize ([current-type-columns 66])
    (refresh-type-columns! (lambda () #f))
    (check-equal? (current-type-columns) 66)))

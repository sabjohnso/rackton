#lang rackton

;; Step 3a of RacktonCmdline.org: named-option constructors
;; (flag / flag-all / opt / opt-all) and the cardinality adapters
;; (value / required / non-empty / last).
;;
;; We build ParseCtx values by hand (as the Step-4 parser eventually
;; will) and check each Arg → Term extraction.
;;
;; RED: the constructors' extractors are stubbed to Err, so every
;; value/list extraction fails; the "absent" and "error" cases that
;; happen to land on Err pass trivially.

(require rackton/cmdline/argval
         rackton/cmdline/arg
         rackton/cmdline/conv
         rackton/cmdline/parsed
         rackton/cmdline/term
         rackton/data/result
         "../unit.rkt")

;; ctx: -v given twice; --count 3 --count 5; --name abc; positionals x y
(: ctx ParseCtx)
(define ctx
  (mk-ctx (list (Pair "v"     (list "" ""))
                (Pair "count" (list "3" "5"))
                (Pair "name"  (list "abc")))
          (list "x" "y")))

(: info (-> (List String) ArgInfo))
(define (info names) (arg-info names))

;; observers (Result EvalError _ has no Eq instance — match instead)
(: ok-bool? (-> (Term Boolean) Boolean Boolean))
(define (ok-bool? t b) (match (term-run t ctx) [(Ok v) (== v b)] [(Err _) #f]))

(: ok-int? (-> (Term Integer) Integer Boolean))
(define (ok-int? t n) (match (term-run t ctx) [(Ok v) (== v n)] [(Err _) #f]))

(: ok-ints? (-> (Term (List Integer)) (List Integer) Boolean))
(define (ok-ints? t xs) (match (term-run t ctx) [(Ok v) (== v xs)] [(Err _) #f]))

(: ok-bools? (-> (Term (List Boolean)) (List Boolean) Boolean))
(define (ok-bools? t xs) (match (term-run t ctx) [(Ok v) (== v xs)] [(Err _) #f]))

(: errs? (-> (Term a) Boolean))
(define (errs? t) (match (term-run t ctx) [(Err _) #t] [(Ok _) #f]))

(: suite (List Test))
(define suite
  (list
    (it "flag present / absent"
        (all-checks
          (list (check-true (ok-bool? (value (flag (info (list "v")))) #t))
                (check-true (ok-bool? (value (flag (info (list "q")))) #f)))))

    (it "flag-all yields one #t per occurrence"
        (all-checks
          (list (check-true (ok-bools? (value (flag-all (info (list "v"))))
                                       (list #t #t))))))

    (it "opt: last occurrence wins; default when absent"
        (all-checks
          (list (check-true (ok-int? (value (opt conv-int 0 (info (list "count")))) 5))
                (check-true (ok-int? (value (opt conv-int 99 (info (list "gone")))) 99)))))

    (it "opt: bad value is an error"
        (all-checks
          (list (check-true (errs? (value (opt conv-int 0 (info (list "name")))))))))

    (it "opt-all: every occurrence parsed; defaults when absent"
        (all-checks
          (list (check-true (ok-ints? (value (opt-all conv-int Nil (info (list "count"))))
                                      (list 3 5)))
                (check-true (ok-ints? (value (opt-all conv-int (list 7) (info (list "gone"))))
                                      (list 7))))))

    (it "required: present unwraps; absent errors"
        (all-checks
          (list (check-true (ok-int? (required (opt (conv-some conv-int) None (info (list "count")))) 5))
                (check-true (errs? (required (opt (conv-some conv-int) None (info (list "gone")))))))))

    (it "non-empty: occurrences pass; empty errors"
        (all-checks
          (list (check-true (ok-ints? (non-empty (opt-all conv-int Nil (info (list "count"))))
                                      (list 3 5)))
                (check-true (errs? (non-empty (opt-all conv-int Nil (info (list "gone")))))))))

    (it "last: last occurrence; empty errors"
        (all-checks
          (list (check-true (ok-int? (last (opt-all conv-int Nil (info (list "count")))) 5))
                (check-true (errs? (last (opt-all conv-int Nil (info (list "gone")))))))))))

(: test-main (IO Unit))
(define test-main (run-suite "rackton/cmdline/argval" suite))

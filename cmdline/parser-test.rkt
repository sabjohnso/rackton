#lang rackton

;; Step 4 of RacktonCmdline.org: the argv -> ParseCtx parser.
;;
;; Covers the syntax rules (--k=v / --k v / -ovalue / -o value /
;; combined -abc / -- / lone -), unambiguous long-option prefix
;; abbreviation, and the unknown / ambiguous / missing-value /
;; flag-takes-no-value errors.
;;
;; RED: parse-argv ignores argv, so every "parsed something" check
;; fails and every error check (expecting Err) fails too.

(require rackton/cmdline/parser
         rackton/cmdline/parsed
         rackton/data/result
         "../unit.rkt")

;; declared options
(: specs (List OptSpec))
(define specs
  (list (OptSpec (list "v" "verbose") #f)
        (OptSpec (list "a" "all")     #f)
        (OptSpec (list "c" "count")   #t)
        (OptSpec (list "f" "file")    #t)
        (OptSpec (list "color")       #f)))   ; long-only; makes --co ambiguous with count

;; observers over the parse result
(: opt-vals (-> (Result EvalError ParseCtx) (List String) (List String)))
(define (opt-vals r names)
  (match r [(Ok ctx) (ctx-opt-values ctx names)] [(Err _) Nil]))

(: flag-ct (-> (Result EvalError ParseCtx) (List String) Integer))
(define (flag-ct r names)
  (match r [(Ok ctx) (ctx-flag-count ctx names)] [(Err _) -1]))

(: positionals-of (-> (Result EvalError ParseCtx) (List String)))
(define (positionals-of r)
  (match r [(Ok ctx) (ctx-positionals ctx)] [(Err _) Nil]))

(: is-err? (-> (Result EvalError ParseCtx) Boolean))
(define (is-err? r) (match r [(Err _) #t] [(Ok _) #f]))

(: p (-> (List String) (Result EvalError ParseCtx)))
(define (p argv) (parse-argv specs argv))

(: suite (List Test))
(define suite
  (list
    (it "long flag"
        (all-checks (list (check-equal? (flag-ct (p (list "--verbose")) (list "v" "verbose")) 1))))

    (it "valued option: --k=v / --k v / -kv / -k v all agree"
        (all-checks
          (list (check-equal? (opt-vals (p (list "--count=5"))   (list "c" "count")) (list "5"))
                (check-equal? (opt-vals (p (list "--count" "5")) (list "c" "count")) (list "5"))
                (check-equal? (opt-vals (p (list "-c5"))          (list "c" "count")) (list "5"))
                (check-equal? (opt-vals (p (list "-c" "5"))       (list "c" "count")) (list "5")))))

    (it "repeated valued option keeps occurrence order"
        (all-checks
          (list (check-equal? (opt-vals (p (list "--count" "3" "--count" "5")) (list "c" "count"))
                              (list "3" "5")))))

    (it "combined short flags -va"
        (all-checks
          (list (check-equal? (flag-ct (p (list "-va")) (list "v" "verbose")) 1)
                (check-equal? (flag-ct (p (list "-va")) (list "a" "all"))     1))))

    (it "positionals"
        (all-checks
          (list (check-equal? (positionals-of (p (list "x" "y"))) (list "x" "y")))))

    (it "-- terminates option parsing"
        (all-checks
          (list (check-equal? (positionals-of (p (list "--" "--count" "5")))
                              (list "--count" "5")))))

    (it "lone - is a positional"
        (all-checks
          (list (check-equal? (positionals-of (p (list "-"))) (list "-")))))

    (it "unambiguous prefix abbreviation"
        (all-checks
          (list (check-equal? (flag-ct (p (list "--verb")) (list "v" "verbose")) 1))))

    (it "mixed options and positionals"
        (let ([r (p (list "-c" "5" "foo" "--verbose" "bar"))])
          (all-checks
            (list (check-equal? (opt-vals r (list "c" "count")) (list "5"))
                  (check-equal? (flag-ct r (list "v" "verbose")) 1)
                  (check-equal? (positionals-of r) (list "foo" "bar"))))))

    (it "unknown option errors"
        (all-checks (list (check-true (is-err? (p (list "--nope")))))))

    (it "ambiguous abbreviation errors"
        (all-checks (list (check-true (is-err? (p (list "--co")))))))

    (it "missing value errors"
        (all-checks (list (check-true (is-err? (p (list "--count")))))))

    (it "flag given a value errors"
        (all-checks (list (check-true (is-err? (p (list "--verbose=1")))))))))

(: test-main (IO Unit))
(define test-main (run-suite "rackton/cmdline/parser" suite))

#lang rackton

;; Step 6a of RacktonCmdline.org: the pure Term -> value bridge.
;;
;; term->specs derives the parser's option specs from the declared
;; kinds; run-term parses argv and runs the Term.  Includes the
;; round-trip property: an integer survives "--count <n>".
;;
;; RED: term->specs and run-term are stubbed, so every check fails.

(require rackton/cmdline/run
         rackton/cmdline/argval
         rackton/cmdline/arg
         rackton/cmdline/conv
         rackton/cmdline/parser
         rackton/cmdline/term
         rackton/cmdline/parsed
         rackton/data/result
         "../unit.rkt")

(: info (-> (List String) ArgInfo))
(define (info names) (arg-info names))

;; observers
(: ran-int? (-> (Term Integer) (List String) Integer Boolean))
(define (ran-int? t argv n) (match (run-term t argv) [(Ok v) (== v n)] [(Err _) #f]))

(: ran-bool? (-> (Term Boolean) (List String) Boolean Boolean))
(define (ran-bool? t argv b) (match (run-term t argv) [(Ok v) (== v b)] [(Err _) #f]))

(: ran-str? (-> (Term String) (List String) String Boolean))
(define (ran-str? t argv s) (match (run-term t argv) [(Ok v) (== v s)] [(Err _) #f]))

(: ran-err? (-> (Term a) (List String) Boolean))
(define (ran-err? t argv) (match (run-term t argv) [(Err _) #t] [(Ok _) #f]))

(: specs-valued (-> (Term a) (List Boolean)))
(define (specs-valued t) (fmap OptSpec-takes-value (term->specs t)))

;; a small config assembled with let+
(data Cfg (Cfg Boolean Integer String))

(: cfg-term (Term Cfg))
(define cfg-term
  (let+ ([v (value (flag (info (list "v" "verbose"))))]
         [c (value (opt conv-int 0 (info (list "c" "count"))))]
         [f (value (pos 0 conv-string "none" (info Nil)))])
    (Cfg v c f)))

(: suite (List Test))
(define suite
  (list
    (it "flag from argv"
        (all-checks
          (list (check-true (ran-bool? (value (flag (info (list "v" "verbose")))) (list "--verbose") #t))
                (check-true (ran-bool? (value (flag (info (list "v" "verbose")))) Nil #f)))))

    (it "opt from argv (long, short-glued, default)"
        (all-checks
          (list (check-true (ran-int? (value (opt conv-int 0 (info (list "c" "count")))) (list "--count" "5") 5))
                (check-true (ran-int? (value (opt conv-int 0 (info (list "c" "count")))) (list "-c5") 5))
                (check-true (ran-int? (value (opt conv-int 9 (info (list "c" "count")))) Nil 9)))))

    (it "required: present unwraps, absent errors"
        (all-checks
          (list (check-true (ran-int? (required (opt (conv-some conv-int) None (info (list "n" "num")))) (list "--num" "7") 7))
                (check-true (ran-err? (required (opt (conv-some conv-int) None (info (list "n" "num")))) Nil)))))

    (it "positional from argv"
        (all-checks
          (list (check-true (ran-str? (value (pos 0 conv-string "none" (info Nil))) (list "hello") "hello")))))

    (it "unknown option propagates the parser error"
        (all-checks
          (list (check-true (ran-err? (value (flag (info (list "v")))) (list "--nope"))))))

    (it "term->specs: flag valueless, opt valued, positional absent"
        (all-checks
          (list (check-equal? (specs-valued (value (flag (info (list "v"))))) (list #f))
                (check-equal? (specs-valued (value (opt conv-int 0 (info (list "c"))))) (list #t))
                (check-equal? (specs-valued (value (pos 0 conv-string "" (info Nil)))) Nil))))

    (it "let+ config assembled from argv"
        (all-checks
          (list (check-true
                  (match (run-term cfg-term (list "--verbose" "-c" "3" "file.txt"))
                    [(Ok (Cfg v c f)) (and (== v #t) (and (== c 3) (== f "file.txt")))]
                    [(Err _) #f])))))

    (it-prop "opt round-trips an integer through --count <n>"
             (for-all (int-range -100000 100000)
                      (lambda (n)
                        (match (run-term (value (opt conv-int 0 (info (list "count"))))
                                         (list "--count" (show n)))
                          [(Ok v)  (== v n)]
                          [(Err _) #f]))))))

(: test-main (IO Unit))
(define test-main (run-suite "rackton/cmdline/run" suite))

#lang rackton

;; Step 2 of RacktonCmdline.org: the Term applicative.
;;
;; Two observable components per Term: the declared ArgInfo list, and
;; the value (or error) the parse produces.  We check both against the
;; applicative structure:
;;   - pure declares no args; fmap preserves args; fapply UNIONS args
;;     (left-to-right, so the synopsis order is stable);
;;   - applicative identity / homomorphism on the produced value;
;;   - errors propagate through fapply from either side;
;;   - let+ (applicative bind) assembles a Term correctly.
;;
;; Term holds functions, so we compare observationally (run it / read
;; its arg names) rather than by ==.

(require rackton/cmdline/term
         rackton/cmdline/arg
         rackton/cmdline/parsed
         rackton/data/result
         "../unit.rkt")

(: ai1 ArgInfo) (define ai1 (arg-info (list "a")))
(: ai2 ArgInfo) (define ai2 (arg-info (list "b")))
(: ctx ParseCtx) (define ctx empty-ctx)

;; declarations carrying ai1/ai2 (kind is irrelevant to these tests)
(: ad1 ArgDecl) (define ad1 (ArgDecl ai1 KFlag))
(: ad2 ArgDecl) (define ad2 (ArgDecl ai2 KFlag))

;; constructed terms
(: t5 (Term Integer))
(define t5 (Term (list ad1) (lambda (c) (Ok 5))))

(: tinc (Term (-> Integer Integer)))
(define tinc (Term (list ad2) (lambda (c) (Ok (lambda (x) (+ x 1))))))

(: tbad (Term Integer))
(define tbad (Term Nil (lambda (c) (Err (EvErr "boom")))))

;; observers
(: runs-to (-> (Term Integer) Integer Boolean))
(define (runs-to t n) (match (term-run t ctx) [(Ok v) (== v n)] [(Err _) #f]))

(: errs? (-> (Term Integer) Boolean))
(define (errs? t) (match (term-run t ctx) [(Err _) #t] [(Ok _) #f]))

;; compare declaration lists by their option names (no Eq instance)
(: names-of (-> (List ArgDecl) (List (List String))))
(define (names-of xs) (fmap (lambda (d) (ArgInfo-names (ArgDecl-info d))) xs))

(: suite (List Test))
(define suite
  (list
    (it "pure declares no args"
        (all-checks
          (list (check-equal? (names-of (term-args (ann (pure 7) (Term Integer))))
                              Nil))))

    (it "fmap preserves args and maps the value"
        (let ([t (fmap (lambda (x) (+ x 10)) t5)])
          (all-checks
            (list (check-equal? (names-of (term-args t)) (names-of (list ad1)))
                  (check-true (runs-to t 15))))))

    (it "fapply unions args left-to-right"
        (let ([t (fapply tinc t5)])
          (all-checks
            (list (check-equal? (names-of (term-args t)) (names-of (list ad2 ad1)))))))

    (it "fapply applies the function"
        (all-checks (list (check-true (runs-to (fapply tinc t5) 6)))))

    (it "applicative identity: pure id <*> v"
        (let ([idt (ann (pure (lambda (x) x)) (Term (-> Integer Integer)))])
          (all-checks (list (check-true (runs-to (fapply idt t5) 5))))))

    (it "homomorphism: pure f <*> pure x"
        (let ([pf (ann (pure (lambda (x) (+ x 1))) (Term (-> Integer Integer)))]
              [px (ann (pure 4) (Term Integer))])
          (all-checks (list (check-true (runs-to (fapply pf px) 5))))))

    (it "error propagates through fapply (left operand)"
        (let ([tf (fmap (lambda (n) (lambda (x) x)) tbad)])
          (all-checks (list (check-true (errs? (fapply tf t5)))))))

    (it "error propagates through fapply (right operand)"
        (all-checks (list (check-true (errs? (fapply tinc tbad))))))

    (it "let+ assembles a term (value + arg union)"
        (let ([t (let+ ([f tinc] [x t5]) (f x))])
          (all-checks
            (list (check-true (runs-to t 6))
                  (check-equal? (names-of (term-args t)) (names-of (list ad2 ad1)))))))))

(: test-main (IO Unit))
(define test-main (run-suite "rackton/cmdline/term" suite))

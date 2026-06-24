#lang rackton

;; Step 3b of RacktonCmdline.org: positional constructors
;; (pos / pos-all / pos-left / pos-right) and enumerated flags
;; (vflag / vflag-all).
;;
;; pos-left n / pos-right n follow cmdliner: the positionals whose
;; index is strictly less than / greater than n (the nth itself is
;; reached with `pos n`).
;;
;; RED: these constructors' extractors are stubbed to Err, so every
;; positional/vflag check fails; the "absent" and "error" cases that
;; land on Err pass trivially.

(require rackton/cmdline/argval
         rackton/cmdline/arg
         rackton/cmdline/conv
         rackton/cmdline/parsed
         rackton/cmdline/term
         rackton/data/result
         "../unit.rkt")

(: info (-> (List String) ArgInfo))
(define (info names) (arg-info names))

;; positional contexts and an enumerated-flag alternative set
(: pctx ParseCtx)
(define pctx (mk-ctx Nil (list "10" "20" "30" "40")))

(: alts (List (Pair Integer ArgInfo)))
(define alts
  (list (Pair 1 (info (list "one")))
        (Pair 2 (info (list "two")))
        (Pair 3 (info (list "three")))))

;; "two" present once
(: vctx ParseCtx)
(define vctx (mk-ctx (list (Pair "two" (list ""))) Nil))

;; "one" and "three" both present
(: vctx-multi ParseCtx)
(define vctx-multi (mk-ctx (list (Pair "one" (list "")) (Pair "three" (list ""))) Nil))

;; "one" once, "two" twice
(: vctx-rep ParseCtx)
(define vctx-rep (mk-ctx (list (Pair "one" (list "")) (Pair "two" (list "" ""))) Nil))

;; observers taking an explicit ctx (Result EvalError _ has no Eq)
(: ran-int? (-> (Term Integer) ParseCtx Integer Boolean))
(define (ran-int? t c n) (match (term-run t c) [(Ok v) (== v n)] [(Err _) #f]))

(: ran-ints? (-> (Term (List Integer)) ParseCtx (List Integer) Boolean))
(define (ran-ints? t c xs) (match (term-run t c) [(Ok v) (== v xs)] [(Err _) #f]))

(: ran-errs? (-> (Term a) ParseCtx Boolean))
(define (ran-errs? t c) (match (term-run t c) [(Err _) #t] [(Ok _) #f]))

(: suite (List Test))
(define suite
  (list
   (it "pos: nth positional; default when out of range"
       (all-checks
        (list (check-true (ran-int? (value (pos 0 conv-int 0 (info Nil))) pctx 10))
              (check-true (ran-int? (value (pos 2 conv-int 0 (info Nil))) pctx 30))
              (check-true (ran-int? (value (pos 9 conv-int -1 (info Nil))) pctx -1)))))

   (it "pos-all: every positional, in order"
       (all-checks
        (list (check-true (ran-ints? (value (pos-all conv-int Nil (info Nil))) pctx
                                     (list 10 20 30 40))))))

   (it "pos-left n: positionals with index < n"
       (all-checks
        (list (check-true (ran-ints? (value (pos-left 2 conv-int Nil (info Nil))) pctx
                                     (list 10 20))))))

   (it "pos-right n: positionals with index > n"
       (all-checks
        (list (check-true (ran-ints? (value (pos-right 1 conv-int Nil (info Nil))) pctx
                                     (list 30 40))))))

   (it "vflag: the present alternative; default when none"
       (all-checks
        (list (check-true (ran-int? (value (vflag 0 alts)) vctx 2))
              (check-true (ran-int? (value (vflag 0 alts)) empty-ctx 0)))))

   (it "vflag: mutually-exclusive alternatives error"
       (all-checks
        (list (check-true (ran-errs? (value (vflag 0 alts)) vctx-multi)))))

   (it "vflag-all: a value per occurrence; defaults when none"
       (all-checks
        (list (check-true (ran-ints? (value (vflag-all Nil alts)) vctx-rep
                                     (list 1 2 2)))
              (check-true (ran-ints? (value (vflag-all (list 9) alts)) empty-ctx
                                     (list 9))))))))

(: main Unit)
(define main (run-io (run-suite "rackton/cmdline/argval positionals + vflag" suite)))

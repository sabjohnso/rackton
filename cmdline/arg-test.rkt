#lang rackton

;; Step 1 of RacktonCmdline.org: ArgInfo defaults + field updaters.
;;
;; `arg-info` builds a record with sensible defaults; each `info-*`
;; updater replaces exactly its field and leaves the rest alone.
;;
;; RED: the updaters in arg.rkt are stubbed to identity, so the four
;; updater tests fail (only the defaults test passes).

(require rackton/cmdline/arg
         "../unit.rkt")

(: base ArgInfo)
(define base (arg-info (list "v" "verbose")))

(: env1 EnvInfo)
(define env1 (EnvInfo "VERBOSE" "emit progress" "ENVIRONMENT"))

;; read the optional fields as Booleans, for the checks
(: docv-is (-> ArgInfo String Boolean))
(define (docv-is i s)
  (match (ArgInfo-docv i) [(Some v) (== v s)] [(None) #f]))

(: env-var-is (-> ArgInfo String Boolean))
(define (env-var-is i s)
  (match (ArgInfo-env i) [(Some e) (== (EnvInfo-var e) s)] [(None) #f]))

(: none-docv? (-> ArgInfo Boolean))
(define (none-docv? i) (match (ArgInfo-docv i) [(None) #t] [(Some _) #f]))

(: none-env? (-> ArgInfo Boolean))
(define (none-env? i) (match (ArgInfo-env i) [(None) #t] [(Some _) #f]))

(: suite (List Test))
(define suite
  (list
    (it "arg-info defaults"
        (all-checks
          (list (check-equal? (ArgInfo-names base) (list "v" "verbose"))
                (check-equal? (ArgInfo-doc base) "")
                (check-equal? (ArgInfo-docs base) "OPTIONS")
                (check-true (none-docv? base))
                (check-true (none-env? base)))))

    (it "info-doc sets doc, leaves names/docs untouched"
        (all-checks
          (list (check-equal? (ArgInfo-doc (info-doc "be loud" base)) "be loud")
                (check-equal? (ArgInfo-names (info-doc "be loud" base))
                              (list "v" "verbose"))
                (check-equal? (ArgInfo-docs (info-doc "be loud" base)) "OPTIONS"))))

    (it "info-docv sets the value placeholder"
        (all-checks
          (list (check-true (docv-is (info-docv "LEVEL" base) "LEVEL")))))

    (it "info-docs sets the man section"
        (all-checks
          (list (check-equal? (ArgInfo-docs (info-docs "COMMON OPTIONS" base))
                              "COMMON OPTIONS"))))

    (it "info-env attaches the environment variable"
        (all-checks
          (list (check-true (env-var-is (info-env env1 base) "VERBOSE")))))))

(: test-main (IO Unit))
(define test-main (run-suite "rackton/cmdline/arg" suite))

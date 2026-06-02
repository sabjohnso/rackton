#lang rackton

;; rackton/unit — the BDD test tree and the IO runner.
;;
;; describe/it/context build an immutable `Test` value (a functional
;; core: nothing runs until the runner consumes it).  `run-tests` is the
;; imperative shell: it walks the tree in `IO`, prints an indented
;; ok/FAIL report, and returns a `Summary` of counts.
;;
;; Unit-test leaves carry an already-evaluated `Assertion`.  Property
;; leaves carry a `Property` that the runner executes inside `try`, so a
;; property whose body panics is reported as a failure rather than
;; aborting the whole run; a failing property prints its seed for replay.
;;
;; Public API: Test, Outcome, Summary, it, it-prop, describe, context,
;; run-tests, summary-passed, summary-failed.  Re-exports the property,
;; generator, and check surfaces so a consumer requires only this module.

(require "property.rkt"
         "check.rkt"
         rackton/system)   ;; `try` (runs a Property, catching panics)

(provide (data-out Test)
         (data-out Outcome)
         (data-out Summary)
         it
         it-prop
         group-of
         run-tests
         run-tests-quiet
         run-suite
         summary-passed
         summary-failed
         ;; Re-exports (single import path keeps instance coherence happy).
         (data-out Property)
         (data-out PropOutcome)
         for-all-gen
         for-all
         run-property
         (data-out Gen)
         (data-out Tree)
         gen-tree
         tree-value
         constant
         int-range
         bool
         gen-integer
         gen-boolean
         gen-pair
         replicate-gen
         gen-list
         element-of
         gen-string
         (data-out CheckResult)
         (data-out Assertion)
         assertion-result
         check-equal?
         check-not-equal?
         check-true
         check-false
         fail
         pass
         all-checks)

;; ----- The tree -----------------------------------------------------

(data Outcome
  (Unit-test Assertion)
  (Prop-test Property))

(data Test
  (TLeaf String Outcome)
  (TGroup String (List Test)))

(: it (-> String (-> Assertion Test)))
(define (it name a) (TLeaf name (Unit-test a)))

(: it-prop (-> String (-> Property Test)))
(define (it-prop name p) (TLeaf name (Prop-test p)))

;; The desugaring target for the variadic `describe` / `context`
;; surface forms (see private/surface.rkt).  Users write
;; `(describe NAME child ...)`; the parser rewrites that to
;; `(group-of NAME (Cons child ... Nil))`.
(: group-of (-> String (-> (List Test) Test)))
(define (group-of name kids) (TGroup name kids))

;; ----- Summary ------------------------------------------------------

(data Summary (Summary Integer Integer))   ;; passed, failed

(: summary-passed (-> Summary Integer))
(define (summary-passed s) (match s [(Summary p _) p]))

(: summary-failed (-> Summary Integer))
(define (summary-failed s) (match s [(Summary _ f) f]))

(: summary-add (-> Summary (-> Summary Summary)))
(define (summary-add a b)
  (match a
    [(Summary p1 f1)
     (match b
       [(Summary p2 f2) (Summary (+ p1 p2) (+ f1 f2))])]))

(: one-pass Summary)
(define one-pass (Summary 1 0))

(: one-fail Summary)
(define one-fail (Summary 0 1))

;; ----- Runner helpers -----------------------------------------------

;; How many cases each property runs, and the default replay seed.
(: prop-cases Integer)
(define prop-cases 100)

(: default-seed Integer)
(define default-seed 42)

(: indent (-> Integer String))
(define (indent d)
  (if (<= d 0) "" (string-append "  " (indent (- d 1)))))

;; Defer a pure computation into IO, so `try` can contain a panic raised
;; while evaluating it.  `pure-io` alone would evaluate eagerly; routing
;; through `flatmap` delays the work until the IO is actually run.
(: defer-io (-> (-> Unit a) (IO a)))
(define (defer-io th)
  (flatmap (lambda (u) (pure-io (th u))) (pure-io Unit)))

;; Print one line at the given depth.
(: emit (-> Integer (-> String (IO Unit))))
(define (emit d text)
  (println (string-append (indent d) text)))

;; ----- Running ------------------------------------------------------

(: report-unit (-> Integer (-> String (-> Assertion (IO Summary)))))
(define (report-unit d name a)
  (match (assertion-result a)
    [(CheckPass)
     (do [_ <- (emit d (string-append "ok - " name))]
       (pure-io one-pass))]
    [(CheckFail m)
     (do [_ <- (emit d (string-append "FAIL - "
                                      (string-append name
                                                     (string-append ": " m))))]
       (pure-io one-fail))]))

(: report-prop (-> Integer (-> String (-> Property (IO Summary)))))
(define (report-prop d name p)
  (do [res <- (try (defer-io (lambda (u) (run-property prop-cases default-seed p))))]
    (match res
      [(Err emsg)
       (do [_ <- (emit d (string-append "FAIL - "
                                        (string-append name
                                                       (string-append ": panicked: " emsg))))]
         (pure-io one-fail))]
      [(Ok (PropPassed n))
       (do [_ <- (emit d (string-append "ok - "
                                        (string-append name
                                                       (string-append " ("
                                                                      (string-append (integer->string n)
                                                                                     " cases)")))))]
         (pure-io one-pass))]
      [(Ok (PropFailed shown seed))
       (do [_ <- (emit d (string-append "FAIL - "
                                        (string-append name
                                                       (string-append ": counterexample="
                                                                      (string-append shown
                                                                                     (string-append " seed="
                                                                                                    (integer->string seed)))))))]
         (pure-io one-fail))])))

(: run-at (-> Integer (-> Test (IO Summary))))
(define (run-at d t)
  (match t
    [(TLeaf name (Unit-test a)) (report-unit d name a)]
    [(TLeaf name (Prop-test p)) (report-prop d name p)]
    [(TGroup name kids)
     (do [_ <- (emit d name)]
       (run-group (+ d 1) kids))]))

(: run-group (-> Integer (-> (List Test) (IO Summary))))
(define (run-group d kids)
  (match kids
    [(Nil) (pure-io (Summary 0 0))]
    [(Cons t rest)
     (do [s1 <- (run-at d t)]
         [s2 <- (run-group d rest)]
       (pure-io (summary-add s1 s2)))]))

(: run-tests (-> Test (IO Summary)))
(define (run-tests t) (run-at 0 t))

;; ----- Quiet runner + panic gate (the #lang rackton entry point) ----
;;
;; `run-tests` prints an ok/FAIL line per leaf, which floods the output
;; when many suites run under `raco test`.  `run-tests-quiet` runs the
;; same tree but prints only FAILING leaves, tagged with their group
;; path — so a passing suite is silent, matching how a rackunit harness
;; behaves.  `run-suite` wraps it: run quietly under a top-level group
;; name, then `panic` (which makes `raco test` report a non-zero result)
;; if any leaf failed.  A `#lang rackton` test file ends with
;; `(define _ (run-io (run-suite "name" (list ...))))`.

(: qreport-unit (-> String (-> String (-> Assertion (IO Summary)))))
(define (qreport-unit path name a)
  (match (assertion-result a)
    [(CheckPass)   (pure-io one-pass)]
    [(CheckFail m)
     (do [_ <- (println (string-append "FAIL - "
                          (string-append path
                            (string-append name (string-append ": " m)))))]
       (pure-io one-fail))]))

(: qreport-prop (-> String (-> String (-> Property (IO Summary)))))
(define (qreport-prop path name p)
  (do [res <- (try (defer-io (lambda (u) (run-property prop-cases default-seed p))))]
    (match res
      [(Err emsg)
       (do [_ <- (println (string-append "FAIL - "
                            (string-append path
                              (string-append name (string-append ": panicked: " emsg)))))]
         (pure-io one-fail))]
      [(Ok (PropPassed _)) (pure-io one-pass)]
      [(Ok (PropFailed shown seed))
       (do [_ <- (println (string-append "FAIL - "
                            (string-append path
                              (string-append name
                                (string-append ": counterexample="
                                  (string-append shown
                                    (string-append " seed="
                                      (integer->string seed))))))))]
         (pure-io one-fail))])))

(: qrun-at (-> String (-> Test (IO Summary))))
(define (qrun-at path t)
  (match t
    [(TLeaf name (Unit-test a)) (qreport-unit path name a)]
    [(TLeaf name (Prop-test p)) (qreport-prop path name p)]
    [(TGroup name kids)
     (qrun-group (string-append path (string-append name " > ")) kids)]))

(: qrun-group (-> String (-> (List Test) (IO Summary))))
(define (qrun-group path kids)
  (match kids
    [(Nil) (pure-io (Summary 0 0))]
    [(Cons t rest)
     (do [s1 <- (qrun-at path t)]
         [s2 <- (qrun-group path rest)]
       (pure-io (summary-add s1 s2)))]))

(: run-tests-quiet (-> Test (IO Summary)))
(define (run-tests-quiet t) (qrun-at "" t))

(: run-suite (-> String (-> (List Test) (IO Unit))))
(define (run-suite name tests)
  (do [s <- (run-tests-quiet (group-of name tests))]
    (if (> (summary-failed s) 0)
        (panic (string-append name
                 (string-append ": "
                   (string-append (integer->string (summary-failed s)) " failure(s)"))))
        (pure-io Unit))))

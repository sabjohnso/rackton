#lang rackton

;; Step 6b of RacktonCmdline.org: commands + the pure evaluator.
;;
;; eval-core is driven with explicit argv and an explicit environment
;; map, so subcommand dispatch, leaf evaluation, --help / --version,
;; errors, and env-default seeding are all tested without IO.
;;
;; RED: eval-core is stubbed to EvalFail, so every check fails (the
;; error checks pass trivially).

(require rackton/cmdline/eval
         rackton/cmdline/argval
         rackton/cmdline/arg
         rackton/cmdline/conv
         rackton/cmdline/term
         rackton/data/result
         "../unit.rkt")

(: info (-> (List String) ArgInfo))
(define (info names) (arg-info names))

;; a leaf: --count <n>, default 0; also reads $COUNT
(: count-term (Term Integer))
(define count-term
  (value (opt conv-int 0 (info-env (EnvInfo "COUNT" "" "") (info (list "c" "count"))))))

(: leaf (Cmd Integer))
(define leaf (cmd-v (cmd-version "2.0" (cmd-doc "count things" (cmd-info "tool"))) count-term))

;; a git-like group with two integer subcommands and a nested group
(: add-leaf (Cmd Integer))
(define add-leaf (cmd-v (cmd-doc "add things" (cmd-info "add"))
                        (value (opt conv-int 1 (info (list "n"))))))

(: commit-leaf (Cmd Integer))
(define commit-leaf (cmd-v (cmd-doc "commit things" (cmd-info "commit"))
                           (value (opt conv-int 2 (info (list "n"))))))

(: remote-grp (Cmd Integer))
(define remote-grp
  (cmd-group (cmd-doc "manage remotes" (cmd-info "remote")) None
             (list (cmd-v (cmd-doc "add a remote" (cmd-info "add"))
                          (value (opt conv-int 7 (info (list "n"))))))))

(: grp (Cmd Integer))
(define grp
  (cmd-group (cmd-version "9.9" (cmd-doc "the tool" (cmd-info "git"))) None
             (list add-leaf commit-leaf remote-grp)))

(: no-env (List (Pair String String)))
(define no-env Nil)

;; observers
(: ok-int? (-> (EvalOutcome Integer) Integer Boolean))
(define (ok-int? o n) (match o [(OkValue v) (== v n)] [_ #f]))

(: is-fail? (-> (EvalOutcome a) Boolean))
(define (is-fail? o) (match o [(EvalFail _) #t] [_ #f]))

(: is-version? (-> (EvalOutcome a) String Boolean))
(define (is-version? o s) (match o [(VersionText v) (== v s)] [_ #f]))

(: contains? (-> String String Boolean))
(define (contains? needle hay)
  (let loop ([i 0])
    (cond
      [(> (+ i (string-length needle)) (string-length hay)) #f]
      [(string-prefix? needle (substring hay i (string-length hay))) #t]
      [else (loop (+ i 1))])))

(: help-has? (-> (EvalOutcome a) String Boolean))
(define (help-has? o needle) (match o [(HelpText t) (contains? needle t)] [_ #f]))

(: suite (List Test))
(define suite
  (list
   (it "leaf: parses option / uses default / reports unknown"
       (all-checks
        (list (check-true (ok-int? (eval-core leaf no-env (list "--count" "5")) 5))
              (check-true (ok-int? (eval-core leaf no-env Nil) 0))
              (check-true (is-fail? (eval-core leaf no-env (list "--nope")))))))

   (it "leaf: --help renders name and options"
       (let ([o (eval-core leaf no-env (list "--help"))])
         (all-checks
          (list (check-true (help-has? o "tool"))
                (check-true (help-has? o "OPTIONS"))
                (check-true (help-has? o "--count"))))))

   (it "leaf: --version"
       (all-checks
        (list (check-true (is-version? (eval-core leaf no-env (list "--version")) "2.0")))))

   (it "env default: $COUNT used when absent, argv wins when present"
       (all-checks
        (list (check-true (ok-int? (eval-core leaf (list (Pair "COUNT" "7")) Nil) 7))
              (check-true (ok-int? (eval-core leaf (list (Pair "COUNT" "7")) (list "--count" "3")) 3)))))

   (it "group: dispatches to a subcommand"
       (all-checks
        (list (check-true (ok-int? (eval-core grp no-env (list "add" "-n" "4")) 4))
              (check-true (ok-int? (eval-core grp no-env (list "commit")) 2)))))

   (it "group: unknown subcommand errors"
       (all-checks
        (list (check-true (is-fail? (eval-core grp no-env (list "frobnicate")))))))

   (it "group: --help lists subcommands; --version"
       (let ([h (eval-core grp no-env (list "--help"))])
         (all-checks
          (list (check-true (help-has? h "add"))
                (check-true (help-has? h "commit"))
                (check-true (is-version? (eval-core grp no-env (list "--version")) "9.9"))))))

   (it "nested group: dispatches two levels deep"
       (all-checks
        (list (check-true (ok-int? (eval-core grp no-env (list "remote" "add" "-n" "5")) 5)))))))

(: main Unit)
(define main (run-io (run-suite "rackton/cmdline/eval" suite)))

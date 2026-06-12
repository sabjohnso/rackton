#lang racket/base

;; Tests for repl-paredit.rkt — the pure paredit commands.
;;
;; The example cases are ported from paredit.el's `paredit-commands`
;; table (the machine-checked before/after pairs in its docstrings),
;; restricted to the v1 command set; a `|` marks the point.  The
;; properties pin the paredit invariant itself: a balanced entry stays
;; balanced under every command, from every cursor position.

(module+ test
  (require rackunit
           rackcheck
           racket/format
           "repl-entry.rkt"
           "repl-paredit.rkt")

  ;; (check-paredit command "before" "after") with |-marked points.
  (define-check (check-paredit cmd before after)
    (define result (cmd (marked->entry before)))
    (with-check-info (['before before]
                      ['expected after]
                      ['actual (entry->marked result)])
      (unless (equal? (entry->marked result) after)
        (fail-check))))

  ;; ----- electric: open ----------------------------------------------

  (check-paredit paredit-open-round
                 "(a b |c d)"             "(a b (|) c d)")
  (check-paredit paredit-open-round
                 "(foo \"bar |baz\" quux)" "(foo \"bar (|baz\" quux)")
  (check-paredit paredit-open-round
                 "(a ; comment|\nb)"      "(a ; comment(|\nb)")
  (check-paredit paredit-open-round
                 "foo|"                   "foo (|)")
  (check-paredit paredit-open-square
                 "(a b |c d)"             "(a b [|] c d)")

  ;; ----- electric: close ----------------------------------------------

  (check-paredit paredit-close-round
                 "(a b |c   )"            "(a b c)|")
  (check-paredit paredit-close-round
                 "; Hello,| world!"       "; Hello,)| world!")
  (check-paredit paredit-close-round
                 "|abc"                   "|abc")
  (check-paredit paredit-close-round
                 "(f \"a|b\")"            "(f \"a)|b\")")

  ;; ----- electric: doublequote -----------------------------------------

  (check-paredit paredit-doublequote
                 "(frob grovel |full lexical)"
                 "(frob grovel \"|\" full lexical)")
  (check-paredit paredit-doublequote
                 "(foo \"bar |baz\" quux)"
                 "(foo \"bar \\\"|baz\" quux)")
  (check-paredit paredit-doublequote
                 "\"abc|\""               "\"abc\"|")
  (check-paredit paredit-doublequote
                 "; foo|"                 "; foo\"|")

  ;; ----- electric: backward delete --------------------------------------

  (check-paredit paredit-backward-delete
                 "(quu|x)"                "(qu|x)")
  (check-paredit paredit-backward-delete
                 "(a b)|"                 "(a b|)")
  (check-paredit paredit-backward-delete
                 "(|)"                    "|")
  (check-paredit paredit-backward-delete
                 "(|a)"                   "(|a)")
  (check-paredit paredit-backward-delete
                 "\"|\""                  "|")
  (check-paredit paredit-backward-delete
                 "\"a|\""                 "\"|\"")
  (check-paredit paredit-backward-delete
                 "(f \"|x\")"             "(f \"|x\")")
  (check-paredit paredit-backward-delete
                 "#\\(|"                  "|")
  (check-paredit paredit-backward-delete
                 "| x"                    "| x")

  ;; ----- electric: forward delete ---------------------------------------

  (check-paredit paredit-forward-delete
                 "(|)"                    "|")
  (check-paredit paredit-forward-delete
                 "|()"                    "(|)")
  (check-paredit paredit-forward-delete
                 "x| yz"                  "x|yz")
  (check-paredit paredit-forward-delete
                 "(a|)"                   "(a|)")
  (check-paredit paredit-forward-delete
                 "\"|\""                  "|")
  (check-paredit paredit-forward-delete
                 "|#\\("                  "|")
  (check-paredit paredit-forward-delete
                 "(+ #\\(|)"              "(+ #\\(|)")
  ;; the char before the closer is a literal's payload, not an opener
  ;; — found by the balance property (seed 873083136)

  ;; ----- kill -------------------------------------------------------------

  (define (kill-of before)
    (define-values (en killed) (paredit-kill (marked->entry before)))
    (list (entry->marked en) killed))

  (check-equal? (kill-of "(|foo bar)")     '("(|)" "foo bar"))
  (check-equal? (kill-of "|foo bar")       '("|" "foo bar"))
  (check-equal? (kill-of "(foo |bar (baz\nzot)) x")
                '("(foo |) x" "bar (baz\nzot)")
                "a sexp starting on this line is killed even across lines")
  (check-equal? (kill-of "\"ab|cd ef\"")   '("\"ab|\"" "cd ef"))
  (check-equal? (kill-of "(foo)| ; bar")   '("(foo)|" " ; bar"))
  (check-equal? (kill-of "(a)|")           '("(a)|" #f)
                "nothing to kill")

  ;; ----- transforms ---------------------------------------------------------

  (check-paredit paredit-slurp-forward
                 "(foo (bar |baz) quux zot)"
                 "(foo (bar |baz quux) zot)")
  (check-paredit paredit-slurp-forward
                 "(a b ((c| d)) e f)"
                 "(a b ((c| d) e) f)")
  (check-paredit paredit-slurp-backward
                 "(foo bar (baz| quux) zot)"
                 "(foo (bar baz| quux) zot)")
  (check-paredit paredit-barf-forward
                 "(foo (bar |baz quux) zot)"
                 "(foo (bar |baz) quux zot)")
  (check-paredit paredit-barf-backward
                 "(foo (bar baz |quux) zot)"
                 "(foo bar (baz |quux) zot)")
  (check-paredit paredit-splice
                 "(foo (bar| baz) quux)"
                 "(foo bar| baz quux)")
  (check-paredit paredit-splice
                 "a| b"                   "a| b")
  (check-paredit paredit-raise
                 "(dynamic-wind in (lambda () |body) out)"
                 "(dynamic-wind in |body out)")
  (check-paredit paredit-wrap-round
                 "(foo |bar baz)"
                 "(foo (|bar) baz)")

  ;; ----- the invariant ---------------------------------------------------

  ;; Every command, from every position in a balanced entry, yields a
  ;; balanced entry with the point in range.  This is the property that
  ;; defines paredit.

  (define gen:atom
    (gen:choice (gen:const 'foo) (gen:const 'x) (gen:const '+)
                (gen:integer-in -99 99)
                (gen:const "a b") (gen:const #\() (gen:const 'λ)))
  (define (gen:datum depth)
    (if (zero? depth)
        gen:atom
        (gen:choice gen:atom
                    (gen:list (gen:datum (sub1 depth)) #:max-length 4))))
  ;; Separators include newlines and comments — a comment containing a
  ;; stray paren is exactly what the deletion guards must respect.
  (define gen:separator
    (gen:choice (gen:const " ") (gen:const "\n")
                (gen:const " ; stray ( in comment\n")))
  (define gen:balanced-entry
    (gen:let ([pre  (gen:datum 2)]
              [sep  gen:separator]
              [d    (gen:datum 3)]
              [tail (gen:choice (gen:const "") (gen:const " ; tail )"))]
              [frac (gen:integer-in 0 100)])
      (let ([text (string-append (~s pre) sep (~s d) tail)])
        (entry text (quotient (* frac (string-length text)) 100)))))

  (define all-commands
    (list (cons 'open-round      paredit-open-round)
          (cons 'open-square     paredit-open-square)
          (cons 'close-round     paredit-close-round)
          (cons 'doublequote     paredit-doublequote)
          (cons 'backward-delete paredit-backward-delete)
          (cons 'forward-delete  paredit-forward-delete)
          (cons 'kill            (lambda (en)
                                   (define-values (en* _k) (paredit-kill en))
                                   en*))
          (cons 'slurp-forward   paredit-slurp-forward)
          (cons 'slurp-backward  paredit-slurp-backward)
          (cons 'barf-forward    paredit-barf-forward)
          (cons 'barf-backward   paredit-barf-backward)
          (cons 'splice          paredit-splice)
          (cons 'raise           paredit-raise)
          (cons 'wrap-round      paredit-wrap-round)))

  (check-property
   (make-config #:tests 800)
   (property balance-is-preserved ([en gen:balanced-entry]
                                   [i (gen:integer-in
                                       0 (sub1 (length all-commands)))])
     ;; `i` names the command in any counterexample.
     (define out ((cdr (list-ref all-commands i)) en))
     (and (entry-balanced? out)
          (<= 0 (entry-point out) (entry-length out))))))

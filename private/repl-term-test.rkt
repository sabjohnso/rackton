#lang racket/base

;; Tests for repl-term.rkt — the terminal shell's pure parts: the key
;; decoder (input characters → key names), the layout engine (entry →
;; rows + cursor placement), and the editor state machine (state × key
;; → state) that routes keys through the paredit commands.

(module+ test
  (require rackunit
           racket/string
           racket/list
           "repl-entry.rkt"
           "repl-term.rkt")

  ;; ----- key decoding -------------------------------------------------

  (define (decode str)
    ;; Decode every key in the string; returns the list of keys.
    (let loop ([cs (string->list str)] [acc '()])
      (if (null? cs)
          (reverse acc)
          (let-values ([(key rest) (decode-key cs)])
            (loop rest (cons key acc))))))

  (check-equal? (decode "ab") '((self . #\a) (self . #\b)))
  (check-equal? (decode "\u01") '("C-a"))
  (check-equal? (decode "\u0B") '("C-k"))
  (check-equal? (decode "\r") '("RET"))
  (check-equal? (decode "\t") '("TAB"))
  (check-equal? (decode "\u7F") '("DEL"))
  (check-equal? (decode "\e[A") '("up"))
  (check-equal? (decode "\e[1;5C") '("C-right"))
  (check-equal? (decode "\e[3~") '("delete"))
  (check-equal? (decode "\e[Z") '("S-TAB"))
  (check-equal? (decode "\ep") '("M-p"))
  (check-equal? (decode "\e(") '("M-("))
  (check-equal? (decode "\e\u13") '("C-M-s"))
  (check-equal? (decode "\e\e[A") '("M-up"))
  (check-equal? (decode "\e") '("ESC")
                "a lone escape with nothing after it is the ESC key")
  (check-equal? (decode "\e[200~ab\u1B[201~") '((paste . "ab"))
                "bracketed paste arrives as one key")

  ;; ----- layout ---------------------------------------------------------

  ;; (layout-rows text width prompt-length) → list of (start end prefix?)
  ;; rows; (layout-cursor text point width prompt-length) → (row . col).

  (check-equal? (layout-rows "abc" 10 3)
                '((0 3))
                "one short logical line is one row")
  (check-equal? (layout-rows "abcdefghij" 8 3)
                '((0 5) (5 10))
                "a long line wraps at width minus the prompt")
  (check-equal? (layout-rows "ab\ncd" 10 3)
                '((0 2) (3 5))
                "logical lines split on newline (the newline is no row text)")
  (check-equal? (layout-rows "" 10 3)
                '((0 0))
                "an empty entry still has one row")

  (check-equal? (layout-cursor "abc" 0 10 3) '(0 . 3))
  (check-equal? (layout-cursor "abc" 3 10 3) '(0 . 6))
  (check-equal? (layout-cursor "ab\ncd" 4 10 3) '(1 . 4)
                "point after the newline lands on the second row")
  (check-equal? (layout-cursor "abcdefghij" 5 8 3) '(1 . 3)
                "point at a wrap boundary starts the next row")

  ;; ----- the editor state machine -----------------------------------------

  (define cfg
    (editor-config
     (lambda (text)            ; ready?: balanced and non-blank
       (and (entry-balanced? (entry text 0))
            (not (string=? (string-trim text) ""))))
     (lambda (prefix)          ; completions
       (filter (lambda (s) (string-prefix? s prefix))
               '("define" "define-alias" "define-struct")))
     "λ> "))

  (define (feed st keys)
    (for/fold ([st st]) ([k (in-list keys)])
      (editor-step st cfg k)))

  (define (typed str [history '()])
    (feed (make-editor-state history) (decode str)))

  (define (entry-of st) (entry->marked (editor-state-entry st)))

  ;; typing routes through paredit
  (check-equal? (entry-of (typed "(+ 1 2")) "(+ 1 2|)"
                "open paren inserted its closer; typing continues inside")
  (check-equal? (entry-of (typed "(+ 1 2)")) "(+ 1 2)|"
                "the closing paren moves past the inserted closer")
  (check-equal? (entry-of (typed "(\"a\u7F")) "(\"|\")"
                "backspace in a string is the paredit backspace")
  (check-equal? (entry-of (typed "ab\u7F\u7F")) "|")

  ;; RET accepts only when ready
  (check-equal? (editor-state-done (typed "(+ 1 2)\r")) 'accept)
  (check-false  (editor-state-done (typed "(+ 1 2\r")))
  (check-regexp-match #rx"\\(\\+ 1 2\n" (entry-text (editor-state-entry
                                                     (typed "(+ 1 2\r")))
                      "RET on an incomplete form inserts a newline")

  ;; the newline is indented under the open paren
  (check-equal? (entry-of (typed "(foo\r")) "(foo\n  |)"
                "the continuation line indents two past the opener")

  ;; kill + yank round-trip
  (check-equal? (entry-of (typed "(a b)\u01\u02\u0B\u19"))
                "(a b)|"
                "C-k kills the rest and C-y yanks it back (C-a/C-b position)")

  ;; C-d on an empty entry is eof
  (check-equal? (editor-state-done (typed "\u04")) 'eof)
  (check-false  (editor-state-done (typed "x\u04")))

  ;; paste inserts raw, no electric routing
  (check-equal? (entry-of (feed (make-editor-state '())
                                '((paste . "(a"))))
                "(a|"
                "pasted text bypasses the electric layer")

  ;; ----- history ------------------------------------------------------------

  (define hist '("(third)" "(second)" "(first)"))

  (check-equal? (entry-of (feed (make-editor-state hist) (decode "\e[A")))
                "(third)|"
                "Up on an empty entry recalls the most recent input")
  (check-equal? (entry-of (feed (make-editor-state hist) (decode "\e[A\e[A")))
                "(second)|")
  (check-equal? (entry-of (feed (make-editor-state hist) (decode "\e[A\e[A\e[B")))
                "(third)|"
                "Down comes back toward the present")
  (check-equal? (entry-of (feed (make-editor-state hist)
                                (decode "(x\ry\e[A")))
                "(x|\n  y)"
                "Up below the first row is cursor motion, not history")

  ;; M-p searches by the text before the cursor
  (check-equal? (entry-of (feed (make-editor-state
                                 '("(second x)" "(first x)" "zzz"))
                                (append (decode "(f") (list "M-p"))))
                "(first x)|"
                "prefix search recalls the matching entry")

  ;; ----- completion ------------------------------------------------------------

  (check-equal? (entry-of (typed "defi\t")) "define|"
                "Tab extends to the common prefix")
  (check-regexp-match #rx"define-alias"
                      (or (editor-state-message (typed "define\t")) "")
                      "an ambiguous Tab lists candidates in the message"))

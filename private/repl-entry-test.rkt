#lang racket/base

;; Tests for repl-entry.rkt — the pure entry model under the REPL's
;; structural editor: an immutable (text, point) pair, a tokenizer
;; backed by the standard Racket lexer (our "syntax table"), context
;; queries (in-string?, in-comment?, enclosing delimiters), and
;; s-expression motion.
;;
;; Notation: in test inputs a `|` marks the point, exactly as in
;; paredit.el's command examples.

(module+ test
  (require rackunit
           rackcheck
           racket/format
           "repl-entry.rkt")

  ;; "(a |b)" → entry with text "(a b)" and point 3.
  (define (e str)
    (define i (char-position str))
    (entry (string-append (substring str 0 i) (substring str (add1 i)))
           i))
  (define (char-position str)
    (for/first ([c (in-string str)] [i (in-naturals)] #:when (char=? c #\|)) i))

  ;; Render an entry back to |-notation, for readable equality checks.
  (define (render en)
    (string-append (substring (entry-text en) 0 (entry-point en))
                   "|"
                   (substring (entry-text en) (entry-point en))))

  ;; ----- edits ------------------------------------------------------

  (check-equal? (render (entry-insert (e "(a |b)") "x ")) "(a x |b)"
                "insert puts text at point and moves point past it")

  (check-equal? (render (entry-delete (e "(a xy|b)") 3 5)) "(a |b)"
                "deleting before the point pulls the point back")

  (check-equal? (render (entry-delete (e "(|a xyb)") 3 5)) "(|a b)"
                "deleting after the point leaves the point alone")

  (check-equal? (render (entry-delete (e "(a x|yb)") 3 5)) "(a |b)"
                "deleting across the point lands the point at the cut")

  (check-equal? (render (entry-goto (e "|abc") 2)) "ab|c"
                "goto moves the point")

  ;; ----- context queries --------------------------------------------

  (check-true  (entry-in-string? (e "(f \"a|b\")")))
  (check-false (entry-in-string? (e "(f |\"ab\")"))
               "before the opening quote is not in the string")
  (check-false (entry-in-string? (e "(f \"ab\"|)"))
               "after the closing quote is not in the string")
  (check-true  (entry-in-string? (e "(f \"ab|"))
               "inside an unterminated string counts as in-string")
  (check-true  (entry-in-string? (e "\"ab|\""))
               "just before the closing quote is in the string")

  (check-true  (entry-in-comment? (e "(a ; com|ment\nb)")))
  (check-true  (entry-in-comment? (e "(a ; comment|\nb)"))
               "just before the comment's newline is still in the comment")
  (check-false (entry-in-comment? (e "(a ; comment\n|b)")))
  ;; Block-comment cases use explicit points: the `|` marker notation
  ;; collides with the `#|`/`|#` delimiters themselves.
  (check-true  (entry-in-comment? (entry "#| block |# x" 6)))
  (check-false (entry-in-comment? (entry "#| block |# x" 11))
               "after the closing block-comment delimiter is outside")
  (check-false (entry-in-comment? (e "(a |b) ; comment")))

  ;; enclosing-openers: innermost first, as (char . position) pairs.
  (check-equal? (entry-enclosing-openers (e "(a [b |c] d)"))
                '((#\[ . 3) (#\( . 0)))
  (check-equal? (entry-enclosing-openers (e "|(a)")) '())
  (check-equal? (entry-enclosing-openers (e "(\"(\" |x)")) '((#\( . 0))
                "a paren inside a string is not structural")
  (check-equal? (entry-enclosing-openers (e "(#\\( |x)")) '((#\( . 0))
                "a character literal paren is not structural")
  (check-equal? (entry-enclosing-openers (e "(; (\n|x)")) '((#\( . 0))
                "a paren inside a comment is not structural")

  ;; ----- balance ----------------------------------------------------

  (check-true  (entry-balanced? (e "(define (f x)| (+ x 1))")))
  (check-false (entry-balanced? (e "(define (f x|")))
  (check-false (entry-balanced? (e "\"open|")))
  (check-true  (entry-balanced? (e "|")))

  ;; ----- motion -----------------------------------------------------

  (define (fwd str)
    (define en (e str))
    (forward-sexp-position (entry-text en) (entry-point en)))
  (define (bwd str)
    (define en (e str))
    (backward-sexp-position (entry-text en) (entry-point en)))

  (check-equal? (fwd "|(a b) c") 5  "over a whole list")
  (check-equal? (fwd "(|a b)")   2  "over an atom")
  (check-equal? (fwd "(ab|cd e)") 5 "from mid-atom to its end")
  (check-equal? (fwd "| \"str\" x") 6 "over a string, skipping whitespace")
  (check-equal? (fwd "|; c\n(a)") 7  "skips a comment to the next sexp")
  (check-equal? (fwd "(a|)")     #f "nothing before the closer")
  (check-equal? (fwd "(a) |")    #f "nothing at the end")
  (check-equal? (fwd "|#;(a) b") 5
                "a #; prefix is skipped; the commented datum is the next sexp")

  (check-equal? (bwd "(a b)| c") 0  "back over a whole list")
  (check-equal? (bwd "(a |b)")   1  "back over an atom")
  (check-equal? (bwd "(ab cd|)") 4  "from mid-atom to its start")
  (check-equal? (bwd "\"str\" |x") 0 "back over a string")
  (check-equal? (bwd "(|a)")     #f "nothing after the opener")

  (check-equal? (let ([en (e "(a (b |c) d)")])
                  (up-list-position (entry-text en) (entry-point en)))
                8
                "up-list lands after the enclosing closer")
  (check-equal? (let ([en (e "(a |b)")])
                  (up-list-position (entry-text en) (entry-point en)))
                5)
  (check-equal? (let ([en (e "|(a)")])
                  (up-list-position (entry-text en) (entry-point en)))
                #f
                "no enclosing list at top level")
  (check-equal? (let ([en (e "(a |b")])
                  (up-list-position (entry-text en) (entry-point en)))
                #f
                "an unterminated enclosing list has no up")

  (check-equal? (let ([en (e "(a (b |c) d)")])
                  (backward-up-position (entry-text en) (entry-point en)))
                3
                "backward-up lands on the enclosing opener")

  (check-equal? (let ([en (e "|a (b c)")])
                  (down-list-position (entry-text en) (entry-point en)))
                3
                "down-list enters the next list")
  (check-equal? (let ([en (e "(a b|)")])
                  (down-list-position (entry-text en) (entry-point en)))
                #f)

  ;; ----- properties ---------------------------------------------------

  ;; Random Rackton-ish datums, rendered to text with ~s.
  (define gen:atom
    (gen:choice (gen:const 'foo) (gen:const 'x) (gen:const '+)
                (gen:integer-in -99 99)
                (gen:const "a str") (gen:const #\() (gen:const 'λ)))
  (define (gen:datum depth)
    (if (zero? depth)
        gen:atom
        (gen:choice gen:atom
                    (gen:list (gen:datum (sub1 depth)) #:max-length 4))))
  (define gen:text
    (gen:let ([d (gen:datum 3)]) (~s d)))

  (check-property
   (property rendered-datums-are-balanced ([text gen:text])
     (entry-balanced? (entry text 0))))

  (check-property
   (property forward-over-a-whole-datum-reaches-its-end ([text gen:text])
     (equal? (forward-sexp-position text 0) (string-length text))))

  (check-property
   (property backward-from-the-end-reaches-the-start ([text gen:text])
     (equal? (backward-sexp-position text (string-length text)) 0)))

  (check-property
   (property forward-then-backward-returns-to-a-sexp-start ([text gen:text])
     ;; From the start of any datum, stepping over it and back is the
     ;; identity.
     (equal? (backward-sexp-position text (forward-sexp-position text 0))
             0))))

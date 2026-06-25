#lang rackton

;; Tests for the Rackton source formatter (RacktonFormat.org).
;;
;; Step 0: the wrapped lexer boundary.  The token stream must cover the
;; entire input — concatenating each token's exact text reproduces the
;; source byte-for-byte — so the formatter only ever moves whitespace.
;;
;; RED: rackton/tools/fmt-lib does not exist yet.

(require rackton/tools/fmt-lib
         "../unit.rkt")

;; concatenate the texts of a token stream.
(: token-texts (-> (List (Pair String String)) String))
(define (token-texts toks)
  (foldr (lambda (p acc) (mappend (snd p) acc)) "" toks))

(: token-types (-> (List (Pair String String)) (List String)))
(define (token-types toks) (fmap fst toks))

(: covers? (-> String Boolean))
(define (covers? s) (== (token-texts (racket-tokenize s)) s))

(: suite (List Test))
(define suite
  (list
   (it "the token stream reproduces the source exactly"
       (all-checks
        (list (check-true (covers? "(define x 1)"))
              (check-true (covers? "(f a b) ; trailing comment\n"))
              (check-true (covers? "#| a block |# atom"))
              (check-true (covers? "  (a\n   b)  "))
              (check-true (covers? "(list \"str\" #\\a 42)"))
              (check-true (covers? "#;(skip) keep"))
              (check-true (covers? "")))))

   (it "classifies parentheses and symbols"
       (all-checks
        (list (check-equal? (token-types (racket-tokenize "(a)"))
                            (list "parenthesis" "symbol" "parenthesis")))))))

;; ----- Step 1: lex -------------------------------------------------

(: relex (-> String String))
(define (relex s)
  (foldr (lambda (t acc) (mappend (token-text t) acc)) "" (lex (racket-tokenize s))))

(: lex-suite (List Test))
(define lex-suite
  (list
   (it "the typed token stream still reproduces the source"
       (all-checks
        (list (check-true (== (relex "(define x 1)") "(define x 1)"))
              (check-true (== (relex "(f a b) ; c\n") "(f a b) ; c\n"))
              (check-true (== (relex "[a {b}]") "[a {b}]"))
              (check-true (== (relex "#| blk |# 42") "#| blk |# 42"))
              (check-true (== (relex "#;(skip) keep") "#;(skip) keep")))))

   (it "classifies bracket shapes, atoms, and comments"
       (let ([ts (lex (racket-tokenize "(a [b] ; c\n)"))])
         (all-checks
          (list (check-true (match ts [(Cons (TOpen (Round)) _) #t] [_ #f]))
                (check-true (match (lex (racket-tokenize "; hi"))
                              [(Cons (TLineC _) _) #t] [_ #f]))
                (check-true (match (lex (racket-tokenize "#| b |#"))
                              [(Cons (TBlockC _) _) #t] [_ #f]))))))))

;; ----- Step 2: parse (CST) -----------------------------------------

(: parse-suite (List Test))
(define parse-suite
  (list
   (it "parses nesting and bracket shape"
       (all-checks
        (list (check-true
               (match (parse-source "(a [b])")
                 [(Cons (INode (Group (Round)
                                      (Cons (INode (Atom "a"))
                                            (Cons (INode (Group (Square) _)) (Nil)))))
                        (Nil)) #t]
                 [_ #f])))))

   (it "distinguishes trailing from own-line comments"
       (all-checks
        (list (check-true (match (parse-source "a ; t\n")
                            [(Cons _ (Cons (IComment (CLine) _ #t) _)) #t] [_ #f]))
              (check-true (match (parse-source "a\n; o")
                            [(Cons _ (Cons (IComment (CLine) _ #f) _)) #t] [_ #f])))))

   (it "preserves a blank line as a separator"
       (all-checks
        (list (check-true
               (match (parse-source "a\n\nb")
                 [(Cons (INode (Atom "a")) (Cons (IBlank) (Cons (INode (Atom "b")) (Nil)))) #t]
                 [_ #f])))))

   (it "keeps a block comment and a #; datum marker"
       (all-checks
        (list (check-true (match (parse-source "#| b |#")
                            [(Cons (IComment (CBlock) _ _) _) #t] [_ #f]))
              (check-true (match (parse-source "#;x")
                            [(Cons (IComment (CDatum) _ _) _) #t] [_ #f])))))))

;; ----- Step 5: reflow ----------------------------------------------

(: reflow-suite (List Test))
(define reflow-suite
  (list
   (it "a form that fits stays on one line"
       (all-checks
        (list (check-equal? (reflow-source 80 "(define   (f  x)   (+ x   1))")
                            "(define (f x) (+ x 1))\n"))))

   (it "a call that does not fit keeps arg1 on the head line, rest +2"
       (all-checks
        (list (check-equal? (reflow-source 10 "(foo aa bb cc)")
                            "(foo aa\n  bb\n  cc)\n"))))

   (it "a data list aligns its elements under the first"
       (all-checks
        (list (check-equal? (reflow-source 8 "([a 1] [b 2])")
                            "([a 1]\n [b 2])\n")
              (check-equal? (reflow-source 12 "(let ([x 1] [y 2]) z)")
                            "(let ([x 1]\n      [y 2])\n  z)\n"))))

   (it "a trailing comment stays on its line"
       (all-checks
        (list (check-equal? (reflow-source 80 "(a b)   ; hi\n(c)")
                            "(a b) ; hi\n(c)\n"))))

   (it "a blank line between top-level forms is preserved"
       (all-checks
        (list (check-equal? (reflow-source 80 "(a)\n\n\n(b)")
                            "(a)\n\n(b)\n"))))

   ;; head-indent table: special forms indent their body by 2 (not
   ;; aligned under arg 1), and nested forms compound correctly.
   (it "define keeps its signature on the head line, body indents 2"
       (all-checks
        (list (check-equal? (reflow-source 20 "(define (f x) (g x) (h x))")
                            "(define (f x)\n  (g x)\n  (h x))\n"))))

   (it "let keeps its bindings on the head line, body indents 2"
       (all-checks
        (list (check-equal? (reflow-source 14 "(let ([x 1]) (g x))")
                            "(let ([x 1])\n  (g x))\n"))))

   (it "cond has no head argument; clauses indent 2"
       (all-checks
        (list (check-equal? (reflow-source 10 "(cond [a 1] [b 2])")
                            "(cond\n  [a 1]\n  [b 2])\n"))))

   (it "a nested special form compounds the indent (define > let)"
       (all-checks
        (list (check-equal? (reflow-source 18 "(define (f) (let ([x 1]) (g x)))")
                            "(define (f)\n  (let ([x 1])\n    (g x)))\n"))))

   (it "a trailing comment after the head args stays on the head line"
       (all-checks
        (list (check-equal? (reflow-source 80 "(let ([x 1]) ; note\n  (g x))")
                            "(let ([x 1]) ; note\n  (g x))\n"))))

   ;; house-style tuning of the indent table
   (it "do keeps its first binding on the head line, body +2"
       (all-checks
        (list (check-equal? (reflow-source 16 "(do [a b] (c) (d))")
                            "(do [a b]\n  (c)\n  (d))\n"))))

   (it "require is a call: specs +2 in reflow"
       (all-checks
        (list (check-equal? (reflow-source 18 "(require aaa bbb ccc)")
                            "(require aaa\n  bbb\n  ccc)\n"))))

   (it "the racket escape keeps type + vars on the head line, body +2"
       (all-checks
        (list (check-equal? (reflow-source 20 "(racket Foo (x) (bar) (baz))")
                            "(racket Foo (x)\n  (bar)\n  (baz))\n"))))))

;; ----- Step 4: reindent --------------------------------------------

(: reindent-suite (List Test))
(define reindent-suite
  (list
   (it "reindent fixes body indentation, keeping the line breaks"
       (all-checks
        (list (check-equal? (reindent-source "(define (f x)\n(g x)\n(h x))")
                            "(define (f x)\n  (g x)\n  (h x))"))))

   (it "reindent aligns call arguments under the first"
       (all-checks
        (list (check-equal? (reindent-source "(foo bar\nbaz)")
                            "(foo bar\n     baz)"))))

   (it "reindent keeps intra-line spacing and adds none of its own"
       (all-checks
        (list (check-equal? (reindent-source "(a   b)") "(a   b)"))))

   (it "reindent compounds nested indentation"
       (all-checks
        (list (check-equal? (reindent-source "(define (f)\n(let ([x 1])\n(g x)))")
                            "(define (f)\n  (let ([x 1])\n    (g x)))"))))

   (it "reindent aligns data-list elements under the first"
       (all-checks
        (list (check-equal? (reindent-source "([x 1]\n[y 2])") "([x 1]\n [y 2])"))))

   (it "racket/base special forms reindent their body +2, not aligned under arg1"
       (all-checks
        (list (check-equal? (reindent-source "(define-syntax foo\n(bar))")
                            "(define-syntax foo\n  (bar))")
              (check-equal? (reindent-source "(case x\n[a 1]\n[b 2])")
                            "(case x\n  [a 1]\n  [b 2])")
              (check-equal? (reindent-source "(let-values ([x y])\n(g))")
                            "(let-values ([x y])\n  (g))"))))))

;; ----- safety: formatting preserves every code token --------------

;; the texts of the non-whitespace tokens, in order.
(: code-texts (-> String (List String)))
(define (code-texts s)
  (foldr (lambda (t acc) (match t [(TSpace _) acc] [_ (Cons (token-text t) acc)]))
         Nil
         (lex (racket-tokenize s))))

;; formatting (reflow at several widths, and reindent) never changes the
;; code-token sequence — it only ever moves whitespace.
(: preserves? (-> String Boolean))
(define (preserves? s)
  (let ([base (code-texts s)])
    (and (== (code-texts (reflow-source 80 s)) base)
         (and (== (code-texts (reflow-source 20 s)) base)
              (and (== (code-texts (reflow-source 5 s)) base)
                   (== (code-texts (reindent-source s)) base))))))

(: snippets (List String))
(define snippets
  (list "(define (f x) (+ x 1))"
        "(let* ([a 1] [b 2]) (cons a b)) ; trailing\n(g)"
        "(cond [(> x 0) \"positive\"] [else (h #\\a)])"
        "#| a block |#\n(data T (A) (B Integer))"
        "(do [x <- m]\n  (pure (list 1 2 3 4 5)))"
        "#;(skip this) (keep this)"
        "(a\n\n(b c)\n; a comment\n(d))"
        "(match v\n  [(Some x) x]\n  [(None) 0])"))

(: safety-suite (List Test))
(define safety-suite
  (list
   (it "formatting preserves every code token (reflow @ 80/20/5 + reindent)"
       (all-checks (fmap (lambda (s) (check-true (preserves? s))) snippets)))))

(: main Unit)
(define main
  (run-io (run-suite "rackton/tools/fmt"
                     (append suite
                             (append lex-suite
                                     (append parse-suite
                                             (append reflow-suite
                                                     (append reindent-suite safety-suite))))))))

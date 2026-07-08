#lang rackton

;; Phase 1 of Pretty.org: the core algebra + greedy renderer.
;;
;; flatten's rules are pinned behaviorally (rendering a flattened doc
;; gives its one-line form), and the renderer is pinned with two
;; goldens that collapse when they fit the width and break when they
;; do not.
;;
;; RED: the engine `best` is stubbed, so every rendered string is empty
;; and all checks fail.

(require rackton/text/pretty
         "../unit.rkt")

;; "a, " line "b, " line "c"  — three comma-separated items
(: items Doc)
(define items
  (<> (text "a")
      (<> (<> (text ",") line)
          (<> (text "b")
              (<> (<> (text ",") line)
                  (text "c"))))))

;; f( <group of items, indented 2> )
(: call Doc)
(define call
  (<> (text "f(")
      (<> (nest 2 (group items))
          (text ")"))))

;; begin <indent 2: body> end, as one group
(: block Doc)
(define block
  (group (<> (text "begin")
             (<> (nest 2 (<> line (text "body")))
                 (<> line (text "end"))))))

(: no-newline? (-> String Boolean))
(define (no-newline? s)
  (let loop ([i 0])
    (cond
      [(>= i (string-length s)) #t]
      [(== (substring s i (+ i 1)) "\n") #f]
      [else (loop (+ i 1))])))

(: suite (List Test))
(define suite
  (list
    (it "flatten's rules (rendered one-line forms)"
        (all-checks
          (list (check-equal? (pretty 5 (flatten empty)) "")
                (check-equal? (pretty 5 (flatten (text "ab"))) "ab")
                (check-equal? (pretty 5 (flatten line)) " ")
                (check-equal? (pretty 5 (flatten (<> (text "a") (<> line (text "b"))))) "a b")
                ;; nest is irrelevant once flat
                (check-equal? (pretty 5 (flatten (nest 4 (<> line (text "x"))))) " x")
                ;; flatten (group x) = flatten x
                (check-equal? (pretty 5 (flatten (group (<> (text "a") (<> line (text "b")))))) "a b"))))

    (it "a flattened doc never breaks, at any width"
        (all-checks
          (list (check-true (no-newline? (pretty 1 (flatten items)))))))

    (it "group collapses when it fits the width"
        (all-checks
          (list (check-equal? (pretty 80 call) "f(a, b, c)")
                (check-equal? (pretty 80 block) "begin body end"))))

    (it "group breaks (with nesting) when it does not fit"
        (all-checks
          (list (check-equal? (pretty 6 call) "f(a,\n  b,\n  c)")
                (check-equal? (pretty 8 block) "begin\n  body\nend"))))))

;; ----- Phase 2: law suite ------------------------------------------

;; Two docs are equal iff they render the same at every width; we sample
;; a spread of widths.
(: renders-same? (-> Doc Doc Boolean))
(define (renders-same? a b)
  (let loop ([ws (list 1 3 7 16 50 120)])
    (match ws
      [(Nil) #t]
      [(Cons w rest) (if (== (pretty w a) (pretty w b)) (loop rest) #f)])))

;; drop every space and newline (layout), keeping only content.
(: strip-ws (-> String String))
(define (strip-ws s)
  (let loop ([i 0] [acc ""])
    (cond
      [(>= i (string-length s)) acc]
      [(== (substring s i (+ i 1)) " ")  (loop (+ i 1) acc)]
      [(== (substring s i (+ i 1)) "\n") (loop (+ i 1) acc)]
      [else (loop (+ i 1) (string-append acc (substring s i (+ i 1))))])))

;; the width of a doc laid out flat on one line.
(: flat-len (-> Doc Integer))
(define (flat-len d) (string-length (pretty 1000000 (flatten d))))

;; ----- a bounded recursive Doc generator ---------------------------

(: gen-leaf (Gen Doc))
(define gen-leaf (element-of (list empty line (text "a") (text "bb") (text ""))))

(: gen-doc (-> Integer (Gen Doc)))
(define (gen-doc depth)
  (if (<= depth 0)
    gen-leaf
    (do [c <- (int-range 0 3)]
      (cond
        [(== c 0) gen-leaf]
        [(== c 1) (do [a <- (gen-doc (- depth 1))]
                    [b <- (gen-doc (- depth 1))]
                    (constant (<> a b)))]
        [(== c 2) (do [i <- (int-range 0 4)]
                    [x <- (gen-doc (- depth 1))]
                    (constant (nest i x)))]
        [else     (do [x <- (gen-doc (- depth 1))]
                    (constant (group x)))]))))

(: g1 (Gen Doc))
(define g1 (gen-doc 4))

(: g2 (Gen (Pair Doc Doc)))
(define g2 (gen-pair (gen-doc 3) (gen-doc 3)))

(: g3 (Gen (Pair Doc (Pair Doc Doc))))
(define g3 (gen-pair (gen-doc 3) (gen-pair (gen-doc 3) (gen-doc 3))))

(: gen-str (Gen String))
(define gen-str (element-of (list "" "a" "bb" "ccc")))

(: law-suite (List Test))
(define law-suite
  (list
    (it-prop "monoid: empty is a left and right unit of <>"
             (for-all g1 (lambda (x)
                           (and (renders-same? (<> empty x) x)
                                (renders-same? (<> x empty) x)))))

    (it-prop "<> is associative"
             (for-all g3 (lambda (t)
                           (match t
                             [(Pair x (Pair y z))
                              (renders-same? (<> (<> x y) z) (<> x (<> y z)))]))))

    (it-prop "text: text \"\" = empty, and text distributes over append"
             (for-all (gen-pair gen-str gen-str)
                      (lambda (p)
                        (match p
                          [(Pair a b)
                           (and (renders-same? (text "") empty)
                                (renders-same? (<> (text a) (text b)) (text (mappend a b))))]))))

    (it-prop "nest: nest 0 = id, and nest composes additively"
             (for-all g1 (lambda (x)
                           (and (renders-same? (nest 0 x) x)
                                (renders-same? (nest 2 (nest 3 x)) (nest 5 x))))))

    (it-prop "nest distributes over <>"
             (for-all g2 (lambda (p)
                           (match p
                             [(Pair x y)
                              (renders-same? (nest 2 (<> x y)) (<> (nest 2 x) (nest 2 y)))]))))

    (it-prop "flatten distributes over <>"
             (for-all g2 (lambda (p)
                           (match p
                             [(Pair x y)
                              (renders-same? (flatten (<> x y)) (<> (flatten x) (flatten y)))]))))

    (it-prop "flatten ignores nest; flatten (group x) = flatten x"
             (for-all g1 (lambda (x)
                           (and (renders-same? (flatten (nest 3 x)) (flatten x))
                                (renders-same? (flatten (group x)) (flatten x))))))

    (it-prop "safety: content is preserved across widths"
             (for-all g1 (lambda (x) (== (strip-ws (pretty 3 x)) (strip-ws (pretty 100 x))))))

    (it-prop "safety: a flattened doc never breaks"
             (for-all g1 (lambda (x) (no-newline? (pretty 1 (flatten x))))))

    (it-prop "safety: a group that fits stays on one line"
             (for-all g1 (lambda (x) (no-newline? (pretty (flat-len x) (group x))))))))

;; ----- Phase 3: derived combinators --------------------------------

(: abc (List Doc))
(define abc (list (text "a") (text "b") (text "c")))

(: combinator-suite (List Test))
(define combinator-suite
  (list
    (it "glue + enclose + punctuate"
        (all-checks
          (list (check-equal? (pretty 80 (<+> (text "a") (text "b"))) "a b")
                (check-equal? (pretty 80 (enclose (text "<") (text ">") (text "x"))) "<x>")
                (check-equal? (pretty 80 (parens (text "x"))) "(x)")
                (check-equal? (pretty 80 (hcat (punctuate (text ",") abc))) "a,b,c"))))

    (it "hsep never breaks; vcat stacks; cat hugs when it fits"
        (all-checks
          (list (check-equal? (pretty 2 (hsep abc)) "a b c")
                (check-equal? (pretty 80 (vcat abc)) "a\nb\nc")
                (check-equal? (pretty 80 (cat abc)) "abc"))))

    (it "sep collapses or breaks as a unit"
        (all-checks
          (list (check-equal? (pretty 80 (sep abc)) "a b c")
                (check-equal? (pretty 3 (sep abc)) "a\nb\nc"))))

    (it "doc-list / doc-tuple flat and broken"
        (all-checks
          (list (check-equal? (pretty 80 (doc-list abc)) "[a, b, c]")
                (check-equal? (pretty 80 (doc-tuple abc)) "(a, b, c)")
                (check-equal? (pretty 3 (doc-list abc)) "[\n  a,\n  b,\n  c\n]"))))

    (it "fillSep word-wraps"
        (let ([ws (list (text "aa") (text "bb") (text "cc"))])
          (all-checks
            (list (check-equal? (pretty 80 (fillSep ws)) "aa bb cc")
                  (check-true (no-newline? (pretty 80 (fillSep ws))))
                  (check-false (no-newline? (pretty 4 (fillSep ws))))))))))

;; ----- Phase 4: column-aware layer ---------------------------------

(: align-suite (List Test))
(define align-suite
  (list
    (it "hardline forces a break even inside a group"
        (all-checks
          (list (check-equal? (pretty 80 (group (<> (text "a") (<> hardline (text "b"))))) "a\nb"))))

    (it "align lines continuations up under the start column"
        (all-checks
          (list (check-equal? (pretty 80 (<> (text "prefix ") (align (vsep abc))))
                              "prefix a\n       b\n       c"))))

    (it "indent shifts every line by n"
        (all-checks
          (list (check-equal? (pretty 80 (indent 4 (vsep abc))) "    a\n    b\n    c"))))

    (it "fill pads a doc out to a column width"
        (all-checks
          (list (check-equal? (pretty 80 (<> (fill 6 (text "ab")) (text "|"))) "ab    |")
                (check-equal? (pretty 80 (<> (fill 6 (text "abcdef")) (text "|"))) "abcdef|"))))

    (it "ribbon caps non-indentation width below the page width"
        ;; page width 40 but ribbon 0.25 ⇒ ~10 cols of content per line,
        ;; so the group breaks even though it fits in 40.
        (all-checks
          (list (check-false (no-newline? (layout (render 0.25 40 (sep (list (text "aaaa") (text "bbbb") (text "cccc")))))))
                (check-true  (no-newline? (layout (render 1.0  40 (sep (list (text "aaaa") (text "bbbb") (text "cccc"))))))))))))

;; ----- Phase 6: column-aligned fill (fill-grid) --------------------

(: abcde (List Doc))
(define abcde (list (text "a") (text "b") (text "c") (text "d") (text "e")))

(: grid-suite (List Test))
(define grid-suite
  (list
    (it "fill-grid packs more columns as the width grows (gap 1)"
        (all-checks
          (list (check-equal? (pretty 9 (fill-grid/gap 1 abcde)) "a b c d e")
                (check-equal? (pretty 5 (fill-grid/gap 1 abcde)) "a b c\nd e")
                (check-equal? (pretty 3 (fill-grid/gap 1 abcde)) "a b\nc d\ne"))))

    (it "fill-grid sizes each column to its own widest item (per-column)"
        (let ([items (list (text "a") (text "x") (text "bbb") (text "y"))])
          (all-checks
            ;; col0 = max(a, bbb) = 3, so x and y line up under column 4
            (list (check-equal? (pretty 5 (fill-grid/gap 1 items)) "a   x\nbbb y")))))

    (it "fill-grid is page-width adaptive and aligns under the start column"
        (all-checks
          (list (check-equal? (pretty 8 (<> (text ">> ") (fill-grid/gap 1 abcde)))
                              ">> a b c\n   d e"))))

    (it "fill-grid default gap is two spaces"
        (all-checks
          (list (check-equal? (pretty 80 (fill-grid abc)) "a  b  c"))))

    (it "Doc is a Monoid: mappend = <>, mempty = empty"
        (all-checks
          (list (check-equal? (pretty 80 (mappend (text "a") (text "b"))) "ab")
                (check-equal? (pretty 80 (mappend (ann mempty Doc) (text "x"))) "x")
                (check-equal? (pretty 80 (mappend (text "x") (ann mempty Doc))) "x"))))))

(: test-main (IO Unit))
(define test-main (run-suite "rackton/text/pretty"
                             (append suite
                                     (append law-suite
                                             (append combinator-suite
                                                     (append align-suite grid-suite))))))

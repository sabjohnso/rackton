#lang rackton

;; rackton/text/pretty — a Wadler/Leijen pretty printer.
;;
;; A `Doc` describes a SET of layouts of the same text; `group` is the
;; one combinator that offers a choice (flat vs. as-written), and the
;; renderer greedily takes the flat layout whenever its first line still
;; fits the page width.  See Pretty.org for the full design.
;;
;; Phase 1: the core algebra (empty/text/line/<>/nest/group), `flatten`,
;; and the greedy renderer.  The position-aware combinators, the derived
;; layer, and fill-grid come in later phases.
;;
;; STATUS: red.  The rendering engine `best` is stubbed (returns the
;; empty layout), so everything that renders fails; green implements it.

(provide (data-out Doc)
         empty text line <> nest group flatten
         pretty
         ;; Phase 3: line flavors, glue, sequences, enclosing
         linebreak softline softbreak
         <+> <$> <$$> </> <//>
         hsep vsep sep fillSep hcat vcat cat fillCat spread stack
         punctuate
         enclose parens brackets braces angles dquotes squotes
         encloseSep doc-list doc-tuple
         ;; Phase 4: column-aware layer
         (data-out SimpleDoc) render layout
         hardline column nesting width-doc page-width
         align hang indent fill fill-break
         ;; Phase 6: column-aligned fill
         flat-width fill-grid fill-grid/gap)

;; ----- the document algebra ----------------------------------------

(data Doc
  DEmpty
  (DText  String)        ; text with no newlines
  (DLine  String)        ; a line break; the String is its flattened form
  DHard                  ; a mandatory break; never flattens
  DFail                  ; flatten of a hard break — `fits?` rejects it
  (DCat   Doc Doc)
  (DNest  Integer Doc)
  (DUnion Doc Doc)        ; group's choice — LEFT is flatter than RIGHT
  (DColumn   (-> Integer Doc))    ; a doc chosen from the current column
  (DNesting  (-> Integer Doc))    ; a doc chosen from the current indentation
  (DPageWidth (-> Integer Doc)))  ; a doc chosen from the renderer's page width

;; The rendered, choice-free layout.
(data SimpleDoc
  SEmpty
  (SText String SimpleDoc)
  (SLine Integer SimpleDoc))   ; newline + that many spaces of indent

;; Show by hand (DColumn/DNesting hold functions, so it can't be derived);
;; handy for debugging and for property counterexamples.
(instance (Show Doc)
  (define (show d)
    (match d
      [(DEmpty)     "empty"]
      [(DText s)    (mappend "(text " (mappend (show s) ")"))]
      [(DLine s)    (mappend "(line " (mappend (show s) ")"))]
      [(DHard)      "hardline"]
      [(DFail)      "fail"]
      [(DCat a b)   (mappend "(<> " (mappend (show a) (mappend " " (mappend (show b) ")"))))]
      [(DNest i x)  (mappend "(nest " (mappend (show i) (mappend " " (mappend (show x) ")"))))]
      [(DUnion a b) (mappend "(union " (mappend (show a) (mappend " " (mappend (show b) ")"))))]
      [(DColumn f)    "#<column>"]
      [(DNesting f)   "#<nesting>"]
      [(DPageWidth f) "#<page-width>"])))

;; ----- primitives --------------------------------------------------

(: empty Doc)
(define empty DEmpty)

(: text (-> String Doc))
(define (text s) (DText s))

;; a line break that becomes a single space when its group goes flat.
(: line Doc)
(define line (DLine " "))

(: <> (-> Doc Doc Doc))
(define (<> a b) (DCat a b))

(: nest (-> Integer Doc Doc))
(define (nest i d) (DNest i d))

;; ----- flatten + group ---------------------------------------------

;; the single-line meaning of a document.
(: flatten (-> Doc Doc))
(define (flatten d)
  (match d
    [(DEmpty)     DEmpty]
    [(DText s)    (DText s)]
    [(DLine s)    (DText s)]        ; a break becomes its flattened text
    [(DHard)      DFail]             ; a hard break cannot be flattened
    [(DFail)      DFail]
    [(DCat a b)   (DCat (flatten a) (flatten b))]
    [(DNest i x)  (flatten x)]       ; nesting is irrelevant on one line
    [(DUnion a b) (flatten a)]       ; the left branch is already flatter
    [(DColumn f)    (DColumn (lambda (k) (flatten (f k))))]
    [(DNesting f)   (DNesting (lambda (i) (flatten (f i))))]
    [(DPageWidth f) (DPageWidth (lambda (w) (flatten (f w))))]))

;; offer x flat or x as written.
(: group (-> Doc Doc))
(define (group d) (DUnion (flatten d) d))

;; Doc is a Monoid: `<>` is `mappend`, `empty` is `mempty`, so documents
;; compose with the rest of the prelude (fold with `mconcat`, etc.).
;; (Use the raw constructors — `mempty` is registered before the `empty`
;; / `<>` aliases are defined further down.)
(instance (Semigroup Doc)
  (define (mappend a b) (DCat a b)))

(instance (Monoid Doc)
  (define mempty DEmpty))

;; ----- rendering ---------------------------------------------------

(: spaces (-> Integer String))
(define (spaces n) (if (<= n 0) "" (string-append " " (spaces (- n 1)))))

(: layout (-> SimpleDoc String))
(define (layout sd)
  (match sd
    [(SEmpty)       ""]
    [(SText s rest) (string-append s (layout rest))]
    [(SLine i rest) (string-append (string-append "\n" (spaces i)) (layout rest))]))

;; Does the flat first line of this work list fit in `avail` columns?
;; Walk cheaply, stopping at the first line break; `k` is the absolute
;; column, needed to resolve DColumn/DNesting.  A DFail (a flattened
;; hard break) means the flat layout is impossible — reject it.
(: fits? (-> Integer Integer Integer (List (Pair Integer Doc)) Boolean))
(define (fits? w avail k items)
  (if (< avail 0)
      #f
      (match items
        [(Nil) #t]
        [(Cons (Pair i d) rest)
         (match d
           [(DEmpty)     (fits? w avail k rest)]
           [(DText s)    (fits? w (- avail (string-length s)) (+ k (string-length s)) rest)]
           [(DLine _)    #t]
           [(DHard)      #t]
           [(DFail)      #f]
           [(DCat a b)   (fits? w avail k (Cons (Pair i a) (Cons (Pair i b) rest)))]
           [(DNest j x)  (fits? w avail k (Cons (Pair (+ i j) x) rest))]
           [(DUnion a b) (fits? w avail k (Cons (Pair i a) rest))]
           [(DColumn f)  (fits? w avail k (Cons (Pair i (f k)) rest))]
           [(DNesting f) (fits? w avail k (Cons (Pair i (f i)) rest))]
           [(DPageWidth f) (fits? w avail k (Cons (Pair i (f w)) rest))])])))

;; The greedy engine.  `w` page width, `r` ribbon width, `n` the current
;; line's indentation, `k` the current column; work list of (indent . doc).
;; At a union, take the flat branch if its first line fits the lesser of
;; the remaining page width and the remaining ribbon.
(: best (-> Integer Integer Integer Integer (List (Pair Integer Doc)) SimpleDoc))
(define (best w r n k items)
  (match items
    [(Nil) SEmpty]
    [(Cons (Pair i d) rest)
     (match d
       [(DEmpty)     (best w r n k rest)]
       [(DText s)    (SText s (best w r n (+ k (string-length s)) rest))]
       [(DLine _)    (SLine i (best w r i i rest))]   ; break: new line, indent i, column i
       [(DHard)      (SLine i (best w r i i rest))]
       [(DFail)      (best w r n k rest)]              ; defensive; not reached in chosen output
       [(DCat a b)   (best w r n k (Cons (Pair i a) (Cons (Pair i b) rest)))]
       [(DNest j x)  (best w r n k (Cons (Pair (+ i j) x) rest))]
       [(DColumn f)    (best w r n k (Cons (Pair i (f k)) rest))]
       [(DNesting f)   (best w r n k (Cons (Pair i (f i)) rest))]
       [(DPageWidth f) (best w r n k (Cons (Pair i (f w)) rest))]
       [(DUnion a b)
        (let* ([by-width  (- w k)]
               [by-ribbon (+ (- r k) n)]
               [avail     (if (< by-width by-ribbon) by-width by-ribbon)])
          (if (fits? w avail k (Cons (Pair i a) rest))
              (best w r n k (Cons (Pair i a) rest))
              (best w r n k (Cons (Pair i b) rest))))])]))

;; render at a ribbon fraction (0.0–1.0): the ribbon caps the
;; non-indentation characters per line to ribbon-frac * width.
(: render (-> Float Integer Doc SimpleDoc))
(define (render rfrac w d)
  (let* ([raw (float->integer (* (integer->float w) rfrac))]
         [r   (if (< raw 0) 0 (if (> raw w) w raw))])
    (best w r 0 0 (list (Pair 0 d)))))

(: pretty (-> Integer Doc String))
(define (pretty w d) (layout (render 1.0 w d)))

;; ====== Phase 3: derived combinators ===============================
;; STATUS: red.  fold-docs / encloseSep / punctuate are stubbed; the
;; sequence and enclosing goldens fail.  Green implements them.

;; ----- line flavors (no engine change) -----------------------------

(: space Doc)     (define space (text " "))
(: linebreak Doc) (define linebreak (DLine ""))      ; flattens to nothing
(: softline Doc)  (define softline (group line))      ; space if fits, else break
(: softbreak Doc) (define softbreak (group linebreak)); nothing if fits, else break

;; ----- glue --------------------------------------------------------

(: <+> (-> Doc Doc Doc))   (define (<+> a b) (<> a (<> space b)))      ; space
(: <$> (-> Doc Doc Doc))   (define (<$> a b) (<> a (<> line b)))        ; line
(: <$$> (-> Doc Doc Doc))  (define (<$$> a b) (<> a (<> linebreak b)))  ; linebreak
(: </> (-> Doc Doc Doc))   (define (</> a b) (<> a (<> softline b)))     ; softline
(: <//> (-> Doc Doc Doc))  (define (<//> a b) (<> a (<> softbreak b)))   ; softbreak

;; ----- folding a list of docs --------------------------------------

;; combine a list with `f`, `empty` for the empty list.
(: fold-docs (-> (-> Doc Doc Doc) (List Doc) Doc))
(define (fold-docs f ds)
  (match ds
    [(Nil)          empty]
    [(Cons d (Nil)) d]
    [(Cons d rest)  (f d (fold-docs f rest))]))

;; ----- sequences ---------------------------------------------------

(: hsep (-> (List Doc) Doc))    (define (hsep ds)    (fold-docs <+> ds))   ; spaces, no break
(: vsep (-> (List Doc) Doc))    (define (vsep ds)    (fold-docs <$> ds))   ; lines
(: sep (-> (List Doc) Doc))     (define (sep ds)     (group (vsep ds)))     ; all flat or all broken
(: fillSep (-> (List Doc) Doc)) (define (fillSep ds) (fold-docs </> ds))   ; word-wrap
(: hcat (-> (List Doc) Doc))    (define (hcat ds)    (fold-docs <> ds))    ; no separator
(: vcat (-> (List Doc) Doc))    (define (vcat ds)    (fold-docs <$$> ds))  ; linebreaks
(: cat (-> (List Doc) Doc))     (define (cat ds)     (group (vcat ds)))
(: fillCat (-> (List Doc) Doc)) (define (fillCat ds) (fold-docs <//> ds))
(: spread (-> (List Doc) Doc))  (define (spread ds)  (hsep ds))
(: stack (-> (List Doc) Doc))   (define (stack ds)   (vcat ds))

;; put `sep` after every doc but the last.
(: punctuate (-> Doc (List Doc) (List Doc)))
(define (punctuate sep ds)
  (match ds
    [(Nil)          Nil]
    [(Cons d (Nil)) (list d)]
    [(Cons d rest)  (Cons (<> d sep) (punctuate sep rest))]))

;; ----- enclosing ---------------------------------------------------

(: enclose (-> Doc Doc Doc Doc))
(define (enclose l r x) (<> l (<> x r)))

(: parens   (-> Doc Doc)) (define (parens x)   (enclose (text "(") (text ")") x))
(: brackets (-> Doc Doc)) (define (brackets x) (enclose (text "[") (text "]") x))
(: braces   (-> Doc Doc)) (define (braces x)   (enclose (text "{") (text "}") x))
(: angles   (-> Doc Doc)) (define (angles x)   (enclose (text "<") (text ">") x))
(: dquotes  (-> Doc Doc)) (define (dquotes x)  (enclose (text "\"") (text "\"") x))
(: squotes  (-> Doc Doc)) (define (squotes x)  (enclose (text "'") (text "'") x))

;; left/right brackets with items separated by `sep`; flat on one line
;; or broken one-item-per-line with the brackets on their own lines.
;; When broken, items indent by 2 and a `sep`+space (a `line`) follows
;; each but the last; the brackets sit on their own lines (`linebreak`).
(: encloseSep (-> Doc Doc Doc (List Doc) Doc))
(define (encloseSep l r sep ds)
  (match ds
    [(Nil) (<> l r)]
    [_     (group
            (<> l
                (<> (nest 2 (<> linebreak
                                (fold-docs (lambda (a b) (<> a (<> sep (<> line b)))) ds)))
                    (<> linebreak r))))]))

(: doc-list (-> (List Doc) Doc))
(define (doc-list ds) (encloseSep (text "[") (text "]") (text ",") ds))

(: doc-tuple (-> (List Doc) Doc))
(define (doc-tuple ds) (encloseSep (text "(") (text ")") (text ",") ds))

;; ====== Phase 4: column-aware layer ================================
;; STATUS: red.  align / hang / indent / fill / fill-break are stubbed
;; to identity; hardline / column / nesting / width-doc are real (the
;; engine already handles them).  Green implements the stubbed five.

;; a mandatory line break — a group containing it never collapses.
(: hardline Doc)
(define hardline DHard)

;; build a doc from the current column / indentation.
(: column (-> (-> Integer Doc) Doc))
(define (column f) (DColumn f))

(: nesting (-> (-> Integer Doc) Doc))
(define (nesting f) (DNesting f))

;; render d, then hand the number of columns it occupied to f.
(: width-doc (-> Doc (-> Integer Doc) Doc))
(define (width-doc d f)
  (column (lambda (k1) (<> d (column (lambda (k2) (f (- k2 k1))))))))

;; ----- alignment ---------------------------------------------------

;; set d's indentation to the current column, so its wrapped lines line
;; up under where it started.
(: align (-> Doc Doc))
(define (align d)
  (column (lambda (k) (nesting (lambda (i) (nest (- k i) d))))))

;; align, with d's continuation lines hanging i columns deeper.
(: hang (-> Integer Doc Doc))
(define (hang i d) (align (nest i d)))

;; indent every line of d by i columns (the first line too).
(: indent (-> Integer Doc Doc))
(define (indent i d) (nest i (<> (text (spaces i)) d)))

;; render d, then pad with spaces so it occupies at least n columns.
(: fill (-> Integer Doc Doc))
(define (fill n d)
  (width-doc d (lambda (w) (if (>= w n) empty (text (spaces (- n w)))))))

;; like fill, but if d already exceeds n columns, break to a fresh line
;; indented n (so the next item still starts at column n).
(: fill-break (-> Integer Doc Doc))
(define (fill-break n d)
  (width-doc d (lambda (w) (if (> w n) (nest n linebreak) (text (spaces (- n w)))))))

;; build a doc from the renderer's page width.
(: page-width (-> (-> Integer Doc) Doc))
(define (page-width f) (DPageWidth f))

;; ====== Phase 6: column-aligned fill (fill-grid) ===================
;; STATUS: red.  fill-grid/gap is stubbed to a plain vertical stack; the
;; grid goldens fail.  Green implements the fit search + emission.

(: add-maybe (-> (Maybe Integer) (Maybe Integer) (Maybe Integer)))
(define (add-maybe a b)
  (match (Pair a b) [(Pair (Some x) (Some y)) (Some (+ x y))] [_ None]))

;; the width of a doc laid out flat on one line, or None if it cannot be
;; a single line (a hard break) or its width is position-dependent.
(: flat-width (-> Doc (Maybe Integer)))
(define (flat-width d)
  (match d
    [(DEmpty)       (Some 0)]
    [(DText s)      (Some (string-length s))]
    [(DLine s)      (Some (string-length s))]
    [(DHard)        None]
    [(DFail)        None]
    [(DCat a b)     (add-maybe (flat-width a) (flat-width b))]
    [(DNest i x)    (flat-width x)]
    [(DUnion a b)   (flat-width a)]
    [(DColumn f)    None]
    [(DNesting f)   None]
    [(DPageWidth f) None]))

;; ----- list helpers ------------------------------------------------

(: take-n (-> Integer (List a) (List a)))
(define (take-n n xs)
  (if (<= n 0) Nil (match xs [(Nil) Nil] [(Cons x r) (Cons x (take-n (- n 1) r))])))

(: drop-n (-> Integer (List a) (List a)))
(define (drop-n n xs)
  (if (<= n 0) xs (match xs [(Nil) Nil] [(Cons _ r) (drop-n (- n 1) r)])))

;; xs at indices 0, step, 2·step, …
(: every-nth (-> Integer (List a) (List a)))
(define (every-nth step xs)
  (match xs
    [(Nil)        Nil]
    [(Cons x rest) (Cons x (every-nth step (drop-n (- step 1) rest)))]))

;; group xs into rows of n (row-major).
(: chunk (-> Integer (List a) (List (List a))))
(define (chunk n xs)
  (match xs [(Nil) Nil] [_ (Cons (take-n n xs) (chunk n (drop-n n xs)))]))

(: int-nth (-> (List Integer) Integer Integer))
(define (int-nth xs i)
  (match xs [(Nil) 0] [(Cons x rest) (if (<= i 0) x (int-nth rest (- i 1)))]))

(: max-int-list (-> (List Integer) Integer))
(define (max-int-list xs)
  (match xs [(Nil) 0] [(Cons x rest) (let ([m (max-int-list rest)]) (if (> x m) x m))]))

(: sum-int (-> (List Integer) Integer))
(define (sum-int xs) (match xs [(Nil) 0] [(Cons x rest) (+ x (sum-int rest))]))

(: map-range (-> Integer Integer (-> Integer a) (List a)))
(define (map-range lo hi f)
  (if (>= lo hi) Nil (Cons (f lo) (map-range (+ lo 1) hi f))))

;; measure every item, or None if any cell is unmeasurable.
(: all-widths (-> (List Doc) (Maybe (List Integer))))
(define (all-widths ds)
  (match ds
    [(Nil) (Some Nil)]
    [(Cons d rest)
     (match (Pair (flat-width d) (all-widths rest))
       [(Pair (Some w) (Some ws)) (Some (Cons w ws))]
       [_                         None])]))

;; ----- the fit search (largest fitting column count) ---------------

;; widths of each column for a row-major layout into `ncols` columns:
;; column c holds the items at indices c, c+ncols, c+2·ncols, …
(: all-col-widths (-> (List Integer) Integer (List Integer)))
(define (all-col-widths ws ncols)
  (map-range 0 ncols (lambda (c) (max-int-list (every-nth ncols (drop-n c ws))))))

;; total columns the grid occupies at `ncols`: per-column widths + gaps.
(: grid-total (-> (List Integer) Integer Integer Integer))
(define (grid-total ws ncols gap)
  (+ (sum-int (all-col-widths ws ncols)) (* gap (- ncols 1))))

;; the largest column count (≤ item count) whose grid fits in `avail`;
;; at least 1 (a single over-wide item still gets its own column).
(: best-ncols (-> (List Integer) Integer Integer Integer))
(define (best-ncols ws avail gap)
  (let loop ([c (length ws)])
    (if (<= c 1) 1 (if (<= (grid-total ws c gap) avail) c (loop (- c 1))))))

;; ----- emission ----------------------------------------------------

;; rows stacked with mandatory breaks (so a surrounding group can't
;; collapse the grid).
(: stack-hard (-> (List Doc) Doc))
(define (stack-hard ds) (fold-docs (lambda (a b) (<> a (<> hardline b))) ds))

;; one row: pad each cell to its column width and join with `gap`
;; spaces; the last cell is neither padded nor followed by a gap.
(: emit-row (-> Integer (List Integer) (List Doc) Doc))
(define (emit-row gap cws cells)
  (let loop ([cs cells] [c 0])
    (match cs
      [(Nil)          empty]
      [(Cons cell (Nil)) cell]
      [(Cons cell rest)
       (<> (fill (int-nth cws c) cell)
           (<> (text (spaces gap)) (loop rest (+ c 1))))])))

(: emit-grid (-> Integer (List Integer) (List Doc) Doc))
(define (emit-grid gap cws ds)
  (stack-hard (fmap (lambda (row) (emit-row gap cws row)) (chunk (length cws) ds))))

;; ----- fill-grid ---------------------------------------------------

;; default 2-space gap between columns.
(: fill-grid (-> (List Doc) Doc))
(define (fill-grid ds) (fill-grid/gap 2 ds))

;; A column-aligned fill: lay the items into a row-major grid whose
;; columns line up, sized to the page width (minus the current column)
;; and aligned under the start column.  Cells that cannot be a single
;; line fall back to a plain vertical stack.
(: fill-grid/gap (-> Integer (List Doc) Doc))
(define (fill-grid/gap gap ds)
  (match (all-widths ds)
    [(None)    (stack-hard ds)]
    [(Some ws)
     (align
      (column (lambda (col)
        (page-width (lambda (w)
          (let* ([avail (- w col)]
                 [ncols (best-ncols ws (if (< avail 1) 1 avail) gap)]
                 [cws   (all-col-widths ws ncols)])
            (emit-grid gap cws ds)))))))]))

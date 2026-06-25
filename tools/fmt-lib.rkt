#lang rackton

;; rackton/tools/fmt-lib — the Rackton source formatter core.
;;
;; A pure-Rackton source formatter (see RacktonFormat.org).  The only
;; host dependency is the lexer, wrapped as the `racket-tokenize`
;; foreign below; everything else — the concrete syntax tree, the
;; indentation rules, and the reflow/reindent back-ends — is pure
;; Rackton built on rackton/text/pretty.
;;
;; Step 0: the wrapped lexer boundary.

(provide racket-tokenize
         (data-out Shape) (data-out Token)
         lex token-text
         (data-out Node) (data-out Item) (data-out CommentKind)
         parse parse-source
         reflow reflow-source
         reindent reindent-source)

(require rackton/text/pretty)

;; Drive the standard Racket lexer over a string: a (type . exact-text)
;; pair per token, covering the whole input (concatenating the texts
;; reproduces the source).  Pure: same string in, same tokens out.
(foreign racket-tokenize (-> String (List (Pair String String)))
         #:from rackton/private/lexer-prim)

;; ====== Step 1: lex — raw pairs -> typed tokens ====================
;; STATUS: red.  `lex` is stubbed; the token round-trip fails.

(data Shape Round Square Curly)

;; Each token carries its exact source text, so concatenating the texts
;; still reproduces the source.  TSpace holds the verbatim whitespace
;; (its newline count drives blank-line / trailing-comment decisions in
;; the parser).
(data Token
  (TOpen   Shape)        ; (  [  {
  (TClose  Shape)        ; )  ]  }
  (TAtom   String)       ; symbol / number / string / char / keyword
  (TLineC  String)       ; ; line comment  (text includes the ;)
  (TBlockC String)       ; #| block comment |#
  (TDatumC String)       ; #;  datum-comment marker
  (TSpace  String))      ; verbatim whitespace

(: open-str (-> Shape String))
(define (open-str s) (match s [(Round) "("] [(Square) "["] [(Curly) "{"]))

(: close-str (-> Shape String))
(define (close-str s) (match s [(Round) ")"] [(Square) "]"] [(Curly) "}"]))

;; the exact source text of a token (the inverse of `lex`).
(: token-text (-> Token String))
(define (token-text t)
  (match t
    [(TOpen s)   (open-str s)]
    [(TClose s)  (close-str s)]
    [(TAtom x)   x]
    [(TLineC x)  x]
    [(TBlockC x) x]
    [(TDatumC x) x]
    [(TSpace x)  x]))

(: paren-token (-> String Token))
(define (paren-token txt)
  (cond
    [(== txt "(") (TOpen Round)]
    [(== txt "[") (TOpen Square)]
    [(== txt "{") (TOpen Curly)]
    [(== txt ")") (TClose Round)]
    [(== txt "]") (TClose Square)]
    [(== txt "}") (TClose Curly)]
    [else (TAtom txt)]))

(: classify (-> (Pair String String) Token))
(define (classify p)
  (match p
    [(Pair ty txt)
     (cond
       [(== ty "parenthesis")  (paren-token txt)]
       [(== ty "white-space")  (TSpace txt)]
       [(== ty "comment")      (if (string-prefix? ";" txt) (TLineC txt) (TBlockC txt))]
       [(== ty "sexp-comment") (TDatumC txt)]
       [else                   (TAtom txt)])]))

(: lex (-> (List (Pair String String)) (List Token)))
(define (lex ps) (fmap classify ps))

;; ====== Step 2: parse — tokens -> the CST ==========================
;; STATUS: red.  `parse-seq` is stubbed; the structural tests fail.

;; A concrete syntax tree.  Comments and significant blank lines are
;; first-class items, so both back-ends can keep them in place.
(data Node
  (Atom  String)              ; one verbatim token
  (Group Shape (List Item)))  ; ( … ) / [ … ] / { … }

(data CommentKind CLine CBlock CDatum)

(data Item
  (INode    Node)
  (IComment CommentKind String Boolean)   ; kind, text, trailing? (same line as prior token)
  IBlank)                                   ; one preserved blank-line separator

(: count-newlines (-> String Integer))
(define (count-newlines s)
  (let loop ([i 0] [n 0])
    (cond
      [(>= i (string-length s)) n]
      [(== (substring s i (+ i 1)) "\n") (loop (+ i 1) (+ n 1))]
      [else (loop (+ i 1) n)])))

;; prepend a blank-line separator when there was one (and we are not at
;; the very start of the sequence).
(: with-blank (-> Integer Boolean (List Item) (List Item)))
(define (with-blank nl first? acc)
  (if (and (>= nl 2) (not first?)) (Cons IBlank acc) acc))

;; a comment is trailing when it sits on the same line as the prior token.
(: trailing? (-> Integer Boolean Boolean))
(define (trailing? nl first?) (and (== nl 0) (not first?)))

(: drop-close (-> (List Token) (List Token)))
(define (drop-close ts) (match ts [(Cons (TClose _) rest) rest] [_ ts]))

;; Parse a sequence of items up to a close paren or eof, returning
;; (items . remaining-tokens).  `nl` is the newline count of the
;; whitespace since the last item; `first?` suppresses a leading blank.
(: parse-seq (-> (List Token) (Pair (List Item) (List Token))))
(define (parse-seq toks)
  (let loop ([ts toks] [nl 0] [first? #t] [acc Nil])
    (match ts
      [(Nil)               (Pair (reverse acc) Nil)]
      [(Cons (TClose _) _) (Pair (reverse acc) ts)]      ; leave the close for the caller
      [(Cons (TSpace w) rest)
       (loop rest (+ nl (count-newlines w)) first? acc)]
      [(Cons (TAtom x) rest)
       (loop rest 0 #f (Cons (INode (Atom x)) (with-blank nl first? acc)))]
      [(Cons (TLineC x) rest)
       (loop rest 0 #f (Cons (IComment CLine x (trailing? nl first?)) (with-blank nl first? acc)))]
      [(Cons (TBlockC x) rest)
       (loop rest 0 #f (Cons (IComment CBlock x (trailing? nl first?)) (with-blank nl first? acc)))]
      [(Cons (TDatumC x) rest)
       (loop rest 0 #f (Cons (IComment CDatum x (trailing? nl first?)) (with-blank nl first? acc)))]
      [(Cons (TOpen shape) rest)
       (match (parse-seq rest)
         [(Pair inner after)
          (loop (drop-close after) 0 #f
                (Cons (INode (Group shape inner)) (with-blank nl first? acc)))])])))

(: parse (-> (List Token) (List Item)))
(define (parse toks) (match (parse-seq toks) [(Pair items _) items]))

(: parse-source (-> String (List Item)))
(define (parse-source s) (parse (lex (racket-tokenize s))))

;; ====== Step 5: reflow — CST -> Doc -> string ======================
;; Re-emit the program from the CST to a target width with the pretty
;; printer: groups collapse when they fit and break (head on the first
;; line, args aligned under it) when they do not.  Comments force a
;; break; trailing comments hug their line; blank lines are preserved.
;;
;; STATUS: red.  `reflow` is stubbed; the goldens fail.

(: space Doc)
(define space (text " "))

(: comment-item? (-> Item Boolean))
(define (comment-item? it) (match it [(IComment _ _ _) #t] [_ #f]))

(: trailing-comment? (-> Item Boolean))
(define (trailing-comment? it) (match it [(IComment _ _ trail) trail] [_ #f]))

;; drop IBlank items, remembering a blank-before flag for the next item.
(: mark-blanks (-> (List Item) (List (Pair Boolean Item))))
(define (mark-blanks items)
  (let loop ([its items] [blank #f] [acc Nil])
    (match its
      [(Nil)                 (reverse acc)]
      [(Cons (IBlank) rest)  (loop rest #t acc)]
      [(Cons it rest)        (loop rest #f (Cons (Pair blank it) acc))])))

(: blank-of (-> (List (Pair Boolean Item)) Boolean))
(define (blank-of ms) (match ms [(Cons (Pair b _) _) b] [(Nil) #f]))

(: item-of (-> (List (Pair Boolean Item)) Item))
(define (item-of ms) (match ms [(Cons (Pair _ it) _) it] [(Nil) IBlank]))

;; glue between a preceding item and the next (marked) item.
(: glue (-> Item Boolean Item Doc))
(define (glue prev blank next)
  (cond
    [blank                    (<> hardline hardline)]   ; a blank line
    [(trailing-comment? next) space]                     ; trailing comment hugs prev
    [(comment-item? prev)     hardline]                  ; must break after a comment
    [else                     line]))                    ; group decides flat/break

(: item-doc (-> Item Doc))
(define (item-doc it)
  (match it
    [(INode n)          (node-doc n)]
    [(IComment _ txt _) (text txt)]
    [(IBlank)           empty]))

(: node-doc (-> Node Doc))
(define (node-doc n)
  (match n
    [(Atom x)            (text x)]
    [(Group shape items) (group-doc shape items)]))

(: join-marked (-> (List (Pair Boolean Item)) Doc))
(define (join-marked ms)
  (match ms
    [(Nil)                     empty]
    [(Cons (Pair _ it) (Nil))  (item-doc it)]
    [(Cons (Pair _ it) rest)
     (<> (item-doc it) (<> (glue it (blank-of rest) (item-of rest)) (join-marked rest)))]))


;; ----- the head-indent table ---------------------------------------
;; How a group lays out when broken, chosen by its head atom.

(data IndentSpec
  (Body Integer)   ; special form: keep N distinguished args on the head line, body +2
  Call             ; function call: head + arg1 on the head line, the rest +2
  DataList)         ; head is not a name (a bracketed list): align elements under the first

(: member-of (-> String (List String) Boolean))
(define (member-of x xs)
  (match xs [(Nil) #f] [(Cons y r) (if (== x y) #t (member-of x r))]))

(: head-spec (-> String IndentSpec))
(define (head-spec h)
  (cond
    ;; two distinguished args on the head line, body +2:
    ;; the host escape (type + vars), and syntax-case (stx + literals)
    [(member-of h (list "racket" "syntax-case")) (Body 2)]
    ;; one distinguished arg on the head line, body +2
    [(member-of h (list
                   ;; Rackton + Racket binding / definition forms
                   "define" "lambda" "λ" "fn" "let" "let*" "letrec"
                   "let&" "let%" "let+" "do" "when" "unless" "if" "match"
                   "match*" "parameterize" "parameterize*" "with-handlers"
                   "with-handlers*" "struct" "module" "module*" "module+"
                   "define-syntax" "define-syntax-rule" "define-syntaxes"
                   "define-values" "define-for-syntax" "define-struct"
                   "define-match-expander" "match-define"
                   "let-values" "let*-values" "letrec-values"
                   "let-syntax" "letrec-syntax" "let-syntaxes"
                   "match-let" "match-let*" "case" "begin0"
                   "with-syntax" "with-syntax*" "syntax-parse" "syntax-parser"
                   ;; for-comprehension family
                   "for" "for*" "for/list" "for*/list" "for/fold" "for*/fold"
                   "for/vector" "for*/vector" "for/hash" "for/hasheq"
                   "for/hasheqv" "for/and" "for/or" "for/sum" "for/product"
                   "for/first" "for/last" "for/lists" "for/string" "for/foldr"))
     (Body 1)]
    ;; no distinguished args; every clause / form indents +2
    [(member-of h (list "cond" "begin" "begin-for-syntax" "data" "class"
                        "instance" "case-lambda" "match-lambda" "match-lambda*"))
     (Body 0)]
    ;; any other atom head is a function call (require/provide included)
    [else Call]))

;; a group whose head is not a name (e.g. a let binding list) is a data
;; list; its elements align under the first.
(: head-spec-of (-> Item IndentSpec))
(define (head-spec-of it)
  (match it [(INode (Atom x)) (head-spec x)] [_ DataList]))

;; split the first n marked items off the front.
(: split-marked (-> Integer (List (Pair Boolean Item))
                    (Pair (List (Pair Boolean Item)) (List (Pair Boolean Item)))))
(define (split-marked n xs)
  (if (<= n 0)
      (Pair Nil xs)
      (match xs
        [(Nil) (Pair Nil Nil)]
        [(Cons x rest) (match (split-marked (- n 1) rest)
                         [(Pair a b) (Pair (Cons x a) b)])])))

;; the distinguished args, each preceded by a space, on the head line.
(: dist-on-line (-> (List (Pair Boolean Item)) Doc))
(define (dist-on-line ms)
  (match ms
    [(Nil) empty]
    [(Cons (Pair _ it) rest) (<> space (<> (item-doc it) (dist-on-line rest)))]))

;; ----- the two layouts ---------------------------------------------

;; a trailing comment that ends its line (a line / datum comment), which
;; must therefore stay on the head line and force the following break.
(: trailing-line-comment? (-> Item Boolean))
(define (trailing-line-comment? it)
  (match it
    [(IComment (CLine) _ #t)  #t]
    [(IComment (CDatum) _ #t) #t]
    [_ #f]))

;; If the body starts with a trailing line-comment, lift it (as a Doc)
;; onto the head line; the Maybe being Some also means "force the break".
(: peel-trailing (-> (List (Pair Boolean Item)) (Pair (Maybe Doc) (List (Pair Boolean Item)))))
(define (peel-trailing body)
  (match body
    [(Cons (Pair _ it) rest)
     (if (trailing-line-comment? it)
         (Pair (Some (<> space (item-doc it))) rest)
         (Pair None body))]
    [(Nil) (Pair None body)]))

;; head + N distinguished args on the first line; the body indents 2,
;; relative to the open paren (via align), so nesting compounds.  A
;; trailing comment after the distinguished args stays on the head line.
(: body-layout (-> Shape Item (List (Pair Boolean Item)) Integer Doc))
(define (body-layout shape head rest n)
  (let ([o (text (open-str shape))] [c (text (close-str shape))])
    (match (split-marked n rest)
      [(Pair dist body0)
       (match (peel-trailing body0)
         [(Pair extra body)
          (let ([head-line (match extra
                             [(Some d) (hcat (list (item-doc head) (dist-on-line dist) d))]
                             [(None)   (<> (item-doc head) (dist-on-line dist))])]
                [forced (match extra [(Some _) #t] [(None) #f])])
            (match body
              [(Nil) (if forced
                         (align (group (hcat (list o head-line hardline c))))
                         (group (hcat (list o head-line c))))]
              [_     (let ([sep (if forced hardline line)])
                       (align (group (hcat (list o head-line
                                                 (nest 2 (<> sep (join-marked body)))
                                                 c)))))]))])])))

;; a data list: every element aligns under the first (which sits right
;; after the open bracket — a fixed column, so no name-length drift).
(: datalist-layout (-> Shape (List (Pair Boolean Item)) Doc))
(define (datalist-layout shape ms)
  (let ([o (text (open-str shape))] [c (text (close-str shape))])
    (group (hcat (list o (align (join-marked ms)) c)))))

(: group-doc (-> Shape (List Item) Doc))
(define (group-doc shape items)
  (let ([o (text (open-str shape))] [c (text (close-str shape))]
        [ms (mark-blanks items)])
    (match ms
      [(Nil)                      (<> o c)]
      [(Cons (Pair _ head) (Nil)) (<> o (<> (item-doc head) c))]
      [(Cons (Pair _ head) rest)
       (match (head-spec-of head)
         [(Body n)   (body-layout shape head rest n)]
         [(Call)     (body-layout shape head rest 1)]   ; arg1 on head line, rest +2
         [(DataList) (datalist-layout shape ms)])])))

;; top level: each form on its own line, blank lines preserved.
(: top-glue (-> Item Boolean Item Doc))
(define (top-glue prev blank next)
  (cond
    [blank                    (<> hardline hardline)]
    [(trailing-comment? next) space]
    [else                     hardline]))

(: join-top (-> (List (Pair Boolean Item)) Doc))
(define (join-top ms)
  (match ms
    [(Nil)                    empty]
    [(Cons (Pair _ it) (Nil)) (item-doc it)]
    [(Cons (Pair _ it) rest)
     (<> (item-doc it) (<> (top-glue it (blank-of rest) (item-of rest)) (join-top rest)))]))

;; Build a Doc for the whole program and render it at `width`, with a
;; trailing newline.
(: reflow (-> Integer (List Item) String))
(define (reflow width items)
  (mappend (pretty width (join-top (mark-blanks items))) "\n"))

(: reflow-source (-> Integer String String))
(define (reflow-source width s) (reflow width (parse-source s)))

;; ====== Step 4: reindent — keep line breaks, fix indentation =======
;; A token-stream pass: keep the author's line breaks and intra-line
;; spacing; only recompute each line's leading whitespace from the
;; enclosing forms and the head-indent rules.
;;
;; STATUS: red.  `reindent` is stubbed; the goldens fail.

;; One open paren still in scope: where it opened, its head atom (if
;; any), how many items it has seen, and the column of its first arg.
(struct Ctx
  [open-col : Integer]
  [head     : (Maybe String)]
  [items    : Integer]
  [arg1-col : (Maybe Integer)])

(: spaces (-> Integer String))
(define (spaces n) (if (<= n 0) "" (mappend " " (spaces (- n 1)))))

(: newlines (-> Integer String))
(define (newlines n) (if (<= n 0) "" (mappend "\n" (newlines (- n 1)))))

;; how many characters follow the last newline in s.
(: chars-after-nl (-> String Integer))
(define (chars-after-nl s)
  (let loop ([i 0] [after 0])
    (cond
      [(>= i (string-length s)) after]
      [(== (substring s i (+ i 1)) "\n") (loop (+ i 1) 0)]
      [else (loop (+ i 1) (+ after 1))])))

;; the column after emitting `text` from column `col`.
(: advance-col (-> Integer String Integer))
(define (advance-col col text)
  (if (> (count-newlines text) 0) (chars-after-nl text) (+ col (string-length text))))

;; record a new item (an atom or a nested group's open) for the
;; innermost context: the first is its head, the second fixes arg1's
;; column.
(: record-item (-> (List Ctx) Integer (Maybe String) (List Ctx)))
(define (record-item stack col head)
  (match stack
    [(Nil) Nil]
    [(Cons (Ctx oc h items a) rest)
     (cond
       [(== items 0) (Cons (Ctx oc head 1 a) rest)]
       [(== items 1) (Cons (Ctx oc h 2 (Some col)) rest)]
       [else         (Cons (Ctx oc h (+ items 1) a) rest)])]))

(: drop-ctx (-> (List Ctx) (List Ctx)))
(define (drop-ctx stack) (match stack [(Nil) Nil] [(Cons _ r) r]))

;; a call's continuation lines align under arg1 when the author put
;; arguments on the call's line; otherwise they fall to a fixed +2.
(: apply-indent (-> Integer (Maybe Integer) Integer))
(define (apply-indent oc a) (match a [(Some ac) ac] [(None) (+ oc 2)]))

;; the indentation a new line should get, from the innermost context.
;; A data list (no atom head) aligns its elements under the first, which
;; sits just inside the open bracket (oc+1).
(: compute-indent (-> (List Ctx) Integer))
(define (compute-indent stack)
  (match stack
    [(Nil) 0]
    [(Cons (Ctx oc head items a) _)
     (match head
       [(None)   (+ oc 1)]
       [(Some h) (match (head-spec h)
                   [(Body n)   (+ oc 2)]
                   [(Call)     (apply-indent oc a)]
                   [(DataList) (+ oc 1)])])]))

;; Walk the tokens, tracking the open-paren stack and current column;
;; emit each token verbatim, but at every line break replace the next
;; line's leading whitespace with the computed indentation.
;;
;; Output pieces accumulate in `acc` in REVERSE order — each push is
;; O(1) — and are concatenated once at the end with `string-join` (a
;; single linear pass), so the whole pass is O(n) rather than the
;; O(n^2) of repeatedly `mappend`-ing onto a growing string.
(: reindent (-> (List Token) String))
(define (reindent toks)
  (let loop ([ts toks] [stack Nil] [col 0] [acc Nil])
    (match ts
      [(Nil) (string-join "" (reverse acc))]
      [(Cons (TSpace ws) rest)
       (if (> (count-newlines ws) 0)
           (let ([ind (compute-indent stack)])
             (loop rest stack ind
                   (Cons (spaces ind) (Cons (newlines (count-newlines ws)) acc))))
           (loop rest stack (+ col (string-length ws)) (Cons ws acc)))]
      [(Cons (TOpen shape) rest)
       (loop rest (Cons (Ctx col None 0 None) (record-item stack col None))
             (+ col 1) (Cons (open-str shape) acc))]
      [(Cons (TClose shape) rest)
       (loop rest (drop-ctx stack) (+ col 1) (Cons (close-str shape) acc))]
      [(Cons (TAtom x) rest)
       (loop rest (record-item stack col (Some x)) (advance-col col x) (Cons x acc))]
      [(Cons (TLineC x) rest)
       (loop rest stack (advance-col col x) (Cons x acc))]
      [(Cons (TBlockC x) rest)
       (loop rest stack (advance-col col x) (Cons x acc))]
      [(Cons (TDatumC x) rest)
       (loop rest stack (advance-col col x) (Cons x acc))])))

(: reindent-source (-> String String))
(define (reindent-source s) (reindent (lex (racket-tokenize s))))

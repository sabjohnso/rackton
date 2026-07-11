#lang rackton

;; rackton/text/string — Data.String / Data.Text-style operations over
;; the prelude's String type.  Built on the prelude string/char ops and
;; data/list's drop-while.  (string-length / substring / string-append /
;; string-prefix? / string-split / string-join and the char conversions
;; are in the prelude; mappend is the Semigroup append.)

(require rackton/data/list)
(provide (all-defined-out))

(: null-string? (-> String Boolean))
(define (null-string? s) (== (string-length s) 0))

(: reverse-string (-> String String))
(define (reverse-string s) (chars->string (reverse (string->chars s))))

(: to-upper-string (-> String String))
(define (to-upper-string s) (chars->string (fmap char-upcase (string->chars s))))

(: to-lower-string (-> String String))
(define (to-lower-string s) (chars->string (fmap char-downcase (string->chars s))))

;; --- concatenation -------------------------------------------------

;; Variadic concatenation: `(string-append* a b c …)` joins any number
;; of strings, collapsing what would otherwise be a chain of nested
;; binary `string-append` calls.  (The prelude's `string-append` is the
;; binary building block this folds over.)
(: string-append* (-> String ... String))
(define (string-append* . parts) (foldr string-append "" parts))

;; --- trimming whitespace -------------------------------------------

(: strip-start (-> String String))
(define (strip-start s)
  (chars->string (drop-while char-whitespace? (string->chars s))))

(: strip-end (-> String String))
(define (strip-end s)
  (chars->string (reverse (drop-while char-whitespace? (reverse (string->chars s))))))

(: strip (-> String String))
(define (strip s) (strip-start (strip-end s)))

;; --- splitting -----------------------------------------------------

;; split on every occurrence of a character, KEEPING empty segments
;; (n separators -> n+1 segments).
(: split-keep (-> Char (-> String (List String))))
(define (split-keep c s)
  (let loop ([cs (string->chars s)] [cur Nil] [acc Nil])
    (match cs
      [(Nil) (reverse (Cons (chars->string (reverse cur)) acc))]
      [(Cons ch rest)
       (if (== ch c)
           (loop rest Nil (Cons (chars->string (reverse cur)) acc))
           (loop rest (Cons ch cur) acc))])))

;; lines: split on newlines (a single trailing newline yields no extra
;; empty line, matching Haskell `lines`).
(: lines (-> String (List String)))
(define (lines s)
  (if (null-string? s)
      Nil
      (let ([segs (split-keep #\newline s)])
        (match (reverse segs)
          [(Cons h t) (if (null-string? h) (reverse t) segs)]
          [(Nil)      segs]))))

;; words: split on runs of whitespace, dropping empty pieces.
(: words (-> String (List String)))
(define (words s)
  (let loop ([cs (string->chars s)] [cur Nil] [acc Nil])
    (match cs
      [(Nil)
       (reverse (match cur [(Nil) acc] [_ (Cons (chars->string (reverse cur)) acc)]))]
      [(Cons c rest)
       (if (char-whitespace? c)
           (loop rest Nil (match cur [(Nil) acc] [_ (Cons (chars->string (reverse cur)) acc)]))
           (loop rest (Cons c cur) acc))])))

;; --- joining -------------------------------------------------------

;; unwords: join with single spaces.
(: unwords (-> (List String) String))
(define (unwords ws) (string-join " " ws))

;; unlines: append a newline after each line (Haskell `unlines`).
(: unlines (-> (List String) String))
(define (unlines ls)
  (foldr (lambda (l acc) (mappend l (mappend "\n" acc))) "" ls))

;; --- affix predicates ----------------------------------------------

;; is the first char list a prefix of the second?
(: chars-prefix? (-> (List Char) (-> (List Char) Boolean)))
(define (chars-prefix? p s)
  (match p
    [(Nil) #t]
    [(Cons ph pt)
     (match s
       [(Nil)        #f]
       [(Cons sh st) (if (== ph sh) (chars-prefix? pt st) #f)])]))

;; `string-prefix?` is the prelude's; `string-suffix?` is its mirror.
(: string-suffix? (-> String (-> String Boolean)))
(define (string-suffix? p s) (string-prefix? (reverse-string p) (reverse-string s)))

;; does `needle` occur anywhere in the char list?
(: chars-infix? (-> (List Char) (-> (List Char) Boolean)))
(define (chars-infix? needle s)
  (if (chars-prefix? needle s)
      #t
      (match s
        [(Nil)       #f]
        [(Cons _ st) (chars-infix? needle st)])))

(: string-infix? (-> String (-> String Boolean)))
(define (string-infix? needle s)
  (chars-infix? (string->chars needle) (string->chars s)))

;; --- slicing -------------------------------------------------------

;; the first n characters.
(: take-string (-> Integer (-> String String)))
(define (take-string n s) (chars->string (take n (string->chars s))))

;; all but the first n characters.
(: drop-string (-> Integer (-> String String)))
(define (drop-string n s) (chars->string (drop n (string->chars s))))

;; --- padding / repetition ------------------------------------------

;; pad to width w by prepending copies of c (no-op if already wide enough).
(: pad-left (-> Integer (-> Char (-> String String))))
(define (pad-left w c s)
  (if (>= (string-length s) w)
      s
      (mappend (chars->string (replicate (- w (string-length s)) c)) s)))

;; pad to width w by appending copies of c.
(: pad-right (-> Integer (-> Char (-> String String))))
(define (pad-right w c s)
  (if (>= (string-length s) w)
      s
      (mappend s (chars->string (replicate (- w (string-length s)) c)))))

;; concatenate n copies of s.
(: repeat-string (-> Integer (-> String String)))
(define (repeat-string n s) (string-join "" (replicate n s)))

;; --- substitution --------------------------------------------------

;; replace every occurrence of the char list `from` with `to`.
(: replace-chars (-> (List Char) (-> (List Char) (-> (List Char) (List Char)))))
(define (replace-chars from to s)
  (if (chars-prefix? from s)
      (mappend to (replace-chars from to (drop (length from) s)))
      (match s
        [(Nil)       Nil]
        [(Cons h t)  (Cons h (replace-chars from to t))])))

;; replace every (non-empty) occurrence of substring `from` with `to`.
(: replace (-> String (-> String (-> String String))))
(define (replace from to s)
  (if (null-string? from)
      s
      (chars->string (replace-chars (string->chars from)
                                    (string->chars to)
                                    (string->chars s)))))

;; --- substring splitting -------------------------------------------

;; break at the first occurrence of `sep`: (chars-before, chars-from-sep).
;; When `sep` is absent the second component is Nil (so callers can tell
;; "found at the end" from "not found").
(: break-on-chars (-> (List Char) (-> (List Char) (Pair (List Char) (List Char)))))
(define (break-on-chars sep s)
  (if (chars-prefix? sep s)
      (Pair Nil s)
      (match s
        [(Nil) (Pair Nil Nil)]
        [(Cons h t)
         (match (break-on-chars sep t)
           [(Pair before after) (Pair (Cons h before) after)])])))

;; breakOn: split at the first occurrence of `needle`; the second
;; component starts WITH needle (or is "" when needle is absent).
(: break-on (-> String (-> String (Pair String String))))
(define (break-on needle s)
  (match (break-on-chars (string->chars needle) (string->chars s))
    [(Pair before after) (Pair (chars->string before) (chars->string after))]))

(: split-on-chars (-> (List Char) (-> (List Char) (List (List Char)))))
(define (split-on-chars sep s)
  (match (break-on-chars sep s)
    [(Pair before after)
     (match after
       [(Nil) (Cons before Nil)]
       [_     (Cons before (split-on-chars sep (drop (length sep) after)))])]))

;; splitOn: split on every occurrence of `sep`, KEEPING empty segments
;; (n separators -> n+1 segments).  An empty `sep` yields the whole
;; string as one segment.
(: split-on (-> String (-> String (List String))))
(define (split-on sep s)
  (if (null-string? sep)
      (Cons s Nil)
      (fmap chars->string (split-on-chars (string->chars sep) (string->chars s)))))

;; index of the first occurrence of `needle`, or None.  An empty needle
;; matches at index 0.
(: index-of-chars (-> Integer (-> (List Char) (-> (List Char) (Maybe Integer)))))
(define (index-of-chars i needle s)
  (if (chars-prefix? needle s)
      (Some i)
      (match s
        [(Nil)      None]
        [(Cons _ t) (index-of-chars (+ i 1) needle t)])))

(: index-of (-> String (-> String (Maybe Integer))))
(define (index-of needle s)
  (index-of-chars 0 (string->chars needle) (string->chars s)))

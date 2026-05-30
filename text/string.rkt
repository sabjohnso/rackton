#lang rackton

;; rackton/text/string — Data.String / Data.Text-style operations over
;; the prelude's String type.  Built on the prelude string/char ops and
;; data/list's drop-while.  (string-length / substring / string-append /
;; string-prefix? / string-split / string-join and the char conversions
;; are in the prelude; <> is the Semigroup append.)

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
  (foldr (lambda (l acc) (<> l (<> "\n" acc))) "" ls))

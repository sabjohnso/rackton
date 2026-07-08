#lang rackton

;; word-count.rkt — a small program that touches several stdlib families
;; at once, to show how they fit together:
;;
;;   text/string  — tokenise and lowercase the input (words, to-lower-string,
;;                  pad-left, unlines)
;;   data/map     — accumulate per-word counts (map-insert-with, map-to-list)
;;   data/list    — order the results (sort-on, take)
;;   data/functor — map over the words and rows (fmap)
;;   control      — sequence the IO (do-notation)
;;   system       — read the arguments and the file (argv, read-file, println)
;;
;; Usage:  racket word-count.rkt [FILE]
;;   With no argument it counts a built-in sample; with a FILE it counts
;;   that file.  Prints the five most frequent words as "count  word".

(require rackton/batteries)

;; Lowercase words paired with their frequency, most frequent first.
(: tally (-> String (List (Pair String Integer))))
(define (tally text)
  (let ([counts (foldr (lambda (w m) (map-insert-with + w 1 m))
                       empty-map
                       (fmap to-lower-string (words text)))])
    (reverse (sort-on snd (map-to-list counts)))))

;; One output row: the count right-aligned in a 5-wide column, then the word.
(: render-row (-> (Pair String Integer) String))
(define (render-row p)
  (match p
    [(Pair w n) (mappend (pad-left 5 #\space (show n)) (mappend "  " w))]))

;; The top-n report as a single newline-joined String.
(: report (-> Integer (-> String String)))
(define (report n text)
  (unlines (fmap render-row (take n (tally text)))))

(: sample String)
(define sample
  "the quick brown fox the lazy dog the fox jumps the dog sleeps the end")

(: main (IO Unit))
(define main
  (do [args <- argv]
    (match args
      [(Nil)         (println (report 5 sample))]
      [(Cons file _) (do [text <- (read-file file)]
                       (println (report 5 text)))])))

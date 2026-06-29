#lang rackton

;; todo.rkt — a tiny command-line todo manager.
;;
;; Usage:
;;   todo add <task>     # append a pending item
;;   todo list           # print all items with status + index
;;   todo done <N>       # mark the 1-based item N as done
;;   todo clear          # drop items currently marked done
;;
;; Storage:
;;   The file at $TODO_FILE if set, otherwise ./todos.txt.  One
;;   item per line: "[ ] task" (pending) or "[x] task" (done).

(require rackton/system
         rackton/data/result)

;; ----- Item: one row of the todo file ---------------------------

(data Item
  (Item Boolean String))

;; ----- where the items live ------------------------------------

(: todo-file (IO String))
(define todo-file
  (do [maybe <- (getenv "TODO_FILE")]
    (pure (match maybe
            [(None)   "./todos.txt"]
            [(Some s) s]))))

;; ----- File ↔ items -------------------------------------------

(: parse-item (-> String (Maybe Item)))
(define (parse-item line)
  (cond
    [(string-prefix? "[x] " line) (Some (Item #t (substring line 4 (string-length line))))]
    [(string-prefix? "[ ] " line) (Some (Item #f (substring line 4 (string-length line))))]
    [else None]))

(: render-item (-> Item String))
(define (render-item it)
  (match it
    [(Item done text)
     (let* ([marker (if done "[x] " "[ ] ")])
       (mappend marker text))]))

;; concat-map over list (filter-Some) — keep only Items that parsed.
(: catMaybes (-> (List (Maybe a)) (List a)))
(define (catMaybes xs)
  (match xs
    [(Nil) Nil]
    [(Cons (None) rest)   (catMaybes rest)]
    [(Cons (Some x) rest) (Cons x (catMaybes rest))]))

(: read-items (-> String (IO (List Item))))
(define (read-items path)
  (do [r <- (try (read-file path))]
    (match r
      [(Err _)        (pure Nil)]            ; missing file = empty
      [(Ok contents)
       (let* ([lines (string-split "\n" contents)]
               [nonempty (filter (lambda (s) (not (== s ""))) lines)])
         (pure (catMaybes (fmap parse-item nonempty))))])))

(: write-items (-> String (-> (List Item) (IO Unit))))
(define (write-items path items)
  (let* ([lines (fmap render-item items)]
          [body  (string-join "\n" lines)])
    (write-file path (mappend body "\n"))))

;; ----- subcommands ---------------------------------------------
;; Forward-declared so the mutually-referential defs below can find
;; one another regardless of source order.

(: cmd-list        (IO Unit))
(: print-items     (-> (List Item) (-> Integer (IO Unit))))
(: cmd-add         (-> (List String) (IO Unit)))
(: snoc            (-> (List a) (-> a (List a))))
(: cmd-done        (-> (List String) (IO Unit)))
(: mark-done       (-> (List Item) (-> Integer (Result String (List Item)))))
(: mark-done-from  (-> (List Item) (-> Integer (-> Integer
                                                  (Result String (List Item))))))
(: cmd-clear       (IO Unit))

(define cmd-list
  (do [path  <- todo-file]
      [items <- (read-items path)]
    (print-items items 1)))

(define (print-items items n)
  (match items
    [(Nil) (pure Unit)]
    [(Cons it rest)
     (do [_ <- (println (mappend (integer->string n)
                            (mappend ". " (render-item it))))]
       (print-items rest (+ n 1)))]))

(define (cmd-add args)
  (match args
    [(Nil) (println "usage: todo add <task>")]
    [_
     (let* ([text   (string-join " " args)]
             [new-it (Item #f text)])
       (do [path  <- todo-file]
           [items <- (read-items path)]
           [_     <- (write-items path (snoc items new-it))]
         (println (mappend "added: " text))))]))

;; snoc — append a single element on the right.
(define (snoc xs y)
  (match xs
    [(Nil)        (Cons y Nil)]
    [(Cons h t)   (Cons h (snoc t y))]))

(define (cmd-done args)
  (match args
    [(Cons s _)
     (match (string->integer s)
       [(None)   (println (mappend "not a number: " s))]
       [(Some n)
        (do [path  <- todo-file]
            [items <- (read-items path)]
          (match (mark-done items n)
            [(Err msg) (println msg)]
            [(Ok new)
             (do [_ <- (write-items path new)]
               (println (mappend "marked done: #" (integer->string n))))]))])]
    [(Nil) (println "usage: todo done <N>")]))

;; Mark the 1-based n'th item done.  Returns the rebuilt list, or an
;; error message if n is out of range.  Implemented as two pieces so
;; the recursion stays at the top level.
(define (mark-done items n) (mark-done-from items n 1))

(define (mark-done-from xs target i)
  (match xs
    [(Nil) (Err (mappend "no such item: #" (integer->string target)))]
    [(Cons (Item _ text) rest) :when (== i target)
     (Ok (Cons (Item #t text) rest))]
    [(Cons h rest)
     (match (mark-done-from rest target (+ i 1))
       [(Err e)  (Err e)]
       [(Ok new) (Ok (Cons h new))])]))

(define cmd-clear
  (do [path  <- todo-file]
      [items <- (read-items path)]
      (let* ([keep (filter (lambda (it)
                              (match it [(Item done _) (not done)]))
                            items)]
              [dropped (- (length items) (length keep))])
        (do [_ <- (write-items path keep)]
          (println (mappend "cleared "
                       (mappend (integer->string dropped)
                           " items")))))))

;; ----- usage ---------------------------------------------------

(: usage (IO Unit))
(define usage
  (do [_ <- (println "Usage:")]
      [_ <- (println "  todo add <task>")]
      [_ <- (println "  todo list")]
      [_ <- (println "  todo done <N>")]
      (println "  todo clear")))

;; ----- entry point ---------------------------------------------

(: main (IO Unit))
(define main
  (do [args <- argv]
    (match args
      [(Cons "add"   rest) (cmd-add  rest)]
      [(Cons "list"  _)    cmd-list]
      [(Cons "done"  rest) (cmd-done rest)]
      [(Cons "clear" _)    cmd-clear]
      [_                   usage])))

;; Top-level: run when the module is loaded (the same idiom calc.rkt
;; uses to make `racket examples/todo.rkt …` execute).
(define _ignored (run-io main))

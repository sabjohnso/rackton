#lang racket/base

;; Drives examples/todo.rkt as a subprocess so each scenario starts
;; with a fresh module-instance.  We can't use the single-namespace
;; dynamic-require trick (as calc-demo-test does) because we need to
;; invoke the program multiple times in sequence; subprocess gives us
;; that cleanly.

(require rackunit
         racket/file
         racket/path
         racket/string
         racket/system
         racket/port)

;; The test file lives at <project>/tests/; the example lives at
;; <project>/examples/todo.rkt.
(define todo-rkt
  (path->string
   (build-path (path-only (path->complete-path "todo-demo-test.rkt"))
               'up "examples" "todo.rkt")))

(define racket-exe (find-executable-path "racket"))

;; Run the program with the given argv and TODO_FILE.  Returns the
;; captured stdout as a string.
(define (run-todo todo-file . args)
  (define env (environment-variables-copy (current-environment-variables)))
  (environment-variables-set! env
                              #"TODO_FILE"
                              (string->bytes/utf-8 todo-file))
  (parameterize ([current-environment-variables env])
    (define out (open-output-string))
    (parameterize ([current-output-port out])
      (apply system* racket-exe todo-rkt args))
    (get-output-string out)))

;; Each test gets a fresh todo file path.
(define (with-fresh-todo-file proc)
  (define tmp (make-temporary-file "todo-test-~a.txt"))
  (delete-file tmp)  ;; the program creates it on first add
  (dynamic-wind
   void
   (lambda () (proc (path->string tmp)))
   (lambda () (when (file-exists? tmp) (delete-file tmp)))))

;; ---------- scenarios -------------------------------------------

(test-case "no args prints usage"
  (with-fresh-todo-file
   (lambda (tf)
     (define out (run-todo tf))
     (check-regexp-match #rx"Usage:" out)
     (check-regexp-match #rx"todo add <task>" out))))

(test-case "list on a missing file is empty"
  (with-fresh-todo-file
   (lambda (tf)
     (define out (run-todo tf "list"))
     (check-equal? out ""))))

(test-case "add then list shows the item"
  (with-fresh-todo-file
   (lambda (tf)
     (run-todo tf "add" "buy" "milk")
     (define out (run-todo tf "list"))
     (check-equal? out "1. [ ] buy milk\n"))))

(test-case "two adds, then list"
  (with-fresh-todo-file
   (lambda (tf)
     (run-todo tf "add" "buy" "milk")
     (run-todo tf "add" "walk" "dog")
     (define out (run-todo tf "list"))
     (check-equal? out "1. [ ] buy milk\n2. [ ] walk dog\n"))))

(test-case "done flips an item"
  (with-fresh-todo-file
   (lambda (tf)
     (run-todo tf "add" "task" "one")
     (run-todo tf "add" "task" "two")
     (run-todo tf "done" "1")
     (define out (run-todo tf "list"))
     (check-equal? out "1. [x] task one\n2. [ ] task two\n"))))

(test-case "clear drops done items"
  (with-fresh-todo-file
   (lambda (tf)
     (run-todo tf "add" "a")
     (run-todo tf "add" "b")
     (run-todo tf "add" "c")
     (run-todo tf "done" "2")
     (define clear-out (run-todo tf "clear"))
     (check-regexp-match #rx"cleared 1 items" clear-out)
     (define out (run-todo tf "list"))
     (check-equal? out "1. [ ] a\n2. [ ] c\n"))))

(test-case "done with out-of-range index reports an error"
  (with-fresh-todo-file
   (lambda (tf)
     (run-todo tf "add" "only" "one")
     (define out (run-todo tf "done" "5"))
     (check-regexp-match #rx"no such item: #5" out))))

(test-case "done with non-numeric arg reports an error"
  (with-fresh-todo-file
   (lambda (tf)
     (define out (run-todo tf "done" "abc"))
     (check-regexp-match #rx"not a number: abc" out))))

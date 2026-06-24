#lang rackton

;; greet.rkt — a small command-line tool built with rackton/cmdline,
;; the cmdliner-style command-line library.
;;
;; It shows the whole pipeline: a converter (conv-int), a flag, an
;; option with a value, a positional argument, the four-piece
;; assembly with `let+`, and `eval` — which parses argv, prints
;; auto-generated --help / --version, reports errors, and returns a
;; conventional exit code.
;;
;; Run it with:
;;   racket examples/greet.rkt [OPTION]... [NAME]
;;   racket examples/greet.rkt --help
;;   racket examples/greet.rkt --version
;;
;; Examples:
;;   racket examples/greet.rkt                 => Hello, world!
;;   racket examples/greet.rkt Ada             => Hello, Ada!
;;   racket examples/greet.rkt --loud Ada      => HELLO, ADA!
;;   racket examples/greet.rkt -r 3 Ada        => three lines
;;   racket examples/greet.rkt --nope          => error, exit status 124

(require rackton/cmdline
         rackton/system
         rackton/text/string)

;; ----- the work the tool performs -------------------------------

;; print `s` exactly `n` times.
(: print-n (-> Integer String (IO Unit)))
(define (print-n n s)
  (if (<= n 0)
      (pure Unit)
      (do [_ <- (println s)] (print-n (- n 1) s))))

;; build the greeting from the parsed arguments and emit it.
(: run-greet (-> Boolean Integer String (IO Unit)))
(define (run-greet loud times name)
  (let* ([msg   (mappend "Hello, " (mappend name "!"))]
         [shown (if loud (to-upper-string msg) msg)])
    (print-n times shown)))

;; ----- the command-line interface -------------------------------

;; A Term that assembles the three arguments and yields the IO action.
(: greeting (Term (IO Unit)))
(define greeting
  (let+ ([loud  (value (flag (info-doc "Shout the greeting"
                                       (arg-info (list "l" "loud")))))]
         [times (value (opt conv-int 1
                            (info-docv "N"
                             (info-doc "Repeat the greeting N times"
                                       (arg-info (list "r" "repeat"))))))]
         [name  (value (pos 0 conv-string "world"
                            (info-docv "NAME"
                             (info-doc "Who to greet"
                                       (arg-info Nil)))))])
    (run-greet loud times name)))

(: greet-cmd (Cmd (IO Unit)))
(define greet-cmd
  (cmd-v (cmd-version "1.0"
          (cmd-doc "print a friendly greeting" (cmd-info "greet")))
         greeting))

;; eval parses argv, runs the action (or prints help/version/error),
;; and returns the exit code; exit-with-code makes it the process status.
(: main Unit)
(define main (run-io (do [code <- (eval greet-cmd)] (exit-with-code code))))

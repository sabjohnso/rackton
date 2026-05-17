#lang racket/base

;; End-to-end exercise of the Phase-7 IO monad and string / numeric
;; stdlib.  IO values built with `do` are executed by `run-io`.

(require rackunit
         racket/port
         "../main.rkt")

(rackton
  ;; Build an IO action that prints a greeting using string-append.
  (: greet (-> String (IO Unit)))
  (define (greet name)
    (println (string-append "hello, " name)))

  ;; Sequence two greetings via do-notation; bind chain over IO.
  (: greet-both (-> String (-> String (IO Unit))))
  (define (greet-both a b)
    (do [_ <- (greet a)]
        [_ <- (greet b)]
      (pure-io MkUnit)))

  ;; Numeric helpers + integer->string.
  (define numeric-show
    (string-append "abs=-3 -> "
                   (integer->string (abs (- 0 3)))))

  ;; substring slice
  (define slice (substring "hello, world" 7 12)))

;; ----- value-level checks ------------------------------------

(test-case "string + numeric helpers"
  (check-equal? slice "world")
  (check-equal? numeric-show "abs=-3 -> 3"))

(test-case "IO actions run via run-io and produce output"
  (define out
    (with-output-to-string
      (lambda () (run-io (greet-both "Alice" "Bob")))))
  (check-equal? out "hello, Alice\nhello, Bob\n"))

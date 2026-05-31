#lang racket/base

;; rackton/text/show — ShowS-style display helpers (Text.Show).
;; A ShowS is (-> String String): a difference list that prepends its
;; text to a continuation, so compositions concatenate in linear time.

(require rackunit
         "../main.rkt")

(rackton
  (require rackton/text/show)

  (: r-string String) (define r-string (run-shows (show-string "abc")))
  (: r-shows  String) (define r-shows  (run-shows (shows 42)))
  (: r-char   String) (define r-char   (run-shows (show-char #\Z)))

  (: r-paren-t String) (define r-paren-t (run-shows (show-paren #t (shows 42))))
  (: r-paren-f String) (define r-paren-f (run-shows (show-paren #f (shows 42))))

  ;; chaining: "x=" . shows 5 . "!"
  (: r-chain String)
  (define r-chain
    (run-shows (lambda (s)
                 ((show-string "x=") ((shows 5) ((show-string "!") s)))))))

;; ---------- assertions ---------------------------------------

(test-case "primitive ShowS"
  (check-equal? r-string "abc")
  (check-equal? r-shows "42")
  (check-equal? r-char "Z"))

(test-case "show-paren"
  (check-equal? r-paren-t "(42)")
  (check-equal? r-paren-f "42"))

(test-case "composition threads the continuation"
  (check-equal? r-chain "x=5!"))

#lang racket/base

;; rackton/text/bytes — derived Bytes operations (Data.ByteString).

(require rackunit
         "../main.rkt")

(rackton
  (require rackton/text/bytes)

  ;; helper: render Bytes back to a String for assertions
  (: b->s (-> Bytes String))
  (define (b->s b) (match (bytes->string b) [(Some s) s] [(None) "?"]))

  (: e-null Boolean) (define e-null (bytes-null? bytes-empty))
  (: h-null Boolean) (define h-null (bytes-null? (string->bytes "hi")))

  (: tk String) (define tk (b->s (bytes-take 3 (string->bytes "hello"))))
  (: dp String) (define dp (b->s (bytes-drop 3 (string->bytes "hello"))))

  (: sp-l String) (define sp-l (match (bytes-split 2 (string->bytes "hello")) [(MkPair a _) (b->s a)]))
  (: sp-r String) (define sp-r (match (bytes-split 2 (string->bytes "hello")) [(MkPair _ b) (b->s b)]))

  (: cc String)
  (define cc (b->s (bytes-concat (list (string->bytes "ab")
                                       (string->bytes "cd")
                                       (string->bytes "ef"))))))

;; ---------- assertions ---------------------------------------

(test-case "null / empty"
  (check-true  e-null)
  (check-false h-null))

(test-case "take / drop"
  (check-equal? tk "hel")
  (check-equal? dp "lo"))

(test-case "split"
  (check-equal? sp-l "he")
  (check-equal? sp-r "llo"))

(test-case "concat"
  (check-equal? cc "abcdef"))

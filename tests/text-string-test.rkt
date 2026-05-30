#lang racket/base

;; rackton/text/string — String operations.

(require rackunit
         "../main.rkt")

(rackton
  (require rackton/text/string)

  (: ns0 Boolean) (define ns0 (null-string? ""))
  (: ns1 Boolean) (define ns1 (null-string? "a"))
  (: rev String)  (define rev (reverse-string "abc"))
  (: up String)   (define up (to-upper-string "aB3"))
  (: lo String)   (define lo (to-lower-string "Ab"))

  (: st String)  (define st (strip "  hi  "))
  (: sts String) (define sts (strip-start "  hi"))
  (: ste String) (define ste (strip-end "hi  "))

  (: sk (List String)) (define sk (split-keep #\, "a,b,,c"))
  (: ln  (List String)) (define ln  (lines "a\nb\nc"))
  (: ln2 (List String)) (define ln2 (lines "a\n"))
  (: ln3 (List String)) (define ln3 (lines ""))
  (: ln4 (List String)) (define ln4 (lines "a\n\nb"))
  (: wd  (List String)) (define wd  (words "  hello   world  "))
  (: wd0 (List String)) (define wd0 (words "   "))

  (: uw String) (define uw (unwords (list "a" "b" "c")))
  (: ul String) (define ul (unlines (list "a" "b"))))

;; ---------- assertions ---------------------------------------

(define (lst . xs) (let loop ([xs xs]) (if (null? xs) Nil (Cons (car xs) (loop (cdr xs))))))

(test-case "predicates / reverse / case"
  (check-equal? ns0 #t) (check-equal? ns1 #f)
  (check-equal? rev "cba")
  (check-equal? up "AB3") (check-equal? lo "ab"))

(test-case "strip"
  (check-equal? st "hi") (check-equal? sts "hi") (check-equal? ste "hi"))

(test-case "split-keep / lines / words"
  (check-equal? sk  (lst "a" "b" "" "c"))
  (check-equal? ln  (lst "a" "b" "c"))
  (check-equal? ln2 (lst "a"))
  (check-equal? ln3 Nil)
  (check-equal? ln4 (lst "a" "" "b"))
  (check-equal? wd  (lst "hello" "world"))
  (check-equal? wd0 Nil))

(test-case "joining"
  (check-equal? uw "a b c")
  (check-equal? ul "a\nb\n"))

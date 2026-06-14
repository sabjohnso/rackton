#lang racket/base

;; Quote / quasiquote / unquote list literals.
;;
;; `'(1 2 3)` and friends desugar (in the parser) to Cons/Nil chains of
;; literals, so ordinary inference types them as homogeneous `(List a)`.
;; Quasiquote adds `,` (unquote, evaluate a Rackton expression) and `,@`
;; (unquote-splicing, concatenate a list-typed expression).  Heterogeneous
;; quoted lists are rejected by unification, and unquote/unquote-splicing
;; outside a quasiquote is a compile error (Racket semantics).

(require rackunit
         (for-syntax racket/base)
         "../main.rkt")

;; Forces macro expansion (parse → infer → codegen) in this lexical
;; context, so ill-typed / malformed blocks raise at compile time.
(define-syntax-rule (check-rackton-compile-error form ...)
  (check-exn
   exn:fail?
   (lambda ()
     (eval #'(rackton form ...)
           (variable-reference->namespace (#%variable-reference))))))

;; ----- value-level checks -----

(rackton
  (define nums   '(1 2 3))
  (define syms   '(a b c))
  (define strs   '("a" "b" "c"))
  (define nested `((abc) (def)))
  (define spliced `(1 2 ,@(list 3 4) 5))
  (define unq    `(,(Pair "abc" 123) ,(Pair "def" 456)))
  (define empt   '())

  ;; Racket-side comparison references, built with the existing `(list …)`
  ;; literal (same Cons/Nil structure).
  (define ref-nums    (list 1 2 3))
  (define ref-syms    (list 'a 'b 'c))
  (define ref-strs    (list "a" "b" "c"))
  (define ref-nested  (list (list 'abc) (list 'def)))
  (define ref-spliced (list 1 2 3 4 5))
  (define ref-unq     (list (Pair "abc" 123) (Pair "def" 456)))
  (define ref-empty   (list)))

(test-case "quote of numbers is a List Integer"
  (check-equal? nums ref-nums))

(test-case "quote of identifiers is a List Symbol"
  (check-equal? syms ref-syms))

(test-case "quote of strings is a List String"
  (check-equal? strs ref-strs))

(test-case "quasiquote nests plain lists"
  (check-equal? nested ref-nested))

(test-case "unquote-splicing concatenates"
  (check-equal? spliced ref-spliced))

(test-case "unquote evaluates a Rackton expression"
  (check-equal? unq ref-unq))

(test-case "empty quote is Nil"
  (check-equal? empt ref-empty))

;; ----- type pinning via `ann` -----

(test-case "a correct annotation type-checks"
  (check-not-exn
   (lambda ()
     (eval #'(rackton (define xs (ann '(1 2 3) (List Integer))))
           (variable-reference->namespace (#%variable-reference))))))

(test-case "a wrong annotation is rejected"
  (check-rackton-compile-error
   (define xs (ann '(1 2 3) (List String)))))

;; ----- heterogeneous list is a type error -----

(test-case "heterogeneous quoted list is rejected"
  ;; `Pair` fixes the element type to Symbol; "abc" is a String → clash.
  (check-rackton-compile-error
   (define x '(Pair "abc" 1 2 3))))

;; ----- unquote outside quasiquote is an error -----

(test-case "bare unquote is rejected"
  (check-rackton-compile-error
   (define y 0)
   (define x ,y)))

(test-case "bare unquote-splicing is rejected"
  (check-rackton-compile-error
   (define ys (list 1 2))
   (define x ,@ys)))

(test-case "unquote inside plain quote is rejected"
  (check-rackton-compile-error
   (define y 0)
   (define x '(1 ,y 3))))

;; ----- quoted / quasiquoted patterns -----

(rackton
  ;; quote builds a fixed-shape Cons/Nil pattern
  (define (is123 xs) (match xs ['(1 2 3) #t] [_ #f]))
  (define (abc?  xs) (match xs ['(a b c) #t] [_ #f]))
  (define (empty? xs) (match xs ['() #t] [_ #f]))

  ;; quasiquote with an unquoted sub-pattern binds the middle element
  (define (mid xs) (match xs [`(1 ,x 3) x] [_ 0]))

  ;; results, computed in Rackton, checked at the Racket level
  (define p-is123-ok (is123 '(1 2 3)))
  (define p-is123-el (is123 '(1 2 4)))
  (define p-is123-ln (is123 (list 1 2)))
  (define p-abc-ok   (abc? '(a b c)))
  (define p-abc-no   (abc? '(a b d)))
  (define p-empty-y  (empty? (ann Nil (List Integer))))
  (define p-empty-n  (empty? (list 1)))
  (define p-mid-ok   (mid '(1 99 3)))
  (define p-mid-no   (mid '(1 99 4))))

(test-case "quoted integer-list pattern"
  (check-equal? p-is123-ok #t)
  (check-equal? p-is123-el #f)   ;; wrong element
  (check-equal? p-is123-ln #f))  ;; wrong length

(test-case "quoted symbol-list pattern"
  (check-equal? p-abc-ok #t)
  (check-equal? p-abc-no #f))

(test-case "quoted empty-list pattern matches Nil"
  (check-equal? p-empty-y #t)
  (check-equal? p-empty-n #f))

(test-case "quasiquoted pattern binds an unquoted sub-pattern"
  (check-equal? p-mid-ok 99)
  (check-equal? p-mid-no 0))   ;; last element isn't 3

(test-case "bare unquote pattern is rejected"
  (check-rackton-compile-error
   (define (f xs) (match xs [,y 0]))))

(test-case "unquote inside a quoted pattern is rejected"
  (check-rackton-compile-error
   (define (f xs) (match xs ['(1 ,y 3) 0]))))

(test-case "unquote-splicing in a pattern is rejected"
  (check-rackton-compile-error
   (define (f xs) (match xs [`(1 ,@ys 3) 0]))))

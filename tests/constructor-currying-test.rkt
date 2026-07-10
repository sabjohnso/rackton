#lang racket/base

;; A data constructor is a first-class CURRIED value.  Its Hindley–Milner
;; type `(-> a b T)` is curried, so it must apply stepwise (`((MkP a) b)`)
;; and as a partial application (`(MkP a)`), not only saturated
;; (`(MkP a b)`).  Before the fix, a bare n-ary constructor was the raw
;; n-ary struct procedure — stepwise application raised a runtime arity
;; mismatch, and a direct partial application `(MkP a)` failed to compile.

(require rackunit
         (only-in racket/port with-output-to-string)
         rackton)

;; Passing a user constructor by reference and applying it stepwise, all
;; at once, and as a nested-then-completed partial application.
(rackton
 (data (P a b) (MkP a b))
 (: fst-of (-> (P a b) a))
 (define (fst-of p) (match p ((MkP a _) a)))
 (: snd-of (-> (P a b) b))
 (define (snd-of p) (match p ((MkP _ b) b)))

 (: apply-curried (-> (-> a (-> b c)) a b c))
 (define (apply-curried f x y) ((f x) y))

 ;; stepwise via a higher-order function
 (define stepwise (apply-curried MkP 1 2))
 ;; direct partial application, completed later
 (define partial  (MkP 3))
 (define completed (partial 4))
 ;; saturated, unchanged
 (define saturated (MkP 5 6))

 (provide fst-of snd-of stepwise partial completed saturated))

(check-equal? (fst-of stepwise) 1)
(check-equal? (snd-of stepwise) 2)
(check-true (procedure? partial))
(check-equal? (fst-of completed) 3)
(check-equal? (snd-of completed) 4)
(check-equal? (fst-of saturated) 5)
(check-equal? (snd-of saturated) 6)

;; A partially applied constructor passed to `fmap`.
(rackton
 (data (Q a b) (MkQ a b))
 (: qfst (-> (Q a b) a))
 (define (qfst p) (match p ((MkQ a _) a)))
 (: qsnd (-> (Q a b) b))
 (define (qsnd p) (match p ((MkQ _ b) b)))
 ;; (fmap (MkQ 0) xs) : List (Q Int Int) — MkQ is partially applied
 (: labelled (List (Q Integer Integer)))
 (define labelled (fmap (MkQ 0) (Cons 1 (Cons 2 (Cons 3 Nil)))))
 ;; every first is the label 0; the seconds are the original elements
 (: firsts-str String)
 (define firsts-str (show (fmap qfst labelled)))
 (: seconds-str String)
 (define seconds-str (show (fmap qsnd labelled)))
 (provide firsts-str seconds-str))

(check-equal? firsts-str  "[0 0 0]")
(check-equal? seconds-str "[1 2 3]")

;; The prelude tuple constructor `Pair` curries the same way.
(rackton
 (: pair-curried (-> a (-> b (Pair a b))))
 (define (pair-curried x) (Pair x))
 (: mk-str String)
 (define mk-str (show ((pair-curried 7) 8)))
 (provide pair-curried mk-str))

(check-equal? mk-str "(7, 8)")

#lang rackton

;; The `(list e ...)` surface form: sugar for a Cons/Nil chain.
;; `(list a b c)` desugars to `(Cons a (Cons b (Cons c Nil)))`, and
;; `(list)` to `Nil`.  It is purely a parser-level desugaring, so the
;; resulting value is an ordinary `(List a)`.

(require "../unit.rkt")

(: threes (List Integer))
(define threes (list 1 2 3))

(: total Integer)
(define total (sum threes))

(: count Integer)
(define count (length threes))

;; Empty literal.
(: empty (List Integer))
(define empty (list))

(: empty-count Integer)
(define empty-count (length empty))

;; Nested literals.
(: nested (List (List Integer)))
(define nested (list (list 1) (list 2 3)))

(: nested-total Integer)
(define nested-total (sum (fmap sum nested)))

;; Eq on lists, defined locally (the prelude has none for List), so we
;; can compare the literal structurally inside Rackton too.
(: int-list-eq (-> (List Integer) (-> (List Integer) Boolean)))
(define (int-list-eq xs ys)
  (match xs
    [(Nil)       (match ys [(Nil) #t] [(Cons _ _) #f])]
    [(Cons a as) (match ys
                   [(Nil)       #f]
                   [(Cons b bs) (if (== a b) (int-list-eq as bs) #f)])]))

;; Desugars to exactly the Cons/Nil chain.
(: same-as-cons Boolean)
(define same-as-cons
  (int-list-eq threes (Cons 1 (Cons 2 (Cons 3 Nil)))))

(: suite (List Test))
(define suite
  (list
    (it "list literal builds the right Cons/Nil chain"
        (check-true same-as-cons))
    (it "list literal length and sum"
        (all-checks
          (list (check-equal? total 6)
                (check-equal? count 3))))
    (it "empty list literal is Nil"
        (check-equal? empty-count 0))
    (it "nested list literals"
        (check-equal? nested-total 6))))

(: test-main (IO Unit))
(define test-main (run-suite "list-literal" suite))

#lang racket/base

;; rackton/data/{bool,function,ord,functor} — small Data.* utilities.

(require rackunit
         "../main.rkt")

(rackton
  (require rackton/data/bool)
  (require rackton/data/function)
  (require rackton/data/ord)
  (require rackton/data/functor)

  (: b-t String) (define b-t (bool "F" "T" #t))
  (: b-f String) (define b-f (bool "F" "T" #f))
  (: oth Boolean) (define oth otherwise)

  (: r-on Integer)     (define r-on (on (lambda (a b) (+ a b)) (lambda (x) (* x x)) 2 3))
  (: r-apply Integer)  (define r-apply (apply-to 5 (lambda (x) (* x 2))))

  (: c-hi Integer) (define c-hi (clamp 0 10 15))
  (: c-lo Integer) (define c-lo (clamp 0 10 -3))
  (: c-in Integer) (define c-in (clamp 0 10 5))
  (: mnb Integer)  (define mnb (min-by (lambda (x) (- 10 x)) 3 7))
  (: mxb Integer)  (define mxb (max-by (lambda (x) (- 10 x)) 3 7))

  (: cm-some (Maybe Integer)) (define cm-some (const-map 9 (Some 5)))
  (: cm-list (List Integer))  (define cm-list (const-map 0 (list 1 2 3)))
  (: ff (Maybe Integer))      (define ff (fmap-flipped (Some 5) (lambda (x) (* x 2)))))

;; ---------- assertions ---------------------------------------

(test-case "data/bool"
  (check-equal? b-t "T") (check-equal? b-f "F") (check-equal? oth #t))

(test-case "data/function"
  (check-equal? r-on 13)        ; (2*2) + (3*3)
  (check-equal? r-apply 10))

(test-case "data/ord"
  (check-equal? c-hi 10) (check-equal? c-lo 0) (check-equal? c-in 5)
  (check-equal? mnb 7)          ; key 10-x: smaller key (3) is x=7
  (check-equal? mxb 3))

(test-case "data/functor"
  (check-equal? cm-some (Some 9))
  (check-equal? cm-list (Cons 0 (Cons 0 (Cons 0 Nil))))
  (check-equal? ff (Some 10)))

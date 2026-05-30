#lang racket/base

;; rackton/data/foldable — generic folds over List and Maybe.

(require rackunit
         "../main.rkt")

(rackton
  (require rackton/data/foldable)

  (: fm-list String) (define fm-list (fold-map (lambda (n) (integer->string n)) (list 1 2 3)))
  (: fm-some String) (define fm-some (fold-map (lambda (n) (integer->string n)) (Some 5)))
  (: fm-none String) (define fm-none (fold-map (lambda (n) (integer->string n)) (ann None (Maybe Integer))))

  (: fold-list String) (define fold-list (fold (list "a" "b" "c")))

  (: any-l Boolean) (define any-l (any-of (lambda (x) (> x 2)) (list 1 2 3)))
  (: any-s Boolean) (define any-s (any-of (lambda (x) (> x 2)) (Some 5)))
  (: any-n Boolean) (define any-n (any-of (lambda (x) (> x 2)) (ann None (Maybe Integer))))
  (: all-l Boolean) (define all-l (all-of (lambda (x) (> x 0)) (list 1 2 3)))
  (: all-l2 Boolean)(define all-l2 (all-of (lambda (x) (> x 1)) (list 1 2 3)))

  (: el-l Boolean) (define el-l (elem-of 2 (list 1 2 3)))
  (: el-s Boolean) (define el-s (elem-of 2 (Some 2)))
  (: el-n Boolean) (define el-n (elem-of 2 (ann None (Maybe Integer)))))

;; ---------- assertions ---------------------------------------

(test-case "fold-map / fold (Monoid over Foldable)"
  (check-equal? fm-list "123")
  (check-equal? fm-some "5")
  (check-equal? fm-none "")
  (check-equal? fold-list "abc"))

(test-case "any-of / all-of over List and Maybe"
  (check-equal? any-l #t) (check-equal? any-s #t) (check-equal? any-n #f)
  (check-equal? all-l #t) (check-equal? all-l2 #f))

(test-case "elem-of over List and Maybe"
  (check-equal? el-l #t) (check-equal? el-s #t) (check-equal? el-n #f))

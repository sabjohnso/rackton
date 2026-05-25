#lang racket/base

;; Prisms and traversals.

(require rackunit
         "../main.rkt")

(rackton
  ;; ----- 48.A Maybe prisms -----------------------------------

  ;; A prism focusing on the `Some` constructor of `Maybe a`.
  (: some-prism (Prism (Maybe a) a))
  (define some-prism
    (MkPrism (lambda (m) (match m [(Some x) (Some x)] [(None) None]))
             Some))

  ;; A prism focusing on `None` — extracts Unit; builds None ignoring
  ;; its arg.
  (: none-prism (Prism (Maybe a) Unit))
  (define none-prism
    (MkPrism (lambda (m) (match m [(None) (Some MkUnit)] [(Some _) None]))
             (lambda (_) None)))

  (: prev-some-on-some (Maybe Integer))
  (define prev-some-on-some (preview some-prism (Some 7)))

  (: prev-some-on-none (Maybe Integer))
  (define prev-some-on-none (preview some-prism None))

  (: rev-some (Maybe Integer))
  (define rev-some (review some-prism 42))

  (: prev-none-on-none (Maybe Unit))
  (define prev-none-on-none (preview none-prism None))

  ;; ----- 48.B Traversals over lists --------------------------

  (: nums (List Integer))
  (define nums (Cons 1 (Cons 2 (Cons 3 Nil))))

  (: nums-collected (List Integer))
  (define nums-collected (to-list-of list-traversal nums))

  (: nums-doubled (List Integer))
  (define nums-doubled
    (over-of list-traversal (lambda (n) (* n 2)) nums))

  ;; ----- 48.C lens-as-traversal ------------------------------

  (define-struct Point
    [x : Integer]
    [y : Integer]
    #:deriving Lens)

  (: x-trav (Traversal Point Integer))
  (define x-trav (lens-as-traversal Point-x-lens))

  (: p0 Point)
  (define p0 (Point 3 7))

  (: x-collected (List Integer))
  (define x-collected (to-list-of x-trav p0))

  (: x-bumped Point)
  (define x-bumped (over-of x-trav (lambda (n) (+ n 1)) p0)))

;; ---------- assertions ---------------------------------------

(test-case "Some-prism preview / review"
  (check-equal? prev-some-on-some  (Some 7))
  (check-equal? prev-some-on-none  None)
  (check-equal? rev-some           (Some 42)))

(test-case "None-prism preview on None"
  (check-equal? prev-none-on-none (Some MkUnit)))

(test-case "list-traversal gathers and transforms"
  (check-equal? nums-collected nums)
  (check-equal? nums-doubled   (Cons 2 (Cons 4 (Cons 6 Nil)))))

(test-case "lens-as-traversal: single-focus traversal"
  (check-equal? x-collected (Cons 3 Nil))
  (check-equal? x-bumped    (Point 4 7)))

#lang racket/base

;; Tests for the cyclic-default extension of Applicative and Monad:
;;
;;   - Monad: defining either `flatmap` OR `join` is sufficient; the
;;     other is derived.
;;   - Applicative: defining any one of `fapply`, `liftA2`, `product`
;;     is sufficient; the other two are derived.
;;
;; A separate test verifies that omitting all cycle members raises a
;; targeted compile-time error rather than a runtime infinite recursion.

(require rackunit
         (only-in racket/base parameterize)
         "../main.rkt")

;; ----- prelude methods exist on existing instances -----------------

(rackton
  ;; join on the prelude's Monad Maybe: derived from its flatmap default.
  (: jm Integer)
  (define jm
    (match (join (Some (Some 7))) [(None) 0] [(Some x) x]))

  ;; product on the prelude's Applicative Maybe: derived via liftA2.
  (: pm Integer)
  (define pm
    (match (product (Some 3) (Some 4))
      [(None) 0]
      [(Some p) (match p [(MkPair a b) (+ a b)])])))

(test-case "join derived via Monad Maybe default"
  (check-equal? jm 7))

(test-case "product derived via Applicative Maybe default"
  (check-equal? pm 7))

;; ----- instance defines only `join` --------------------------------

(rackton
  (define-data (Box a) (MkBox a))

  (instance (Functor Box)
    (define (fmap f b) (match b [(MkBox x) (MkBox (f x))])))

  (instance (Applicative Box)
    (define (pure x)       (MkBox x))
    (define (fapply bf bx) (match bf [(MkBox f) (fmap f bx)])))

  ;; Only join is defined; flatmap must derive.
  (instance (Monad Box)
    (define (join bb) (match bb [(MkBox b) b])))

  (: bind-via-join Integer)
  (define bind-via-join
    (match (flatmap (lambda (x) (MkBox (+ x 10))) (MkBox 5))
      [(MkBox v) v])))

(test-case "instance with join-only: flatmap derives correctly"
  (check-equal? bind-via-join 15))

;; ----- instance defines only `liftA2` ------------------------------

(rackton
  (define-data (Wrap a) (MkWrap a))

  (instance (Functor Wrap)
    (define (fmap f w) (match w [(MkWrap x) (MkWrap (f x))])))

  ;; Only liftA2 is defined; fapply and product must derive.
  (instance (Applicative Wrap)
    (define (pure x) (MkWrap x))
    (define (liftA2 g x y)
      (match x [(MkWrap a)
       (match y [(MkWrap b) (MkWrap (g a b))])])))

  (: ap-via-liftA2 Integer)
  (define ap-via-liftA2
    (match (fapply (MkWrap (lambda (x) (+ x 1))) (MkWrap 41))
      [(MkWrap v) v]))

  (: prod-via-liftA2 Integer)
  (define prod-via-liftA2
    (match (product (MkWrap 2) (MkWrap 3))
      [(MkWrap p) (match p [(MkPair a b) (+ a b)])])))

(test-case "instance with liftA2-only: fapply derives"
  (check-equal? ap-via-liftA2 42))

(test-case "instance with liftA2-only: product derives"
  (check-equal? prod-via-liftA2 5))

;; ----- instance defines only `product` -----------------------------

(rackton
  (define-data (Cell a) (MkCell a))

  (instance (Functor Cell)
    (define (fmap f c) (match c [(MkCell x) (MkCell (f x))])))

  ;; Only product is defined; fapply and liftA2 must derive.
  (instance (Applicative Cell)
    (define (pure x) (MkCell x))
    (define (product x y)
      (match x [(MkCell a)
       (match y [(MkCell b) (MkCell (MkPair a b))])])))

  (: ap-via-product Integer)
  (define ap-via-product
    (match (fapply (MkCell (lambda (x) (+ x 1))) (MkCell 41))
      [(MkCell v) v]))

  (: liftA2-via-product Integer)
  (define liftA2-via-product
    (match (liftA2 (lambda (a b) (+ a b)) (MkCell 3) (MkCell 4))
      [(MkCell v) v])))

(test-case "instance with product-only: fapply derives"
  (check-equal? ap-via-product 42))

(test-case "instance with product-only: liftA2 derives"
  (check-equal? liftA2-via-product 7))

;; ----- compile-time error when ALL cycle members are omitted -------

(test-case "instance omitting all of {fapply, liftA2, product} fails at compile time"
  (define ((expand src))
    (parameterize ([current-namespace (make-base-namespace)])
      (eval src)))
  (check-exn
   exn:fail?
   (expand
    '(module test-cycle racket/base
       (require rackton)
       (rackton
        (define-data (Stub a) (MkStub a))
        (instance (Functor Stub)
          (define (fmap f s) (match s [(MkStub x) (MkStub (f x))])))
        (instance (Applicative Stub)
          (define (pure x) (MkStub x))))))))

(test-case "instance omitting both of {flatmap, join} fails at compile time"
  (define ((expand src))
    (parameterize ([current-namespace (make-base-namespace)])
      (eval src)))
  (check-exn
   exn:fail?
   (expand
    '(module test-cycle-monad racket/base
       (require rackton)
       (rackton
        (define-data (Stub2 a) (MkStub2 a))
        (instance (Functor Stub2)
          (define (fmap f s) (match s [(MkStub2 x) (MkStub2 (f x))])))
        (instance (Applicative Stub2)
          (define (pure x) (MkStub2 x))
          (define (fapply sf sx) (match sf [(MkStub2 f) (fmap f sx)])))
        (instance (Monad Stub2)))))))

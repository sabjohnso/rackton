#lang rackton

;; Enabler A regression: a library defining a type with an Applicative
;; instance whose `pure` is return-typed, plus a Monoid whose `mempty`
;; is return-typed.  A client in another module must be able to resolve
;; these on the imported instances.

(provide (all-defined-out))

(data (Box a) (MkBox a))

(: unbox-it (-> (Box a) a))
(define (unbox-it b) (match b [(MkBox x) x]))

(instance (Functor Box)
  (define (fmap f b) (match b [(MkBox x) (MkBox (f x))])))

(instance (Applicative Box)
  (define (pure x) (MkBox x))
  (define (fapply bf bx)
    (match bf [(MkBox f) (fmap f bx)])))

(data (Wrap a) (MkWrap (List a)))

(: unwrap (-> (Wrap a) (List a)))
(define (unwrap w) (match w [(MkWrap xs) xs]))

(instance (Semigroup (Wrap a))
  (define (<> x y)
    (match x [(MkWrap xs)
              (match y [(MkWrap ys) (MkWrap (append xs ys))])])))

(instance (Monoid (Wrap a))
  (define mempty (MkWrap Nil)))

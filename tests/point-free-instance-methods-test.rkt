#lang racket/base

;; An instance method may be given in point-free / value form —
;;   (define method some-other-function)
;; — aliasing a separately-`define`d top-level function rather than
;; spelling out (define (method args) body).  Because instances are
;; code-generated before top-level defs, a NAKED reference to such a def
;; used to forward-reference it ("cannot reference an identifier before
;; its definition").  Codegen now eta-expands a value-form method body to
;; the method's declared arity, deferring the reference to call time.
;;
;; Covers a return-typed method (pure), a positional method (==), and a
;; multi-argument method — plus a regression check that the inline form
;; still works.

(require rackunit
         "../main.rkt")

;; ----- return-typed method, point-free alias to a top-level def ----

(rackton
  (data (Box a) (MkBox a))

  ;; A top-level def the instance aliases.  It is emitted AFTER the
  ;; instance, so a naked reference would forward-reference it.
  (define (box-wrap a) (MkBox a))

  (instance (Functor Box)
    (define (fmap f b) (match b [(MkBox x) (MkBox (f x))])))

  (instance (Applicative Box)
    (define pure box-wrap)                       ; point-free, return-typed
    (define (fapply bf bx) (match bf [(MkBox f) (fmap f bx)])))

  (: pf-pure Integer)
  (define pf-pure (match (pure 7) [(MkBox v) v])))

(test-case "point-free return-typed method (pure) resolves"
  (check-equal? pf-pure 7))

;; ----- positional method, point-free alias to a top-level def ------

(rackton
  (data Color Red Green Blue)

  (define (color-eq a b)
    (match a
      [Red   (match b [Red   #t] [_ #f])]
      [Green (match b [Green #t] [_ #f])]
      [Blue  (match b [Blue  #t] [_ #f])]))

  (instance (Eq Color)
    (define == color-eq))                        ; point-free, positional

  (: pf-eq Boolean)
  (define pf-eq (== Red Red))
  (: pf-neq Boolean)
  (define pf-neq (== Red Blue)))

(test-case "point-free positional method (==) resolves"
  (check-true  pf-eq)
  (check-false pf-neq))

;; ----- multi-argument method, point-free alias --------------------

(rackton
  (protocol (Combine (c :: *))
    (: combine (-> c (-> c c))))

  (data Acc (MkAcc Integer))

  (define (acc-combine x y)
    (match x [(MkAcc a) (match y [(MkAcc b) (MkAcc (+ a b))])]))

  (instance (Combine Acc)
    (define combine acc-combine))                ; point-free, 2-arg

  (: pf-combine Integer)
  (define pf-combine
    (match (combine (MkAcc 3) (MkAcc 4)) [(MkAcc v) v])))

(test-case "point-free multi-argument method resolves"
  (check-equal? pf-combine 7))

;; ----- regression: inline form is unaffected ----------------------

(rackton
  (data (Cell a) (MkCell a))

  (instance (Functor Cell)
    (define (fmap f c) (match c [(MkCell x) (MkCell (f x))])))

  (instance (Applicative Cell)
    (define (pure x) (MkCell x))                 ; inline (already a lambda)
    (define (fapply cf cx) (match cf [(MkCell f) (fmap f cx)])))

  (: inline-pure Integer)
  (define inline-pure (match (pure 9) [(MkCell v) v])))

(test-case "inline method form still works"
  (check-equal? inline-pure 9))

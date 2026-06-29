#lang racket/base

;; Named data fields and keyword construction `(C :f v …)`.
;; Keyword construction must list fields in declared order and is
;; equivalent to the positional call; struct fields work the same way.

(require rackunit
         (for-syntax racket/base)
         "../main.rkt")

(define-syntax-rule (check-rackton-compile-error form ...)
  (check-exn
   exn:fail?
   (lambda ()
     (eval #'(rackton form ...)
           (variable-reference->namespace (#%variable-reference))))))

(rackton
  (data (Maybe a)
    (Some [value : a])
    None)

  (struct Point
    [x : Float]
    [y : Float])

  ;; Keyword construction and positional construction agree.
  (: kw-some (Maybe Integer))
  (define kw-some (Some :value 3))

  (: pos-some (Maybe Integer))
  (define pos-some (Some 3))

  ;; A two-field struct built with keywords, read back by accessor.
  (: pt Point)
  (define pt (Point :x 3.0 :y 4.0))

  (: pt-x Float)
  (define pt-x (Point-x pt))

  (: pt-y Float)
  (define pt-y (Point-y pt))

  ;; Positional struct construction still works.
  (: pt2 Point)
  (define pt2 (Point 3.0 4.0))

  (: same-point Boolean)
  (define same-point (if (== pt-x (Point-x pt2)) #t #f))

  ;; Keyword patterns: same order rule, lowered positionally.
  (: some-val (-> (Maybe Integer) Integer))
  (define (some-val m)
    (match m
      [(Some :value x) x]
      [None            0]))

  (: kw-pat-some Integer)
  (define kw-pat-some (some-val (Some 42)))

  (: kw-pat-none Integer)
  (define kw-pat-none (some-val None))

  (: point-sum (-> Point Float))
  (define (point-sum p)
    (match p
      [(Point :x a :y b) (+ a b)]))

  (: pt-sum Float)
  (define pt-sum (point-sum pt)))

(test-case "keyword ctor equals positional ctor"
  (check-equal? kw-some pos-some))

(test-case "struct keyword fields bind in declared order"
  (check-equal? pt-x 3.0)
  (check-equal? pt-y 4.0))

(test-case "positional struct still works"
  (check-equal? same-point #t))

;; ----- rejections (compile-time) ---------------------------------

(test-case "keyword fields out of declared order are rejected"
  (check-rackton-compile-error
   (struct P [x : Float] [y : Float])
   (define bad (P :y 4.0 :x 3.0))))

(test-case "a wrong field label is rejected"
  (check-rackton-compile-error
   (data (Maybe2 a) (Just [value : a]) Nothing2)
   (define bad (Just :wrong 3))))

(test-case "keyword arguments on a positional constructor are rejected"
  (check-rackton-compile-error
   (data (Pair2 a b) (MkPair2 a b))
   (define bad (MkPair2 :a 1 :b 2))))

(test-case "keyword arguments on a plain function are rejected"
  (check-rackton-compile-error
   (define (f x) x)
   (define bad (f :x 3))))

(test-case "a missing keyword value is rejected"
  (check-rackton-compile-error
   (data (Box a) (MkBox [val : a]))
   (define bad (MkBox :val))))

(test-case "mixing positional and keyword arguments is rejected"
  (check-rackton-compile-error
   (struct P3 [x : Float] [y : Float])
   (define bad (P3 :x 3.0 4.0))))

(test-case "a bare keyword is not an expression"
  (check-rackton-compile-error
   (define bad :value)))

;; ----- keyword patterns ------------------------------------------

(test-case "keyword pattern binds the right field"
  (check-equal? kw-pat-some 42)
  (check-equal? kw-pat-none 0))

(test-case "multi-field keyword pattern binds in declared order"
  (check-equal? pt-sum 7.0))

(test-case "keyword pattern out of declared order is rejected"
  (check-rackton-compile-error
   (struct P4 [x : Float] [y : Float])
   (define (f p) (match p [(P4 :y b :x a) (+ a b)]))))

(test-case "keyword pattern on a positional constructor is rejected"
  (check-rackton-compile-error
   (data (Pair4 a b) (MkPair4 a b))
   (define (f p) (match p [(MkPair4 :a x :b y) x]))))

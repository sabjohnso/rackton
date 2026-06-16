#lang racket/base

;; Variadic tuples (Phase 1).
;;
;; A tuple is a heterogeneous, fixed-arity product written `(tuple e …)`
;; with type `(Tuple T …)`.  Unlike the old `Pair` / `Tuple3…7` ladder
;; there is NO arity limit.  Element access is `(tref t n)` where `n` is
;; a NON-NEGATIVE INTEGER LITERAL, checked against the tuple's arity at
;; compile time so an out-of-bounds reference is a type error, never a
;; runtime fault.  Tuples also destructure with a `(tuple p …)` pattern.

(require rackunit
         (for-syntax racket/base)
         "../main.rkt")

(define-syntax-rule (check-rackton-compile-error form ...)
  (check-exn
   exn:fail?
   (lambda ()
     (eval #'(rackton form ...)
           (variable-reference->namespace (#%variable-reference))))))

;; ----- construction + indexed access -------------------------------

(rackton
  (: t3 (Tuple Integer String Boolean))
  (define t3 (tuple 1 "a" #t))

  (: t3-i Integer)
  (define t3-i (tref t3 0))
  (: t3-s String)
  (define t3-s (tref t3 1))
  (: t3-b Boolean)
  (define t3-b (tref t3 2)))

(test-case "tref reads each element with its own type"
  (check-equal? t3-i 1)
  (check-equal? t3-s "a")
  (check-equal? t3-b #t))

;; ----- no arity limit ----------------------------------------------

(rackton
  (: big (Tuple Integer Integer Integer Integer Integer
                Integer Integer Integer Integer Integer))
  (define big (tuple 0 1 2 3 4 5 6 7 8 9))

  (: big-last Integer)
  (define big-last (tref big 9)))

(test-case "tuples have no arity limit"
  (check-equal? big-last 9))

;; ----- pattern matching --------------------------------------------

(rackton
  (: swap (-> (Tuple Integer String) (Tuple String Integer)))
  (define (swap t)
    (match t
      [(tuple a b) (tuple b a)]))

  (: sw (Tuple String Integer))
  (define sw (swap (tuple 7 "x")))
  (: sw0 String)  (define sw0 (tref sw 0))
  (: sw1 Integer) (define sw1 (tref sw 1)))

(test-case "tuple pattern destructures in match"
  (check-equal? sw0 "x")
  (check-equal? sw1 7))

;; ----- destructuring let -------------------------------------------

(rackton
  (: dl Integer)
  (define dl
    (let ([(tuple a b c) (tuple 10 20 30)])
      (+ a (+ b c)))))

(test-case "tuple pattern destructures in let"
  (check-equal? dl 60))

;; ----- compile-time bounds + literal checks ------------------------

(test-case "out-of-bounds tref is a compile error"
  (check-rackton-compile-error
   (: x Integer)
   (define x (tref (tuple 1 2) 5))))

(test-case "non-literal tref index is a compile error"
  (check-rackton-compile-error
   (: x Integer)
   (define x (let ([i 1]) (tref (tuple 1 2) i)))))

(test-case "tuple arity mismatch against an annotation is a type error"
  (check-rackton-compile-error
   (: x (Tuple Integer Integer))
   (define x (tuple 1 2 3))))

;; ----- Eq / Ord / Show (variadic, structural) ----------------------

(rackton
  (: eq-yes Boolean)  (define eq-yes  (== (tuple 1 2 3) (tuple 1 2 3)))
  (: eq-no  Boolean)  (define eq-no   (== (tuple 1 2 3) (tuple 1 9 3)))
  (: eq-nest Boolean) (define eq-nest (== (tuple (tuple 1 2) "x")
                                          (tuple (tuple 1 2) "x")))
  (: lt-yes Boolean)  (define lt-yes  (< (tuple 1 2) (tuple 1 3)))
  (: lt-no  Boolean)  (define lt-no   (< (tuple 2 0) (tuple 1 9)))
  (: sh String)       (define sh      (show (tuple 1 2 3))))

(test-case "tuple Eq is element-wise (and nests)"
  (check-true eq-yes)
  (check-false eq-no)
  (check-true eq-nest))

(test-case "tuple Ord is lexicographic"
  (check-true lt-yes)
  (check-false lt-no))

(test-case "tuple Show"
  (check-equal? sh "(1, 2, 3)"))

;; A polymorphic `(Eq a) =>` function applied at a tuple type forces the
;; constraint `(Eq (Tuple …))` to be discharged structurally.
(rackton
  (: same (All (a) ((Eq a) => (-> a (-> a Boolean)))))
  (define (same x y) (== x y))

  (: poly-eq Boolean)
  (define poly-eq (same (tuple 1 "a") (tuple 1 "a"))))

(test-case "Eq (Tuple …) discharges through a polymorphic constraint"
  (check-true poly-eq))

;; Eq is not free: an element type without Eq makes the tuple ineligible.
(test-case "tuple Eq requires each element to have Eq"
  (check-rackton-compile-error
   (: bad Boolean)
   (define bad (== (tuple (lambda (x) x) 1) (tuple (lambda (x) x) 1)))))

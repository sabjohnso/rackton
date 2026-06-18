#lang racket/base

;; The cyclic Comonad over NON-EMPTY arrays, (Array (+ n 1) a):
;;   extract  = element 0
;;   extend f = the stencil  i ↦ f (array-rotate i w)
;;   duplicate (derived) = the array of all rotations
;; Non-emptiness is enforced by the `(+ n 1)` index: extract on a
;; possibly-empty `(Array n a)` (which includes n=0) is a type error,
;; resolved by the unit-coefficient Nat solver — (+ n 1) ~ 0 has no Nat
;; solution.  (Functor (Array n) — all sizes — supplies the superclass.)

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
  (require rackton/control/comonad)
  (require rackton/data/array)

  (: e0 Integer)
  (define e0 (extract (array 10 20 30)))           ; element 0 → 10

  ;; cyclic "current + next neighbour" stencil
  (: nbrsum (-> (Array 3 Integer) Integer))
  (define (nbrsum w) (+ (aref w 0) (aref w 1)))

  (: ex (Array 3 Integer))
  (define ex (extend nbrsum (array 1 2 3)))
  (: ex0 Integer) (define ex0 (aref ex 0))         ; [1,2,3]→1+2=3
  (: ex1 Integer) (define ex1 (aref ex 1))         ; rot1 [2,3,1]→2+3=5
  (: ex2 Integer) (define ex2 (aref ex 2))         ; rot2 [3,1,2]→3+1=4

  ;; `duplicate` is NOT defined in the instance — it derives from the
  ;; protocol default (duplicate = extend id), which crosses the module
  ;; boundary from rackton/control/comonad.  It is the array of all cyclic
  ;; rotations: position i is the input rotated to bring element i front.
  (: dup (Array 3 (Array 3 Integer)))
  (define dup (duplicate (array 1 2 3)))
  (: d00 Integer) (define d00 (aref (aref dup 0) 0))   ; [1,2,3][0] = 1
  (: d10 Integer) (define d10 (aref (aref dup 1) 0))   ; rot1 [2,3,1][0] = 2
  (: d21 Integer) (define d21 (aref (aref dup 2) 1)))  ; rot2 [3,1,2][1] = 1

(test-case "array comonad: extract and a cyclic extend stencil"
  (check-equal? e0 10)
  (check-equal? ex0 3)
  (check-equal? ex1 5)
  (check-equal? ex2 4))

(test-case "array comonad: duplicate derives from the cross-module default"
  (check-equal? d00 1)
  (check-equal? d10 2)
  (check-equal? d21 1))

(test-case "extract on a possibly-empty array is a type error"
  (check-rackton-compile-error
   (require rackton/control/comonad)
   (require rackton/data/array)
   (: bad Integer)
   (define bad (extract (array)))))

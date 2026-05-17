#lang racket/base

;; Rackton — runtime side of the Phase-3 prelude.
;;
;; Defines the ADT structs that ship with the language, the per-method
;; dispatch tables for every prelude class, the generic-method functions,
;; and the built-in instance registrations for `Integer`, `Boolean`, and
;; `String`.  The compile-time side — class-info / instance-info entries
;; in the typing env — lives in env.rkt's `prelude-env`.
;;
;; Built-in Racket operators are imported under `rkt:` prefixes so they
;; don't clash with the Rackton method names we want to expose.

(require (rename-in racket/base
                    [+  rkt:+]  [-  rkt:-]  [*  rkt:*]
                    [<  rkt:<]  [>  rkt:>]  [=  rkt:=]
                    [<= rkt:<=] [>= rkt:>=])
         racket/format
         "adt.rkt"
         "dict.rkt")

(provide
 ;; ADTs (constructors usable as expressions and as match patterns)
 None Some Nil Cons MkPair Ok Err MkUnit

 ;; Class methods
 +  -  *
 ==  /=
 <  >  <=  >=
 show

 ;; Combinators
 id compose flip const)

;; ----- ADTs -------------------------------------------------------

(define-data-ctor None 0)
(define-data-ctor Some 1)

(define-data-ctor Nil  0)
(define-data-ctor Cons 2)

(define-data-ctor MkPair 2)

(define-data-ctor Ok  1)
(define-data-ctor Err 1)

(define-data-ctor MkUnit 0)

;; ----- Class dispatch tables -------------------------------------

(define $dispatch:+  (make-hasheq))  (define-class-method +  $dispatch:+)
(define $dispatch:-  (make-hasheq))  (define-class-method -  $dispatch:-)
(define $dispatch:*  (make-hasheq))  (define-class-method *  $dispatch:*)
(define $dispatch:== (make-hasheq))  (define-class-method == $dispatch:==)
(define $dispatch:/= (make-hasheq))  (define-class-method /= $dispatch:/=)
(define $dispatch:<  (make-hasheq))  (define-class-method <  $dispatch:<)
(define $dispatch:>  (make-hasheq))  (define-class-method >  $dispatch:>)
(define $dispatch:<= (make-hasheq))  (define-class-method <= $dispatch:<=)
(define $dispatch:>= (make-hasheq))  (define-class-method >= $dispatch:>=)
(define $dispatch:show (make-hasheq))(define-class-method show $dispatch:show)

;; ----- Num Integer ------------------------------------------------

(register-instance-method! $dispatch:+  'Integer (lambda (x y) (rkt:+  x y)))
(register-instance-method! $dispatch:-  'Integer (lambda (x y) (rkt:-  x y)))
(register-instance-method! $dispatch:*  'Integer (lambda (x y) (rkt:*  x y)))

;; ----- Eq instances ----------------------------------------------

(register-instance-method! $dispatch:== 'Integer (lambda (x y) (rkt:= x y)))
(register-instance-method! $dispatch:== 'Boolean (lambda (x y) (if x y (not y))))
(register-instance-method! $dispatch:== 'String  (lambda (x y) (string=? x y)))

(register-instance-method! $dispatch:/= 'Integer (lambda (x y) (not (rkt:= x y))))
(register-instance-method! $dispatch:/= 'Boolean (lambda (x y) (not (if x y (not y)))))
(register-instance-method! $dispatch:/= 'String  (lambda (x y) (not (string=? x y))))

;; ----- Ord Integer -----------------------------------------------

(register-instance-method! $dispatch:<  'Integer (lambda (x y) (rkt:<  x y)))
(register-instance-method! $dispatch:>  'Integer (lambda (x y) (rkt:>  x y)))
(register-instance-method! $dispatch:<= 'Integer (lambda (x y) (rkt:<= x y)))
(register-instance-method! $dispatch:>= 'Integer (lambda (x y) (rkt:>= x y)))

;; ----- Show instances --------------------------------------------

(register-instance-method! $dispatch:show 'Integer
                           (lambda (x) (number->string x)))
(register-instance-method! $dispatch:show 'Boolean
                           (lambda (x) (if x "True" "False")))
(register-instance-method! $dispatch:show 'String
                           (lambda (x) (~a "\"" x "\"")))

;; ----- Combinators ----------------------------------------------

(define (id x) x)
(define (compose f g) (lambda (x) (f (g x))))
(define (flip f) (lambda (x y) (f y x)))
(define (const x) (lambda (_y) x))

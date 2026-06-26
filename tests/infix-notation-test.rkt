#lang racket/base

;; Infix notation.  A quasiquoted identifier in operator position turns an
;; application into infix, mirroring spork's `#%app` notation but realised
;; in Rackton's surface parser:
;;
;;   (a `op b)            => (op a b)
;;   (a `op b `op c ...)  => (op a b c ...)   (operator must be homogeneous)
;;   (`op b)              => (lambda (x) (op x b))   right section
;;   (a `op)              => (lambda (x) (op a x))   left section
;;
;; Because Rackton is curried with over-application, a homogeneous chain of
;; N terms is a single application of the operator to N arguments, and a
;; section is an ordinary one-argument lambda.

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
  ;; ----- binary infix with prelude operators -----
  (define sum    (1 `+ 2))
  (define lt     (1 `< 2))

  ;; ----- a binary user-defined function as the operator -----
  (define (add x y) (+ x y))
  (define added  (3 `add 4))

  ;; ----- a higher-order function as the operator -----
  (define (inc x)    (+ x 1))
  (define (double x) (* x 2))
  ;; (inc `compose double) is (compose inc double) = a function; apply it.
  (define composed ((inc `compose double) 5))     ; inc(double 5) = 11

  ;; ----- homogeneous chains fold into a single application -----
  ;; The operator must accept that many arguments; fma is curried ternary.
  (: fma (-> Integer (-> Integer (-> Integer Integer))))
  (define (fma a b c) (+ (* a b) c))
  (define fma3     (2 `fma 3 `fma 4))             ; (fma 2 3 4) = 10
  (define fma-step ((2 `fma 3) 4))                ; ((fma 2 3) 4) = 10

  ;; ----- right section: operator on the left, argument on the right -----
  (define less-than-three? (`< 3))                ; (lambda (x) (< x 3))
  (define lt3-yes (less-than-three? 2))
  (define lt3-no  (less-than-three? 4))

  (define subtract-three-from (`- 3))             ; (lambda (x) (- x 3))
  (define sub3 (subtract-three-from 5))           ; (- 5 3) = 2

  ;; ----- left section: argument on the left, operator on the right -----
  (define three-is-less-than? (3 `<))             ; (lambda (x) (< 3 x))
  (define lt-from3-yes (three-is-less-than? 4))
  (define lt-from3-no  (three-is-less-than? 2)))

;; ===== basic infix ==================================================

(test-case "binary infix applies the operator to both terms"
  (check-equal? sum 3)
  (check-true lt)
  (check-equal? added 7))

(test-case "a higher-order function works as an infix operator"
  (check-equal? composed 11))

;; ===== homogeneous chains ===========================================

(test-case "a homogeneous chain folds into one application"
  (check-equal? fma3 10)
  (check-equal? fma-step 10))

;; ===== sections (partial application) ===============================

(test-case "a right section curries the operator's first argument"
  (check-true  lt3-yes)
  (check-false lt3-no)
  (check-equal? sub3 2))

(test-case "a left section curries the operator's second argument"
  (check-true  lt-from3-yes)
  (check-false lt-from3-no))

;; ===== rejected forms ===============================================

(test-case "inhomogeneous operators are rejected"
  (check-rackton-compile-error
   (define bad (1 `+ 2 `* 3))))

(test-case "a non-identifier operator is not infix (and 1 is not a function)"
  (check-rackton-compile-error
   (define bad (1 `(lambda (x y) (+ x y)) 2))))

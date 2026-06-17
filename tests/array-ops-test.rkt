#lang racket/base

;; Concrete-size array operations.
;;
;; `array-take` / `array-drop` / `array-split-at` are literal-position
;; forms over an array of CONCRETE size: the size must be a known Nat so
;; the result size (k, n-k) is computed and the split point is bounds-
;; checked at compile time.  `array-map` / `array-fold` are ordinary
;; size-polymorphic functions (map preserves the size; fold consumes it).
;; (A size-polymorphic take/drop/split would need the deferred type-level
;; subtraction / symbolic solving — see PLAN.org.)

(require rackunit
         (for-syntax racket/base)
         "../main.rkt")

(define-syntax-rule (check-rackton-compile-error form ...)
  (check-exn
   exn:fail?
   (lambda ()
     (eval #'(rackton form ...)
           (variable-reference->namespace (#%variable-reference))))))

;; ----- take / drop -------------------------------------------------

(rackton
  (: tk (Array 2 Integer))
  (define tk (array-take 2 (array 10 20 30 40)))
  (: tk0 Integer) (define tk0 (aref tk 0))
  (: tk1 Integer) (define tk1 (aref tk 1))

  (: dp (Array 2 Integer))
  (define dp (array-drop 2 (array 10 20 30 40)))
  (: dp0 Integer) (define dp0 (aref dp 0))
  (: dp1 Integer) (define dp1 (aref dp 1)))

(test-case "array-take / array-drop slice with computed sizes"
  (check-equal? tk0 10) (check-equal? tk1 20)
  (check-equal? dp0 30) (check-equal? dp1 40))

;; ----- split-at ----------------------------------------------------

(rackton
  (: parts (Pair (Array 1 Integer) (Array 2 Integer)))
  (define parts (array-split-at 1 (array 10 20 30)))
  (: lo0 Integer) (define lo0 (aref (fst parts) 0))
  (: hi0 Integer) (define hi0 (aref (snd parts) 0))
  (: hi1 Integer) (define hi1 (aref (snd parts) 1)))

(test-case "array-split-at splits into a Pair of arrays summing to n"
  (check-equal? lo0 10)
  (check-equal? hi0 20)
  (check-equal? hi1 30))

;; ----- map (size-preserving, polymorphic) --------------------------

(rackton
  (: sq (Array 3 Integer))
  (define sq (array-map (lambda (x) (* x x)) (array 1 2 3)))
  (: sq2 Integer) (define sq2 (aref sq 2))

  ;; element type may change
  (: strs (Array 2 String))
  (define strs (array-map show (array 7 8)))
  (: s0 String) (define s0 (aref strs 0))

  ;; works at a polymorphic size — no concrete size needed
  (: double-all (All (n) (-> (Array n Integer) (Array n Integer))))
  (define (double-all xs) (array-map (lambda (x) (* x 2)) xs))
  (: d1 Integer) (define d1 (aref (double-all (array 5 6 7)) 1)))

(test-case "array-map preserves size and is size-polymorphic"
  (check-equal? sq2 9)
  (check-equal? s0 "7")
  (check-equal? d1 12))

;; ----- fold --------------------------------------------------------

(rackton
  (: total Integer)
  (define total (array-fold (lambda (acc x) (+ acc x)) 0 (array 1 2 3 4))))

(test-case "array-fold reduces left-to-right"
  (check-equal? total 10))

;; ----- compile-time checks ----------------------------------------

(test-case "taking more than the array holds is a compile error"
  (check-rackton-compile-error
   (: x (Array 5 Integer))
   (define x (array-take 5 (array 1 2 3)))))

(test-case "a non-literal split point is a compile error"
  (check-rackton-compile-error
   (: x (Pair (Array 1 Integer) (Array 1 Integer)))
   (define x (let ([k 1]) (array-split-at k (array 1 2))))))

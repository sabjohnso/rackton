#lang racket/base

;; `(list …)` patterns in `match`.
;;
;; A list pattern desugars (in the parser) to Cons/Nil constructor
;; patterns, the dual of the `(list …)` expression literal:
;;   (list a b c)   → (Cons a (Cons b (Cons c Nil)))   ; exactly 3 elements
;;   (list a b ...) → (Cons a b)                        ; a = head, b = rest
;;   (list)         → Nil                               ; empty
;; The trailing `<var> ...` binds the remaining elements as one list.
;;
;; Rackton list values are Cons/Nil structs, not Racket lists, so all the
;; match calls run *inside* the rackton block; their scalar / list results
;; are compared at the Racket level (transparent structs compare by value).

(require rackunit
         (for-syntax racket/base)
         "../main.rkt")

;; Asserts that a Rackton block fails to compile.  Takes the forms as a
;; quoted datum (not a syntax template) so a literal `...` inside a pattern
;; stays an ordinary symbol instead of colliding with template ellipsis.
(define (assert-compile-error forms)
  (check-exn
   exn:fail?
   (lambda ()
     (eval (cons 'rackton forms)
           (variable-reference->namespace (#%variable-reference))))))

(rackton
  ;; fixed arity
  (define (sum3 xs)
    (match xs
      [(list a b c) (+ a (+ b c))]
      [_            0]))

  ;; empty vs non-empty
  (define (classify xs)
    (match xs
      [(list)   "empty"]
      [(list _) "one"]
      [_        "many"]))

  ;; head / tail binder
  (define (rest-of xs)
    (match xs
      [(list a b ...) b]
      [(list)         Nil]))

  ;; head with ignored tail
  (define (head-or xs d)
    (match xs
      [(list a _ ...) a]
      [(list)         d]))

  ;; literal sub-pattern in a list pattern
  (define (mid xs)
    (match xs
      [(list 1 x 3) x]
      [_            0]))

  ;; nested list patterns
  (define (pair-sum xs)
    (match xs
      [(list (list a) (list b)) (+ a b)]
      [_                        0]))

  ;; exhaustive: (Cons a b) + Nil covers every list
  (define (safe-head xs d)
    (match xs
      [(list a b ...) a]
      [(list)         d]))

  (define nil-int (ann Nil (List Integer)))

  ;; results, computed in Rackton, checked at the Racket level
  (define r-sum3-3 (sum3 (list 1 2 3)))
  (define r-sum3-2 (sum3 (list 1 2)))
  (define r-sum3-4 (sum3 (list 1 2 3 4)))

  (define r-desc-0 (classify nil-int))
  (define r-desc-1 (classify (list 9)))
  (define r-desc-3 (classify (list 9 8 7)))

  (define r-rest-3 (rest-of (list 1 2 3)))
  (define r-rest-1 (rest-of (list 5)))
  (define r-rest-0 (rest-of nil-int))

  (define r-head-3 (head-or (list 7 8 9) 0))
  (define r-head-0 (head-or nil-int 0))

  (define r-mid-ok (mid (list 1 2 3)))
  (define r-mid-no (mid (list 1 2 4)))

  (define r-pair-2 (pair-sum (list (list 1) (list 2))))
  (define r-pair-3 (pair-sum (list (list 1) (list 2) (list 3))))

  (define r-safe-3 (safe-head (list 4 5 6) 0))
  (define r-safe-0 (safe-head nil-int 0))

  ;; reference list values
  (define ref-23 (list 2 3))
  (define ref-nil nil-int))

(test-case "fixed-arity list pattern"
  (check-equal? r-sum3-3 6)
  (check-equal? r-sum3-2 0)    ;; too short
  (check-equal? r-sum3-4 0))   ;; too long

(test-case "empty vs singleton vs many"
  (check-equal? r-desc-0 "empty")
  (check-equal? r-desc-1 "one")
  (check-equal? r-desc-3 "many"))

(test-case "head/tail binder captures the rest as a list"
  (check-equal? r-rest-3 ref-23)
  (check-equal? r-rest-1 ref-nil)    ;; empty tail
  (check-equal? r-rest-0 ref-nil))

(test-case "wildcard tail"
  (check-equal? r-head-3 7)
  (check-equal? r-head-0 0))

(test-case "literal sub-pattern"
  (check-equal? r-mid-ok 2)
  (check-equal? r-mid-no 0))   ;; last element isn't 3

(test-case "nested list patterns"
  (check-equal? r-pair-2 3)
  (check-equal? r-pair-3 0))

(test-case "head/tail + empty is exhaustive"
  (check-equal? r-safe-3 4)
  (check-equal? r-safe-0 0))

;; ----- exhaustiveness -----

(test-case "head/tail without the empty case is non-exhaustive"
  (assert-compile-error '((define (f xs) (match xs [(list a b ...) a])))))

;; ----- ellipsis misuse -----

(test-case "complex pattern before ... is rejected"
  (assert-compile-error '((define (f xs) (match xs [(list a (Cons x y) ...) x])))))

(test-case "... not in final position is rejected"
  (assert-compile-error '((define (f xs) (match xs [(list a ... b) a])))))

(test-case "lone ... is rejected"
  (assert-compile-error '((define (f xs) (match xs [(list ...) 0])))))

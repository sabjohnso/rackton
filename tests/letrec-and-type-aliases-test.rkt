#lang racket/base

;; End-to-end: letrec, type aliases, polymorphic recursion,
;; multi-(rackton …)-block support, panic, and "did you mean?"
;; suggestions in unbound-identifier errors.

(require rackunit
         "../main.rkt")

;; ----- letrec: mutual recursion ----------------------------------

(rackton
  (define mutual-result
    (letrec ([even? (lambda (n) (if (== n 0) #t (odd? (- n 1))))]
             [odd?  (lambda (n) (if (== n 0) #f (even? (- n 1))))])
      (even? 8))))

(test-case "letrec mutual recursion"
  (check-true mutual-result))

;; ----- type alias: parametric Endo -------------------------------

(rackton
  (define-alias (Endo a) (-> a a))

  (: bump (Endo Integer))
  (define (bump n) (+ n 1)))

(test-case "parametric type alias"
  (check-equal? (bump 41) 42))

;; ----- polymorphic recursion -------------------------------------
;; A function whose declared scheme is polymorphic and which
;; recursively calls itself at a different instantiation.

(rackton
  (: const-int (-> a Integer))
  (define (const-int x)
    (if (== 0 0)
        99
        ;; Recurse at type `Integer` even though the outer call is
        ;; polymorphic.  Without polymorphic recursion this would be
        ;; rejected, because the recursive `const-int 5` would conflict
        ;; with the skolemised parameter type.
        (const-int 5))))

(test-case "polymorphic recursion: declared scheme used for self-calls"
  (check-equal? (const-int "anything") 99))

;; ----- multi-block: TWO (rackton ...) calls in one module --------

(rackton (define a-val 100))
(rackton (define b-val 200))

(test-case "two rackton blocks coexist"
  (check-equal? a-val 100)
  (check-equal? b-val 200))

;; ----- panic: typed at bottom ------------------------------------

(rackton
  (: pick-positive (-> Integer Integer))
  (define (pick-positive n)
    (if (< n 0)
        (panic "negative not allowed")
        n)))

(test-case "panic raises at runtime"
  (check-equal? (pick-positive 7) 7)
  (check-exn exn:fail? (lambda () (pick-positive -3))))

;; ----- did-you-mean: unbound identifier with suggestion ----------

(test-case "unbound-identifier error suggests a near-match"
  (define msg
    (with-handlers ([exn:fail? (lambda (e) (exn-message e))])
      (eval #'(rackton (define x (legnth (Cons 1 Nil))))
            (variable-reference->namespace (#%variable-reference)))))
  (check-regexp-match #rx"did you mean `length`?" msg))

#lang racket/base

;; Pattern guards, destructuring let, let*.

(require rackunit
         (for-syntax racket/base)
         "../main.rkt")

(rackton
  ;; ----- pattern guards on Maybe ----------------------------
  (: classify (-> (Maybe Integer) String))
  (define (classify m)
    (match m
      [(Some x) #:when (> x 0) "positive"]
      [(Some x) #:when (< x 0) "negative"]
      [(Some _)                 "zero"]
      [(None)                   "missing"]))

  (: classify-five  String)
  (define classify-five (classify (Some 5)))

  (: classify-neg  String)
  (define classify-neg (classify (Some -3)))

  (: classify-zero String)
  (define classify-zero (classify (Some 0)))

  (: classify-none String)
  (define classify-none (classify None))

  ;; ----- pattern guards on List, keeping positives only -----
  (: keep-pos (-> (List Integer) (List Integer)))
  (define (keep-pos xs)
    (match xs
      [(Nil) Nil]
      [(Cons h rest) #:when (> h 0) (Cons h (keep-pos rest))]
      [(Cons _ rest)                (keep-pos rest)]))

  (: filtered (List Integer))
  (define filtered (keep-pos (Cons 1 (Cons -2 (Cons 3 (Cons -4 (Cons 5 Nil)))))))

  ;; ----- let destructuring two values ----------------------
  (: pair-sum Integer)
  (define pair-sum
    (let ([(Pair a b) (Pair 7 35)]
          [(Cons h _)   (Cons 100 Nil)])
      (+ a (+ b h))))

  ;; ----- let* building intermediate bindings ----------------
  (: scaled-sum (-> Integer (-> Integer Integer)))
  (define (scaled-sum x y)
    (let* ([sum     (+ x y)]
           [doubled (* 2 sum)])
      (+ sum doubled))))

;; ---------- assertions -------------------------------------------

(test-case "pattern guard: positive"
  (check-equal? classify-five "positive"))

(test-case "pattern guard: negative"
  (check-equal? classify-neg "negative"))

(test-case "pattern guard: zero falls through"
  (check-equal? classify-zero "zero"))

(test-case "pattern guard: None"
  (check-equal? classify-none "missing"))

(test-case "pattern guard on List: keep-pos"
  (check-equal? filtered (Cons 1 (Cons 3 (Cons 5 Nil)))))

(test-case "let destructures multiple values"
  ;; a=7, b=35, h=100 → 7 + 35 + 100 = 142
  (check-equal? pair-sum 142))

(test-case "let* binds intermediates in sequence"
  ;; sum = 10, doubled = 20, result = 30
  (check-equal? (scaled-sum 3 7) 30))

;; ----- exhaustiveness rejection for a guard-only match ----

(define-syntax-rule (check-rackton-compile-error form ...)
  (check-exn
   exn:fail?
   (lambda ()
     (eval #'(rackton form ...)
           (variable-reference->namespace (#%variable-reference))))))

(test-case "guarded sole clause does not satisfy exhaustiveness"
  (check-rackton-compile-error
   (define x (match (Some 5)
               [(Some _) #:when #t 1]))))

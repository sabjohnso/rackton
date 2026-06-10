#lang racket/base

;; Property-based laws for the Environment monad (private/monad/ctx.rkt).
;;
;; A context computation is a function  ctx -> a.  Two are equal when they
;; agree on a fixed set of sample contexts; the laws are checked
;; extensionally against that notion.
;;
;; Laws checked:
;;   monad    — left identity, right identity, associativity
;;   context  — local/ask (= asks), local of a pure value is a no-op,
;;              ask/ask (reading the context twice = once)
;;   notation — let/ctx desugars to bind; let/ctx+ is applicative (pure
;;              body); begin/ctx returns the last computation.
;;
;; rackcheck stays inside `module+ test` so it remains a build-time dep.

(module+ test
  (require rackunit
           rackcheck
           "../private/monad/ctx.rkt")

  ;; ----- extensional equality of context computations -----
  (define sample-ctxs '(-7 -1 0 1 5 42))
  (define (ctx=? m1 m2)
    (for/and ([c (in-list sample-ctxs)])
      (equal? (run-ctx m1 c) (run-ctx m2 c))))

  ;; ----- generators (ctx = int, a = int) -----
  (define gen-int (gen:integer-in -50 50))
  (define gen-comp
    (gen:choice
     (gen:map gen-int ctx-return)
     (gen:const ask)
     (gen:map gen-int (lambda (k) (asks (lambda (c) (+ c k)))))
     (gen:map gen-int (lambda (k) (local (lambda (c) (+ c k)) ask)))))
  (define gen-k  (gen:map gen-int (lambda (n) (lambda (a) (ctx-return (+ a n))))))
  (define gen-k2 (gen:map gen-int (lambda (n) (lambda (a) (asks (lambda (c) (+ c a n)))))))

  ;; ----- monad laws -----
  (check-property
   (property ctx-left-identity ([a gen-int] [k gen-k])
     (ctx=? (ctx-bind (ctx-return a) k) (k a))))

  (check-property
   (property ctx-right-identity ([m gen-comp])
     (ctx=? (ctx-bind m ctx-return) m)))

  (check-property
   (property ctx-associativity ([m gen-comp] [f gen-k] [g gen-k2])
     (ctx=? (ctx-bind (ctx-bind m f) g)
            (ctx-bind m (lambda (x) (ctx-bind (f x) g))))))

  ;; ----- context-specific laws -----
  ;; local f ask  =  asks f   (running ask under a modified context reads it)
  (check-property
   (property ctx-local-ask ([k gen-int])
     (ctx=? (local (lambda (c) (+ c k)) ask)
            (asks (lambda (c) (+ c k))))))

  ;; local on a pure value is a no-op
  (check-property
   (property ctx-local-pure ([x gen-int] [k gen-int])
     (ctx=? (local (lambda (c) (+ c k)) (ctx-return x))
            (ctx-return x))))

  ;; ask/ask — reading the context twice is reading it once
  (check-true
   (ctx=? (ctx-bind ask (lambda (c)
            (ctx-bind ask (lambda (c2) (ctx-return (cons c c2))))))
          (ctx-bind ask (lambda (c) (ctx-return (cons c c))))))

  ;; ----- notation -----
  ;; let/ctx is bind
  (check-property
   (property let-ctx-is-bind ([a gen-int] [k gen-k])
     (ctx=? (let/ctx ([x (ctx-return a)]) (k x))
            (ctx-bind (ctx-return a) k))))

  ;; let/ctx implicit body: bind x, then a final context computation
  (check-true
   (= 20 (run-ctx (let/ctx ([x ask]) (asks (lambda (c) (+ x c)))) 10)))

  ;; let/ctx+ — applicative: independent reads, PURE body (no return)
  (check-true
   (= 21 (run-ctx (let/ctx+ ([x ask] [y (asks add1)]) (+ x y)) 10)))

  ;; begin/ctx returns the last computation's result
  (check-true
   (= 6 (run-ctx (begin/ctx (ctx-return 1) (asks add1)) 5))))

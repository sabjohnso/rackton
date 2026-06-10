#lang racket/base

;; Property-based laws for the State monad (private/monad/state.rkt).
;;
;; A State computation is a function  st -> (values a st).  Two are equal
;; when they agree (result and final state) on a fixed set of sample initial
;; states; the laws are checked extensionally against that notion.
;;
;; Laws checked:
;;   monad      — left identity, right identity, associativity
;;   State      — get/get, put/put, put/get, get/put
;;   notation   — let/state desugars to bind; let/state+ is applicative
;;                (independent right-hand sides, pure body); begin/state
;;                sequences for effect and returns the last result.
;;
;; rackcheck stays inside `module+ test` so it remains a build-time dep
;; (raco setup --check-pkg-deps would flag a top-level rackcheck require).

(module+ test
  (require rackunit
           rackcheck
           "../private/monad/state.rkt")

  ;; ----- extensional equality of State computations -----
  (define sample-states '(-7 -1 0 1 5 42))
  (define (state=? m1 m2)
    (for/and ([s (in-list sample-states)])
      (let-values ([(a1 s1) (run-state m1 s)]
                   [(a2 s2) (run-state m2 s)])
        (and (equal? a1 a2) (equal? s1 s2)))))

  ;; ----- generators (st = int, a = int) -----
  (define gen-int (gen:integer-in -50 50))
  ;; A representative State computation whose RESULT is an int, so the
  ;; arithmetic continuations below are well-typed.  (put/modify return void;
  ;; their effect is still exercised here through int-returning forms.)
  (define gen-comp
    (gen:choice
     (gen:map gen-int state-return)
     (gen:const get)
     (gen:map gen-int (lambda (k) (begin/state (put k) (state-return k))))
     (gen:map gen-int (lambda (k) (begin/state (modify (lambda (s) (+ s k))) get)))))
  ;; continuations int -> State
  (define gen-k  (gen:map gen-int (lambda (n) (lambda (a) (state-return (+ a n))))))
  (define gen-k2 (gen:map gen-int (lambda (n) (lambda (a) (modify (lambda (s) (+ s a n)))))))

  ;; ----- monad laws -----
  (check-property
   (property state-left-identity ([a gen-int] [k gen-k])
     (state=? (state-bind (state-return a) k) (k a))))

  (check-property
   (property state-right-identity ([m gen-comp])
     (state=? (state-bind m state-return) m)))

  (check-property
   (property state-associativity ([m gen-comp] [f gen-k] [g gen-k2])
     (state=? (state-bind (state-bind m f) g)
              (state-bind m (lambda (x) (state-bind (f x) g))))))

  ;; ----- State-specific laws -----
  ;; get/get — reading twice is reading once
  (check-true
   (state=? (state-bind get (lambda (s)
              (state-bind get (lambda (s2) (state-return (cons s s2))))))
            (state-bind get (lambda (s) (state-return (cons s s))))))

  ;; put/put — the last put wins
  (check-property
   (property state-put-put ([s1 gen-int] [s2 gen-int])
     (state=? (begin/state (put s1) (put s2)) (put s2))))

  ;; put/get — after (put s), get yields s
  (check-property
   (property state-put-get ([s gen-int])
     (state=? (state-bind (put s) (lambda (_) get))
              (state-bind (put s) (lambda (_) (state-return s))))))

  ;; get/put — putting back what you got is a no-op
  (check-true
   (state=? (state-bind get put) (state-return (void))))

  ;; ----- notation -----
  ;; let/state is bind, with an implicit-begin/state body (last form returned)
  (check-property
   (property let-state-is-bind ([a gen-int] [k gen-k])
     (state=? (let/state ([x (state-return a)]) (k x))
              (state-bind (state-return a) k))))

  ;; let/state threads effects then returns the last form
  (check-true
   (let-values ([(v s) (run-state
                        (let/state ([x get])
                          (modify add1)
                          (state-return (* x 2)))
                        10)])
     (and (= v 20) (= s 11))))

  ;; let/state+ — applicative: independent RHS, PURE body (no return), the
  ;; map supplies the wrapping; state threads left-to-right.
  (check-true
   (let-values ([(v s) (run-state
                        (let/state+ ([x get] [y (gets add1)]) (+ x y))
                        10)])
     (and (= v 21) (= s 10))))

  ;; begin/state returns the last computation's result
  (check-true
   (let-values ([(v s) (run-state
                        (begin/state (put 3) (modify add1) (gets (lambda (x) (* x 10))))
                        99)])
     (and (= v 40) (= s 4)))))

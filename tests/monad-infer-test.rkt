#lang racket/base

;; Property-based laws for the combined Infer monad
;; (private/monad/infer.rkt) — Environment over State, the engine's working
;; monad, with the auto-lifting binder design:
;;   Infer a = (ctx st) -> (values a st)
;;   let/state binds raw State computations (lifted via state->infer)
;;   let/ctx   binds raw Environment computations (lifted via ctx->infer)
;;   begin/infer sequences Infer computations
;;
;; Two Infer computations are equal when they agree (result + final state)
;; on a grid of sample (ctx, st) pairs.
;;
;; Laws checked:
;;   monad   — left identity, right identity, associativity
;;   lifting — state->infer ignores ctx and runs on st; ctx->infer reads ctx
;;             and leaves st untouched
;;   binders — let/state / let/ctx auto-lift; they mix in one body; let/state+
;;             is applicative; begin/infer sequences; fresh threads a counter
;;
;; rackcheck stays in `module+ test` so it remains a build-time dep.

(module+ test
  (require rackunit
           rackcheck
           racket/list
           "../private/monad/infer.rkt"
           (only-in "../private/monad/state.rkt" state-bind))

  ;; ----- extensional equality over a grid of (ctx, st) -----
  (define sample-ctxs '(-3 0 7))
  (define sample-sts  '(-5 0 9))
  (define (infer=? m1 m2)
    (for*/and ([c (in-list sample-ctxs)] [s (in-list sample-sts)])
      (let-values ([(a1 s1) (run-infer m1 c s)]
                   [(a2 s2) (run-infer m2 c s)])
        (and (equal? a1 a2) (equal? s1 s2)))))

  ;; ----- generators (ctx = int, st = int, a = int) -----
  (define gen-int (gen:integer-in -50 50))
  (define gen-comp
    (gen:choice
     (gen:map gen-int infer-return)
     (gen:const (state->infer get))
     (gen:const (ctx->infer ask))
     (gen:map gen-int (lambda (k) (state->infer (gets (lambda (s) (+ s k))))))
     (gen:map gen-int (lambda (k) (ctx->infer (asks (lambda (c) (+ c k))))))
     ;; one that actually threads state (returns the new state)
     (gen:map gen-int (lambda (k)
                        (state->infer
                         (state-bind (modify (lambda (s) (+ s k)))
                                     (lambda (_) get)))))))
  (define gen-k  (gen:map gen-int (lambda (n) (lambda (a) (infer-return (+ a n))))))
  (define gen-k2 (gen:map gen-int (lambda (n) (lambda (a) (state->infer (gets (lambda (s) (+ s a n))))))))

  ;; ----- monad laws -----
  (check-property
   (property infer-left-identity ([a gen-int] [k gen-k])
     (infer=? (infer-bind (infer-return a) k) (k a))))

  (check-property
   (property infer-right-identity ([m gen-comp])
     (infer=? (infer-bind m infer-return) m)))

  (check-property
   (property infer-associativity ([m gen-comp] [f gen-k] [g gen-k2])
     (infer=? (infer-bind (infer-bind m f) g)
              (infer-bind m (lambda (x) (infer-bind (f x) g))))))

  ;; ----- lifting -----
  ;; state->infer ignores ctx, runs on st (here: replaces it)
  (check-true
   (let-values ([(a s) (run-infer (state->infer (put 5)) 999 0)])
     (and (void? a) (= s 5))))
  ;; ctx->infer reads ctx, leaves st untouched
  (check-true
   (let-values ([(a s) (run-infer (ctx->infer ask) 7 99)])
     (and (= a 7) (= s 99))))

  ;; ----- binders -----
  ;; let/state auto-lifts a State op; body is Infer
  (check-true
   (let-values ([(v s) (run-infer (let/state ([x get]) (infer-return (* x 2))) 0 10)])
     (and (= v 20) (= s 10))))

  ;; let/state and let/ctx mix in one body (read ctx while threading state)
  (check-true
   (let-values ([(v s) (run-infer
                        (let/state ([t (gets add1)])
                          (let/ctx ([c ask])
                            (infer-return (+ t c))))
                        5 10)])
     (and (= v 16) (= s 10))))

  ;; let/state+ — applicative: independent lifted reads, PURE body (no return)
  (check-true
   (let-values ([(v s) (run-infer (let/state+ ([x get] [y (gets add1)]) (+ x y)) 0 10)])
     (and (= v 21) (= s 10))))

  ;; let/ctx+ — applicative over the environment
  (check-true
   (= 21 (let-values ([(v _s) (run-infer (let/ctx+ ([x ask] [y (asks add1)]) (+ x y)) 10 0)]) v)))

  ;; begin/infer sequences Infer computations, returning the last
  (check-true
   (let-values ([(v s) (run-infer
                        (begin/infer (state->infer (put 3)) (state->infer get))
                        0 99)])
     (and (= v 3) (= s 3))))

  ;; fresh threads the counter and yields distinct names
  (check-true
   (let-values ([(names st)
                 (run-infer (let/state ([a fresh] [b fresh] [c fresh])
                              (infer-return (list a b c)))
                            0 (make-infer-state))])
     (and (= 3 (length names))
          (= 3 (length (remove-duplicates names)))
          (= 3 (infer-state-fresh-counter st))))))

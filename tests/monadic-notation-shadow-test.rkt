#lang racket/base

;; The monadic / applicative notations `do`, `let&`, `let%`, `let+` must
;; bind to the type-class METHODS they mean (`flatmap`, `fmap`, `product`),
;; not to whatever those names happen to resolve to lexically.  A module
;; that locally shadows `flatmap` / `fmap` / `product` must still get the
;; method dispatch, so each submodule below shadows the relevant name with
;; a type-INCOMPATIBLE binding and checks the notation still runs.
;;
;; Each scenario is its own `rackton` submodule so the shadowing
;; definitions do not collide at one module level.

(require rackunit)

(module do-blk rackton
  (provide do-val)
  (: flatmap (-> a b Boolean))
  (define (flatmap x f) #t)
  (: r (Maybe Integer))
  (define r (do [x <- (Some 1)]
                [y <- (Some 2)]
                (pure (+ x y))))
  (: do-val Integer)
  (define do-val (match r [(Some v) v] [(None) 0])))

(module let&-blk rackton
  (provide v)
  (: flatmap (-> a b Boolean))
  (define (flatmap x f) #t)
  (: r (Maybe Integer))
  (define r (let& ([x (Some 10)]
                   [y (Some 20)])
              (pure (+ x y))))
  (: v Integer)
  (define v (match r [(Some n) n] [(None) 0])))

(module let%-blk rackton
  (provide v)
  (: flatmap (-> a b Boolean))
  (define (flatmap x f) #t)
  (: product (-> a b Boolean))
  (define (product x y) #t)
  (: r (Maybe Integer))
  (define r (let% ([x (Some 3)]
                   [y (Some 4)])
              (pure (+ x y))))
  (: v Integer)
  (define v (match r [(Some n) n] [(None) 0])))

(module let+-blk rackton
  (provide v)
  (: fmap (-> a b Boolean))
  (define (fmap f x) #t)
  (: product (-> a b Boolean))
  (define (product x y) #t)
  (: r (Maybe Integer))
  (define r (let+ ([x (Some 5)]
                   [y (Some 6)])
              (+ x y)))
  (: v Integer)
  (define v (match r [(Some n) n] [(None) 0])))

;; Polymorphic in the monad: `flatmap` cannot monomorphize, so codegen
;; emits the runtime dispatcher — this is what exercises the codegen
;; shadow-proofing (the concrete cases above monomorphize to an impl name
;; that is shadow-immune anyway).
(module poly-blk rackton
  (provide v)
  (: flatmap (-> a b Boolean))
  (define (flatmap x f) #t)
  ;; chain : Monad m => (m a) -> (a -> m b) -> m b  (inferred)
  (define (chain mx k) (do [x <- mx] (k x)))
  (: r (Maybe Integer))
  (define r (chain (Some 5) (lambda (x) (Some (+ x 100)))))
  (: v Integer)
  (define v (match r [(Some n) n] [(None) 0])))

(require (prefix-in do: (submod "." do-blk))
         (prefix-in la: (submod "." let&-blk))
         (prefix-in lp: (submod "." let%-blk))
         (prefix-in lplus: (submod "." let+-blk))
         (prefix-in poly: (submod "." poly-blk)))

(test-case "do binds to the Monad flatmap method under a flatmap shadow"
  (check-equal? do:do-val 3))

(test-case "let& binds to the Monad flatmap method under a flatmap shadow"
  (check-equal? la:v 30))

(test-case "let% binds to flatmap/product methods under shadows"
  (check-equal? lp:v 7))

(test-case "let+ binds to fmap/product methods under shadows"
  (check-equal? lplus:v 11))

(test-case "do in a polymorphic monad dispatches at runtime under a flatmap shadow"
  (check-equal? poly:v 105))

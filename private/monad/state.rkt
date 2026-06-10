#lang racket/base

;; The State monad — the first half of the functional inference core
;; (PLAN.org "a functional inference core").
;;
;; A State computation is a function  st -> (values a st):  given the current
;; immutable state, it produces a result and the next state.  This replaces
;; the boxed/parameter mutation (the fresh counter, pending preds, resolution
;; tables) with explicit, threaded, immutable state.
;;
;; Public interface:
;;   state-return / state-bind / state-map / state-sequence  — the monad
;;   get / gets / put / modify                               — state access
;;   run-state                                               — the runner
;;   let/state / let/state+ / begin/state                    — binding syntax
;;
;; Binding syntax (see PLAN.org; mirrors OCaml's let* / let+):
;;   (let/state ([x cx] …) body …)
;;       monadic bind — each cx is a computation whose result binds to x, in
;;       sequence (a later cx may use an earlier x); the body is an implicit
;;       `begin/state`, the last form of which is the returned computation.
;;   (let/state+ ([x cx] …) body …)
;;       applicative bind — the cx are independent (none may reference
;;       another's x); the body is a PURE expression of the bound variables
;;       (no `state-return`), which `state-map` wraps.
;;   (begin/state c …)
;;       sequence computations for effect, returning the last result.

(require (for-syntax racket/base))

(provide state-return state-bind state-map state-sequence
         get gets put modify
         run-state
         let/state let/state+ begin/state)

;; ----- the monad ----------------------------------------------------

;; return : a -> State a
(define ((state-return a) st) (values a st))

;; bind : State a -> (a -> State b) -> State b
(define ((state-bind m f) st)
  (let-values ([(a st*) (m st)])
    ((f a) st*)))

;; map (fmap) : (a -> b) -> State a -> State b
(define (state-map f m)
  (state-bind m (lambda (a) (state-return (f a)))))

;; sequence : (listof (State a)) -> State (listof a), threading state
;; left-to-right.  The applicative `let/state+` builds on this: the results
;; are collected independently, then a pure function consumes them.
(define (state-sequence ms)
  (cond
    [(null? ms) (state-return '())]
    [else
     (state-bind (car ms)
       (lambda (v)
         (state-bind (state-sequence (cdr ms))
           (lambda (vs) (state-return (cons v vs))))))]))

;; ----- state access -------------------------------------------------

;; get : State st            — read the state as the result
(define (get st) (values st st))
;; gets : (st -> a) -> State a
(define ((gets f) st) (values (f st) st))
;; put : st -> State void    — replace the state
(define ((put st*) _st) (values (void) st*))
;; modify : (st -> st) -> State void
(define ((modify f) st) (values (void) (f st)))

;; run-state : State a -> st -> (values a st)
(define (run-state m st0) (m st0))

;; ----- binding syntax -----------------------------------------------

;; Monadic let* over State; the body is an implicit `begin/state`.
(define-syntax let/state
  (syntax-rules ()
    [(_ () body ...) (begin/state body ...)]
    [(_ ([x e] rest ...) body ...)
     (state-bind e (lambda (x) (let/state (rest ...) body ...)))]))

;; Sequence computations for effect; the last is the returned computation.
(define-syntax begin/state
  (syntax-rules ()
    [(_ e) e]
    [(_ e0 e ...) (state-bind e0 (lambda (_) (begin/state e ...)))]))

;; Applicative let over State: independent right-hand sides, pure body.
;; Desugars to `pure (lambda (x …) body) <*> …`, expressed as a map over the
;; sequenced results — so the right-hand sides cannot reference one another's
;; bindings (they are only in scope in the body), while state still threads.
(define-syntax let/state+
  (syntax-rules ()
    [(_ ([x e] ...) body ...)
     (state-map (lambda (vs) (apply (lambda (x ...) body ...) vs))
                (state-sequence (list e ...)))]))

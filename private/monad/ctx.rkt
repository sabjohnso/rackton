#lang racket/base

;; The Environment monad — the second half of the functional inference core
;; (PLAN.org "a functional inference core").
;;
;; A context computation is a function  ctx -> a:  it reads an immutable
;; context (the typing env + config flags) but cannot change it.  This
;; replaces the read-only `parameter` dynamic scope (current-type-columns,
;; the config flags, …) with an explicitly threaded context.
;;
;; Named throughout for `ctx`, never "reader" (which in any Lisp means the
;; s-expression reader) and never "env" (which already names the typing
;; environment everywhere in the compiler).  `ctx` is exactly what this
;; monad reads, and it matches the `let/ctx` binding macros.
;;
;; Public interface:
;;   ctx-return / ctx-bind / ctx-map / ctx-sequence  — the monad
;;   ask / asks / local                              — context access
;;   run-ctx                                         — the runner
;;   let/ctx / let/ctx+ / begin/ctx                  — binding syntax
;;
;; Binding syntax (parallels State's, see private/monad/state.rkt):
;;   (let/ctx ([x cx] …) body …)   monadic bind; body is an implicit begin/ctx.
;;   (let/ctx+ ([x cx] …) body …)  applicative bind; the cx are independent
;;                                 reads and the body is a PURE expression.
;;   (begin/ctx c …)               run each, return the last.

(require (for-syntax racket/base))

(provide ctx-return ctx-bind ctx-map ctx-sequence
         ask asks local
         run-ctx
         let/ctx let/ctx+ begin/ctx)

;; ----- the monad ----------------------------------------------------

;; return : a -> Ctx a
(define ((ctx-return a) _ctx) a)

;; bind : Ctx a -> (a -> Ctx b) -> Ctx b
(define ((ctx-bind m f) ctx)
  ((f (m ctx)) ctx))

;; map (fmap) : (a -> b) -> Ctx a -> Ctx b
(define (ctx-map f m)
  (ctx-bind m (lambda (a) (ctx-return (f a)))))

;; sequence : (listof (Ctx a)) -> Ctx (listof a)
(define (ctx-sequence ms)
  (cond
    [(null? ms) (ctx-return '())]
    [else
     (ctx-bind (car ms)
       (lambda (v)
         (ctx-bind (ctx-sequence (cdr ms))
           (lambda (vs) (ctx-return (cons v vs))))))]))

;; ----- context access -----------------------------------------------

;; ask : Ctx ctx            — read the whole context
(define (ask ctx) ctx)
;; asks : (ctx -> a) -> Ctx a
(define ((asks f) ctx) (f ctx))
;; local : (ctx -> ctx) -> Ctx a -> Ctx a  — run under a modified context
(define ((local f m) ctx) (m (f ctx)))

;; run-ctx : Ctx a -> ctx -> a
(define (run-ctx m ctx) (m ctx))

;; ----- binding syntax -----------------------------------------------

(define-syntax let/ctx
  (syntax-rules ()
    [(_ () body ...) (begin/ctx body ...)]
    [(_ ([x e] rest ...) body ...)
     (ctx-bind e (lambda (x) (let/ctx (rest ...) body ...)))]))

(define-syntax begin/ctx
  (syntax-rules ()
    [(_ e) e]
    [(_ e0 e ...) (ctx-bind e0 (lambda (_) (begin/ctx e ...)))]))

;; Applicative let over the context monad: independent reads, pure body.
(define-syntax let/ctx+
  (syntax-rules ()
    [(_ ([x e] ...) body ...)
     (ctx-map (lambda (vs) (apply (lambda (x ...) body ...) vs))
              (ctx-sequence (list e ...)))]))

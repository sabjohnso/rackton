#lang racket/base

;; The Infer monad — the combined working monad for the inference engine
;; (PLAN.org "a functional inference core", Phase 0 deliverable 4).
;;
;; NOTE: this is the Infer *monad*, not the inference engine — the engine
;; lives in private/infer.rkt and will eventually run inside this monad.
;;
;; Infer is the Environment monad over the State monad:
;;     Infer a = (ctx st) -> (values a st)
;; given a read-only context (the typing env + config) and the current
;; immutable state (the fresh counter, and later the pending preds and
;; resolution tables), it produces a result and the next state.
;;
;; The binding macros use the AUTO-LIFTING design (the settled choice):
;;   (let/state ([x sc] …) body …)
;;       each sc is a RAW State computation; it is lifted with state->infer
;;       at the binder and bound; the body is an implicit `begin/infer`.
;;   (let/ctx ([x cc] …) body …)
;;       each cc is a RAW Environment computation, lifted with ctx->infer.
;;   (begin/infer c …)   sequence Infer computations, return the last.
;; So `let/state` and `let/ctx` stay genuinely monad-specific — their
;; right-hand sides are State resp. Environment computations — yet they
;; compose in one body (a let/state body may contain a let/ctx), because the
;; body forms are Infer computations.  To sequence a State op for effect in a
;; body, bind it (e.g. `[_ (put x)]`).
;;
;; We import the State and Environment OPS via `only-in` (not their
;; standalone binding macros), so this module's `let/state`/`let/ctx` are the
;; only ones in scope for an importer; the engine requires just this module.

;; Only the OPS are imported — not the standalone binding macros — so this
;; module's auto-lifting let/state / let/ctx are the only ones an importer
;; sees.  bind/return/map/sequence of State and Ctx aren't needed: Infer has
;; its own, reached through the lifts.
(require (for-syntax racket/base)
         (only-in "state.rkt" get gets put modify)
         (only-in "ctx.rkt"   ask asks local))

(provide ;; the monad
         infer-return infer-bind infer-map infer-sequence run-infer
         ;; lifts
         state->infer ctx->infer
         ;; state record + fresh-variable supply + pending-pred bag
         (struct-out infer-state) make-infer-state fresh
         add-preds snapshot-preds set-preds
         ;; re-exported raw ops (used as let/state / let/ctx right-hand sides)
         get gets put modify
         ask asks local
         ;; binding syntax
         let/state let/state+
         let/ctx let/ctx+
         let/infer let/infer+ begin/infer)

;; ----- the monad ----------------------------------------------------

;; return : a -> Infer a
(define ((infer-return a) _ctx st) (values a st))

;; bind : Infer a -> (a -> Infer b) -> Infer b
(define ((infer-bind m f) ctx st)
  (let-values ([(a st*) (m ctx st)])
    ((f a) ctx st*)))

;; map (fmap) : (a -> b) -> Infer a -> Infer b
(define (infer-map f m)
  (infer-bind m (lambda (a) (infer-return (f a)))))

;; sequence : (listof (Infer a)) -> Infer (listof a)
(define (infer-sequence ms)
  (cond
    [(null? ms) (infer-return '())]
    [else
     (infer-bind (car ms)
       (lambda (v)
         (infer-bind (infer-sequence (cdr ms))
           (lambda (vs) (infer-return (cons v vs))))))]))

;; run-infer : Infer a -> ctx -> st -> (values a st)
(define (run-infer m ctx st) (m ctx st))

;; ----- lifts --------------------------------------------------------

;; state->infer : State a -> Infer a   (ignore ctx, run on st)
(define ((state->infer sc) _ctx st) (sc st))

;; ctx->infer : Ctx a -> Infer a       (read ctx, leave st untouched)
(define ((ctx->infer cc) ctx st) (values (cc ctx) st))

;; ----- state record + fresh-variable supply -------------------------
;; The inference state.  Grows by phase (PLAN.org): the fresh counter and
;; the pending-pred bag now; the codegen-plan tables in #4.

(struct infer-state (fresh-counter pending-preds) #:transparent)
(define (make-infer-state) (infer-state 0 '()))

;; fresh : State symbol  — a fresh, distinct name, bumping the counter.
;; (The engine wraps the name in a `tvar`; kept symbol-only here so the
;; monad stays decoupled from types.rkt.)
(define (fresh st)
  (define n (infer-state-fresh-counter st))
  (values (string->symbol (string-append "t" (number->string n)))
          (struct-copy infer-state st [fresh-counter (add1 n)])))

;; ----- pending-pred accumulator (State ops on infer-state) ----------
;; Replaces the current-pending-preds box (infer.rkt).  add-preds prepends,
;; matching the existing add-preds! semantics.

(define (snapshot-preds st) (values (infer-state-pending-preds st) st))
(define ((add-preds ps) st)
  (values (void)
          (struct-copy infer-state st
                       [pending-preds (append ps (infer-state-pending-preds st))])))
(define ((set-preds ps) st)
  (values (void) (struct-copy infer-state st [pending-preds ps])))

;; ----- binding syntax (auto-lifting) --------------------------------

;; let/state — bind raw State computations, lifted into Infer; the body is
;; an implicit begin/infer (its forms are Infer computations).
(define-syntax let/state
  (syntax-rules ()
    [(_ () body ...) (begin/infer body ...)]
    [(_ ([x e] rest ...) body ...)
     (infer-bind (state->infer e) (lambda (x) (let/state (rest ...) body ...)))]))

;; let/ctx — bind raw Environment computations, lifted into Infer.
(define-syntax let/ctx
  (syntax-rules ()
    [(_ () body ...) (begin/infer body ...)]
    [(_ ([x e] rest ...) body ...)
     (infer-bind (ctx->infer e) (lambda (x) (let/ctx (rest ...) body ...)))]))

;; begin/infer — sequence Infer computations for effect, return the last.
(define-syntax begin/infer
  (syntax-rules ()
    [(_ e) e]
    [(_ e0 e ...) (infer-bind e0 (lambda (_) (begin/infer e ...)))]))

;; let/infer — bind raw Infer computations (no lift): the result of another
;; Infer function, a recursive call, a composed computation.  The engine
;; composes Infer values constantly, so this is the workhorse binder; the
;; body is an implicit begin/infer.
(define-syntax let/infer
  (syntax-rules ()
    [(_ () body ...) (begin/infer body ...)]
    [(_ ([x e] rest ...) body ...)
     (infer-bind e (lambda (x) (let/infer (rest ...) body ...)))]))

;; let/infer+ — applicative over independent Infer computations.
(define-syntax let/infer+
  (syntax-rules ()
    [(_ ([x e] ...) body ...)
     (infer-map (lambda (vs) (apply (lambda (x ...) body ...) vs))
                (infer-sequence (list e ...)))]))

;; let/state+ — applicative: independent State right-hand sides (each
;; lifted), pure body.
(define-syntax let/state+
  (syntax-rules ()
    [(_ ([x e] ...) body ...)
     (infer-map (lambda (vs) (apply (lambda (x ...) body ...) vs))
                (infer-sequence (list (state->infer e) ...)))]))

;; let/ctx+ — applicative over the environment.
(define-syntax let/ctx+
  (syntax-rules ()
    [(_ ([x e] ...) body ...)
     (infer-map (lambda (vs) (apply (lambda (x ...) body ...) vs))
                (infer-sequence (list (ctx->infer e) ...)))]))

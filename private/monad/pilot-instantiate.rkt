#lang racket/base

;; Phase-1 pilot (PLAN.org): `instantiate` ported to the Infer monad, to
;; validate the binder notation against real inference code before committing
;; the whole engine.
;;
;; Compare with the live private/infer.rkt::instantiate, which mutates two
;; dynamically-scoped boxes — the fresh counter (via fresh-tvar) and the
;; pending-pred bag (via add-preds!).  Here both are threaded through the
;; Infer state: `fresh` for the counter, `add-preds` for the pred bag.  The
;; control flow is identical to the original; only the plumbing changes.
;;
;; Reference original (private/infer.rkt), for side-by-side reading:
;;
;;   (define (instantiate sch)
;;     (define raw
;;       (match sch
;;         [(scheme '() body) body]
;;         [(scheme vs body)
;;          (define s (for/fold ([s empty-subst]) ([v (in-list vs)])
;;                      (subst-extend s v (fresh-tvar v))))
;;          (apply-subst s body)]))
;;     (define unforalled (instantiate-tforall raw))
;;     (cond
;;       [(qual? unforalled)
;;        (add-preds! (qual-constraints unforalled))
;;        (instantiate-tforall (qual-body unforalled))]
;;       [else unforalled]))

(require "infer.rkt"
         "../types.rkt"
         racket/match)

(provide instantiate-m instantiate-tforall-m)

;; A fresh tvar, as an Infer computation.
(define fresh-tvar-m
  (let/state ([n fresh]) (infer-return (tvar n))))

;; Build a substitution sending each of `vs` to a distinct fresh tvar.
;;   fresh-subst-m : (listof symbol) -> Infer subst
(define (fresh-subst-m vs)
  (let loop ([vs vs] [s empty-subst])
    (cond
      [(null? vs) (infer-return s)]
      [else
       (let/infer ([t fresh-tvar-m])
         (loop (cdr vs) (subst-extend s (car vs) t)))])))

;; Strip the outermost tforalls, freshening each bound var.
;;   instantiate-tforall-m : type -> Infer type
(define (instantiate-tforall-m t)
  (match t
    [(tforall vs body)
     (let/infer ([s (fresh-subst-m vs)])
       (instantiate-tforall-m (apply-subst s body)))]
    [_ (infer-return t)]))

;; Instantiate a scheme.  Identical control flow to the original; the fresh
;; counter and the pending-pred bag ride in the Infer state.
;;   instantiate-m : scheme -> Infer type
(define (instantiate-m sch)
  (let/infer ([raw (match sch
                     [(scheme '() body) (infer-return body)]
                     [(scheme vs body)
                      (let/infer ([s (fresh-subst-m vs)])
                        (infer-return (apply-subst s body)))])]
              [unforalled (instantiate-tforall-m raw)])
    (cond
      [(qual? unforalled)
       (let/state ([_ (add-preds (qual-constraints unforalled))])
         (instantiate-tforall-m (qual-body unforalled)))]
      [else (infer-return unforalled)])))

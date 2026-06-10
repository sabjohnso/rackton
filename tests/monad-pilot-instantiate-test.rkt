#lang racket/base

;; Phase-1 pilot validation: the monadic `instantiate-m`
;; (private/monad/pilot-instantiate.rkt) must agree with the live
;; private/infer.rkt::instantiate.
;;
;; "Agree" = the instantiated type is alpha-equivalent (the two allocate
;; fresh vars with different prefixes — a0… vs t0… — so we canonicalise
;; tvar names by first occurrence before comparing) AND the same number of
;; constraints is surfaced into the pending-pred bag.

(module+ test
  (require rackunit
           rackcheck
           racket/match
           (only-in "../private/types.rkt"
                    tvar tvar? tcon tapp tforall scheme qual)
           (only-in "../private/infer.rkt"
                    instantiate current-fresh-state current-pending-preds)
           (only-in "../private/monad/infer.rkt"
                    run-infer make-infer-state infer-state-pending-preds)
           "../private/monad/pilot-instantiate.rkt"
           (submod "../private/type-gen.rkt" test))   ; gen:scheme

  ;; ----- run each variant -----
  (define (instantiate-orig sch)
    (parameterize ([current-fresh-state   (box 0)]
                   [current-pending-preds (box '())])
      (define t (instantiate sch))
      (values t (unbox (current-pending-preds)))))

  (define (instantiate-mon sch)
    (let-values ([(t st) (run-infer (instantiate-m sch) #f (make-infer-state))])
      (values t (infer-state-pending-preds st))))

  ;; ----- alpha-normalisation: canonical tvar names by first occurrence -----
  (define (normalize-tvars t)
    (define seen (make-hash))
    (define (canon name)
      (hash-ref! seen name (lambda () (string->symbol (format "_n~a" (hash-count seen))))))
    (let walk ([t t])
      (match t
        [(tvar n)          (tvar (canon n))]
        [(tcon _)          t]
        [(tapp h args)     (tapp (walk h) (map walk args))]
        [(tforall vs body) (tforall (map canon vs) (walk body))]
        [_ t])))

  ;; ----- the equivalence property -----
  (check-property
   (property instantiate-monadic-equiv-original ([sch (gen:scheme 3)])
     (let-values ([(t-o ps-o) (instantiate-orig sch)]
                  [(t-m ps-m) (instantiate-mon sch)])
       (and (equal? (normalize-tvars t-o) (normalize-tvars t-m))
            (= (length ps-o) (length ps-m))))))

  ;; ----- focused examples -----
  ;; empty scheme: body returned unchanged, no preds
  (let-values ([(t ps) (instantiate-mon (scheme '() (tcon 'Integer)))])
    (check-equal? t (tcon 'Integer))
    (check-equal? ps '()))

  ;; one var used twice: the sharing is preserved (both become the same
  ;; fresh tvar, distinct from the original 'a)
  (let-values ([(t _ps)
                (instantiate-mon
                 (scheme '(a) (tapp (tcon '->) (list (tvar 'a) (tvar 'a)))))])
    (match t
      [(tapp (tcon '->) (list x y))
       (check-true (tvar? x))
       (check-equal? x y)
       (check-not-equal? x (tvar 'a))]
      [_ (fail "expected an arrow")]))

  ;; two distinct vars stay distinct
  (let-values ([(t _ps)
                (instantiate-mon
                 (scheme '(a b) (tapp (tcon 'Pair) (list (tvar 'a) (tvar 'b)))))])
    (match t
      [(tapp (tcon 'Pair) (list x y))
       (check-true (and (tvar? x) (tvar? y)))
       (check-not-equal? x y)]
      [_ (fail "expected a Pair")])))

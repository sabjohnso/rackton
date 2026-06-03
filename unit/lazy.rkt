#lang rackton

;; rackton/unit — laziness primitives.
;;
;; Integrated shrinking represents a generated value together with its
;; (unbounded) tree of shrink candidates, so the children must live
;; behind a thunk or construction would diverge in this strict language.
;;
;; The canonical `Lazy`/`Stream` and their combinators now live in
;; rackton/data/lazy (memoizing / call-by-need — strictly better for
;; shrink trees, which are pure, so a forced subtree is computed once and
;; reused).  This module re-exports them and keeps two back-compat
;; aliases the unit framework grew up with:
;;
;;   force-lazy  =  force
;;   delay-lazy  =  defer an already-evaluated value  (= (delay x))
;;
;; New code should prefer the `delay` form, `force`, and the `stream-*`
;; combinators directly.

(require rackton/data/lazy)

(provide (all-from-out rackton/data/lazy)
         force-lazy
         delay-lazy)

(: force-lazy (-> (Lazy a) a))
(define (force-lazy l) (force l))

(: delay-lazy (-> a (Lazy a)))
(define (delay-lazy x) (delay x))

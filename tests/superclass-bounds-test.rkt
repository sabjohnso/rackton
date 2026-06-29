#lang rackton

;; End-to-end coverage for superclass BOUNDS and the :requires clause
;; (the retirement of the prefix `((Super x) => (Head …))` head).
;;
;; Two paths the prelude does not itself exercise as a user program:
;;
;;  1. A user-defined HIGHER-KINDED class whose parameter kind comes
;;     from its bound: `[f => Functor]` makes `f` have kind `* -> *`
;;     with no explicit `::` annotation (Variant A kind-from-bound).
;;     The Functor superclass must also be usable from the subclass.
;;
;;  2. A `:requires` clause as the body-level way to state a superclass
;;     constraint, here on a single parameter so it is equivalent to a
;;     `[a => Eq]` bound but travels the clause path through the parser
;;     and inference.

(require "../unit.rkt")

;; ----- 1. higher-kinded class via a bound --------------------------

(data (Box a) (MkBox a))

(instance (Functor Box)
  (define (fmap f b) (match b [(MkBox x) (MkBox (f x))])))

;; `[f => Functor]`: no `::`, yet `f` is used at kind `* -> *` in the
;; method signatures.  Compilation only succeeds if the kind is taken
;; from the Functor bound.
(protocol (Container [f => Functor])
  (: peek (-> (f a) a)))

(instance (Container Box)
  (define (peek b) (match b [(MkBox x) x])))

(: boxed (Box Integer))
(define boxed (MkBox 7))

(: peeked Integer)
(define peeked (peek boxed))

;; The Functor superclass is in scope for `Box` values.
(: mapped (Box Integer))
(define mapped (fmap (lambda (x) (+ x 1)) boxed))

(: mapped-val Integer)
(define mapped-val (peek mapped))

;; ----- 2. superclass via a :requires clause -----------------------

(protocol (Tagged a)
  (: tag (-> a Integer)))

;; :requires names the superclass in the body instead of on the head.
(protocol (Tagged2 a)
  (:requires (Tagged a))
  (: retag (-> a Integer)))

(instance (Tagged Integer)
  (define (tag x) x))

;; The body uses `tag`, which is available only because the `Tagged`
;; superclass constraint was discharged for the instance type.
(instance (Tagged2 Integer)
  (define (retag x) (+ (tag x) 100)))

(: retagged Integer)
(define retagged (retag 5))

;; ----- suite -------------------------------------------------------

(: suite (List Test))
(define suite
  (list
   (it "higher-kinded bound: peek extracts through the Functor box"
       (check-equal? peeked 7))
   (it "higher-kinded bound: Functor superclass method is usable"
       (check-equal? mapped-val 8))
   (it ":requires: subclass method reaches the superclass method"
       (check-equal? retagged 105))))

(: main Unit)
(define main (run-io (run-suite "superclass-bounds" suite)))

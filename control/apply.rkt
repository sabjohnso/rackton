#lang rackton

;; rackton/control/apply — Control.Apply.  `FunctorApply` (Edward Kmett's
;; "Apply") is a `Functor` that supports application but carries no
;; `pure` — it is `Applicative` minus the unit.
;;
;; Hierarchy note: `FunctorApply` sits PARALLEL to `Applicative`, not
;; below it.  Its only superclass is `Functor`.  The prelude's
;; `Applicative` is left untouched; every prelude `Applicative` simply
;; gets a `FunctorApply` instance here with `apply = fapply`, so the two
;; agree wherever both are defined.  Keeping it standalone means adding
;; `apply` to a type that is NOT a full `Applicative` (e.g. the env
;; comonad `Pair e`, which needs only `Semigroup e`) without forcing a
;; `pure` it cannot lawfully provide.

(provide (all-defined-out))

;; `apply` and `liftF2` form a mutual default cycle, mirroring the
;; prelude `Applicative`'s `fapply`/`liftA2`: an instance defines
;; whichever is more natural and the other derives.  Defining neither is
;; rejected at compile time (the cyclic-default check).
(protocol (FunctorApply [f => Functor])
  (: apply  (-> (f (-> a b)) (-> (f a) (f b))))
  (: liftF2 (-> (-> a (-> b c)) (-> (f a) (-> (f b) (f c)))))
  ;; apply ff fx = liftF2 (\g x -> g x) ff fx
  (define (apply ff fx) (liftF2 (lambda (g) (lambda (x) (g x))) ff fx))
  ;; liftF2 g fa fb = apply (fmap g fa) fb
  (define (liftF2 g fa fb) (apply (fmap g fa) fb))
  ;; The Apply composition law: `apply` is associative under composition.
  ;; This is the Applicative composition law minus the `pure`-dependent
  ;; identity and homomorphism, which `FunctorApply` cannot state (it has
  ;; no `pure`).  Quantified over containers of functions, so it type-
  ;; checks as the specification rather than running through the generic
  ;; bundle (no generator for `(f (-> …))`).
  #:laws
    ([composition ((Eq (f c)) =>
       (All ([u : (f (-> b c))] [v : (f (-> a b))] [w : (f a)])
         (== (apply (apply (fmap (lambda (g) (lambda (h) (lambda (x) (g (h x))))) u) v) w)
             (apply u (apply v w)))))]))

;; The prelude Applicatives are Applies with `apply = fapply`.

(instance (FunctorApply Maybe)
  (define (apply ff fx) (fapply ff fx)))

(instance (FunctorApply List)
  (define (apply ff fx) (fapply ff fx)))

(instance (FunctorApply (Either a))
  (define (apply ff fx) (fapply ff fx)))

(instance (FunctorApply Identity)
  (define (apply ff fx) (fapply ff fx)))

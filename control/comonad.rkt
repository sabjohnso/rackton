#lang rackton

;; rackton/control/comonad — Control.Comonad.  A `Comonad` is the
;; categorical dual of a `Monad`: where a monad lets you PUT a value in a
;; context (`pure`) and FLATTEN nested contexts (`join`), a comonad lets
;; you TAKE a value out of a context (`extract`) and SPLIT a context into
;; a context-of-contexts (`duplicate`).
;;
;;   Monad    pure :: a -> m a      join      :: m (m a) -> m a
;;   Comonad  extract :: w a -> a   duplicate :: w a -> w (w a)
;;
;; `extend` is the dual of `flatmap`: it runs a co-Kleisli arrow
;; `w a -> b` over every "position" of the structure.
;;
;; `ComonadApply` is the comonadic counterpart of `FunctorApply`: a
;; comonad whose values can be applied positionwise (`coapply` / `<@>`),
;; defaulting to the `FunctorApply` `apply`.

(require rackton/control/apply)

(provide (all-defined-out))

;; `duplicate` and `extend` form a mutual default cycle (dual to the
;; prelude `Monad`'s `join`/`flatmap`): an instance defines whichever is
;; natural and the other derives.  `extract` is the sole irreducible
;; primitive.  Defining neither of the cyclic pair is rejected at compile
;; time.
(protocol (Comonad [w => Functor])
  (: extract   (-> (w a) a))
  (: duplicate (-> (w a) (w (w a))))
  (: extend    (-> (-> (w a) b) (-> (w a) (w b))))
  ;; duplicate = extend id
  (define (duplicate w) (extend (lambda (x) x) w))
  ;; extend f = fmap f . duplicate
  (define (extend f w) (fmap f (duplicate w)))
  ;; The comonad laws, dual to the prelude Monad laws: `extract` is a left
  ;; and right counit for `duplicate`, and `duplicate` is coassociative.
  ;; Quantified over the element type; the container is compared via an
  ;; assumed `(Eq (w …))`.  All three are property-runnable (no arrow
  ;; binder, no return-typed method).
  :laws
    ([extract-duplicate ((Eq (w a)) =>
       (All ([c : (w a)]) (== (extract (duplicate c)) c)))]
     [fmap-extract-duplicate ((Eq (w a)) =>
       (All ([c : (w a)]) (== (fmap extract (duplicate c)) c)))]
     [duplicate-duplicate ((Eq (w (w (w a)))) =>
       (All ([c : (w a)])
         (== (duplicate (duplicate c)) (fmap duplicate (duplicate c)))))]))

;; ComonadApply: a Comonad that is also a FunctorApply, with `coapply`
;; (Haskell's `<@>`) defaulting to the `FunctorApply` `apply`.  Instances
;; whose zip is cheaper to write directly may override it.
(protocol (ComonadApply (w :: (-> * *)))
  (:requires (Comonad w) (FunctorApply w))
  (: coapply (-> (w (-> a b)) (-> (w a) (w b))))
  (define (coapply ff fx) (apply ff fx))
  ;; `coapply` must agree with the inherited `FunctorApply` `apply`: an
  ;; instance that overrides the default for a cheaper zip stays
  ;; consistent with it.  Quantified over a container of functions, so it
  ;; type-checks as the specification (no generator for `(w (-> …))`).
  :laws
    ([coapply-apply ((Eq (w b)) =>
       (All ([ff : (w (-> a b))] [fx : (w a)])
         (== (coapply ff fx) (apply ff fx))))]))

;; --- Identity: the trivial comonad ----------------------------------
;; `extract` unwraps; `duplicate` adds exactly one layer.  Since there is
;; only one position, `extend f = Identity . f`.
(instance (Comonad Identity)
  (define (extract i) (match i [(Identity x) x]))
  (define (duplicate i) (Identity i)))

(instance (ComonadApply Identity)
  (define (coapply ff fx) (apply ff fx)))

#lang rackton

;; rackton/data/tuple — Data.Tuple.  `fst` / `snd` stay in the prelude;
;; `swap` moves here (Phase 2 slim).  `curry` / `uncurry` convert
;; between a Pair-taking function and its two-argument form.

(require rackton/control/apply
         rackton/control/comonad)

(provide (all-defined-out))

(: swap (-> (Pair a b) (Pair b a)))
(define (swap p) (match p [(Pair a b) (Pair b a)]))

;; --- Pair as the ENV comonad ----------------------------------------
;;
;; `Pair e` carries an environment `e` next to a focus `a`.  Its Functor
;; and Comonad act on the focus (the second component); duplicate copies
;; the environment into the new outer layer.  FunctorApply/ComonadApply
;; combine two environments with `mappend`, hence the `Semigroup e`
;; constraint (this is the env comonad's `Apply`, distinct from any
;; `Applicative (Pair e)`, which would need a full `Monoid e`).
;;
;; Methods are written out in full: the FunctorApply / Comonad /
;; ComonadApply default bodies live in their own modules and a class's
;; defaults do not cross the module boundary (the sidecar drops them), so
;; a cross-module instance must be complete.

(instance (Functor (Pair a))
  (define (fmap f p) (match p [(Pair e x) (Pair e (f x))])))

(instance (Comonad (Pair a))
  (define (extract p) (match p [(Pair _ x) x]))
  (define (duplicate p) (match p [(Pair e x) (Pair e (Pair e x))]))
  (define (extend f w) (fmap f (duplicate w))))

(instance ((Semigroup e) => (FunctorApply (Pair e)))
  (define (apply ff fx)
    (match ff
      [(Pair e1 f) (match fx [(Pair e2 x) (Pair (mappend e1 e2) (f x))])]))
  (define (liftF2 g fa fb) (apply (fmap g fa) fb)))

(instance ((Semigroup e) => (ComonadApply (Pair e)))
  (define (coapply ff fx) (apply ff fx)))

;; curry: turn a function on a Pair into a two-argument function.
(: curry (-> (-> (Pair a b) c) (-> a (-> b c))))
(define (curry f a b) (f (Pair a b)))

;; uncurry: turn a two-argument function into one taking a Pair.
(: uncurry (-> (-> a (-> b c)) (-> (Pair a b) c)))
(define (uncurry f p) (match p [(Pair a b) (f a b)]))

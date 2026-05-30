#lang rackton

;; rackton/data/functor — Data.Functor.  (fmap is the prelude class
;; method; void is in the prelude.)

(provide (all-defined-out))

;; (const-map a fa) — replace every value in fa with a (Haskell `<$`).
(: const-map ((Functor f) => (-> a (-> (f b) (f a)))))
(define (const-map a fa) (fmap (lambda (_) a) fa))

;; (fmap-flipped fa f) = (fmap f fa) (Haskell `<&>`).
(: fmap-flipped ((Functor f) => (-> (f a) (-> (-> a b) (f b)))))
(define (fmap-flipped fa f) (fmap f fa))

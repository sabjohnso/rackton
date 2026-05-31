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

;; (const-replace-flipped fa b) — replace every value in fa with b, with
;; the arguments flipped relative to const-map (Haskell `$>`).
(: const-replace-flipped ((Functor f) => (-> (f a) (-> b (f b)))))
(define (const-replace-flipped fa b) (const-map b fa))

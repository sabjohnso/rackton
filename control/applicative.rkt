#lang rackton

;; rackton/control/applicative — Control.Applicative.  pure / fapply /
;; liftA2 are prelude Applicative methods; when / unless are in the
;; prelude.  This adds the higher-arity lift.

(provide (all-defined-out))

;; lift-a3: apply a curried 3-ary function under an applicative.
(: lift-a3 ((Applicative f) =>
            (-> (-> a (-> b (-> c d)))
                (-> (f a) (-> (f b) (-> (f c) (f d)))))))
(define (lift-a3 g fa fb fc)
  (fapply (fapply (fmap g fa) fb) fc))

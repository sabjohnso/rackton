#lang racket/base

;; User-defined needs-dict instance bodies (the lifted-instance
;; pattern, ahead of mtl-style classes).

(require rackunit
         "../main.rkt")

(rackton
  (require rackton/control/monad/state)
  (require rackton/control/monad/reader)
  ;; ----- A small class with ONE return-typed method ---------
  (protocol (HasUnit (m :: (-> * *)))
    (: unit-val (m Integer)))

  ;; Base instances
  (instance (HasUnit Maybe)
    (define unit-val (Some 1)))

  (instance (HasUnit IO)
    (define unit-val (pure-io 2)))

  ;; Lifted instance over EnvT: the body uses the polymorphic
  ;; `unit-val` whose class param `m` is bound by the instance qual.
  (instance ((HasUnit m) => (HasUnit (EnvT String m)))
    (define unit-val (lift-env-t unit-val)))

  ;; Lifted instance over StateT: needs (Functor m) too, because
  ;; lift-state-t uses inner fmap.  This is the first instance in
  ;; the tests whose qual context has multiple constraints, both of
  ;; which are dict-bearing.
  (instance ((HasUnit m) (Functor m) => (HasUnit (StateT Integer m)))
    (define unit-val (lift-state-t unit-val)))

  ;; Concrete uses
  (: env-of-maybe (EnvT String Maybe Integer))
  (define env-of-maybe unit-val)

  (: env-of-io (EnvT String IO Integer))
  (define env-of-io unit-val)

  (: state-of-maybe (StateT Integer Maybe Integer))
  (define state-of-maybe unit-val)

  (: state-of-io (StateT Integer IO Integer))
  (define state-of-io unit-val)

  ;; ----- A class with TWO return-typed methods to confirm dict-
  ;; arg ordering is stable ----------------------------------
  (protocol (TwoVals (m :: (-> * *)))
    (: one-val (m Integer))
    (: two-val (m Integer)))

  (instance (TwoVals Maybe)
    (define one-val (Some 10))
    (define two-val (Some 20)))

  (instance ((TwoVals m) => (TwoVals (EnvT String m)))
    (define one-val (lift-env-t one-val))
    (define two-val (lift-env-t two-val)))

  (: env-one (EnvT String Maybe Integer))
  (define env-one one-val)

  (: env-two (EnvT String Maybe Integer))
  (define env-two two-val))

;; ---------- assertions ---------------------------------------

(test-case "lifted HasUnit over EnvT Maybe"
  (check-equal? ((run-env-t env-of-maybe) "ignored") (Some 1)))

(test-case "lifted HasUnit over EnvT IO"
  (check-equal? (run-io ((run-env-t env-of-io) "ignored")) 2))

(test-case "lifted HasUnit over StateT Maybe"
  (check-equal? ((run-state-t state-of-maybe) 0) (Some (MkPair 0 1))))

(test-case "lifted HasUnit over StateT IO"
  (check-equal? (run-io ((run-state-t state-of-io) 0)) (MkPair 0 2)))

(test-case "two-method lifted instance: one-val"
  (check-equal? ((run-env-t env-one) "ignored") (Some 10)))

(test-case "two-method lifted instance: two-val"
  (check-equal? ((run-env-t env-two) "ignored") (Some 20)))

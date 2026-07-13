#lang racket/base

;; A non-prelude monad — user-defined OR stdlib Result — must be usable
;; as the base monad of a transformer stack, including the runtime-witness
;; path that nested ExceptT-over-ExceptT exercises.  Codegen registers
;; each return-typed `pure` into the $pure-by-tag witness table (keyed by
;; the type's $ctor: tags), so `pure-via-witness` can reconstruct it.

(require rackunit rackton)

(rackton
  (require rackton/control/monad/except)
  (require rackton/data/result)

  ;; ----- a user-defined monad (an Identity clone) -----------------
  (data (Box a) (Box a))
  (instance (Functor Box)
    (define (fmap f b) (match b [(Box x) (Box (f x))])))
  (instance (Applicative Box)
    (define (pure x) (Box x))
    (define (fapply bf bx) (match bf [(Box f) (fmap f bx)])))
  (instance (Monad Box)
    (define (flatmap f b) (match b [(Box x) (f x)])))
  (: unbox-box (-> (Box a) a))
  (define (unbox-box b) (match b [(Box x) x]))

  ;; single layer: ExceptT over the user monad
  (: one (ExceptT String Box Integer))
  (define one (let& ([x (pure 5)] [y (pure 37)]) (pure (+ x y))))
  (: one-shown String)
  (define one-shown (show (unbox-box (run-except-t one))))

  ;; nested: ExceptT over ExceptT over the user monad (witness path)
  (: two (ExceptT String (ExceptT String Box) Integer))
  (define two (let& ([x (pure 5)] [y (pure 37)]) (pure (+ x y))))
  (: two-shown String)
  (define two-shown (show (unbox-box (run-except-t (run-except-t two))))))

(test-case "ExceptT over a user monad (single layer)"
  (check-equal? one-shown "(Ok 42)"))

(test-case "ExceptT over ExceptT over a user monad (nested witness path)"
  (check-equal? two-shown "(Ok (Ok 42))"))

(displayln "transformer-base-monad: passed")

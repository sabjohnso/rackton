#lang rackton

;; rackton/data/kleisli — the Kleisli category of a Monad.
;;
;; `Kleisli m a b` wraps a monadic function `a -> m b`.  Composing two
;; such functions with `flatmap` is Kleisli composition, so over any
;; `Monad m` the wrapper is a full Arrow:
;;
;;   Category     ident = pure;  g . f = \a -> f a >>= g
;;   Arrow        arr f = pure . f;  first lifts over a strict Pair
;;   ArrowChoice  on-left routes the Left branch through the arrow
;;   ArrowApply   arrow-app applies a wrapped arrow to its argument
;;
;; `ArrowLoop` is intentionally absent: tying its feedback knot needs
;; `MonadFix` (a lawful `mfix`), which the language does not provide.
;;
;; Instance bodies pattern-match the `Kleisli` wrapper inline rather than
;; through `run-kleisli`: matching directly keeps the wrapped function's
;; type linked to the instance's `m`, which an accessor call would leave
;; too polymorphic for the method's declared type to check.
;;
;; on-second / split / fanout (Arrow) and on-right / fork / fanin
;; (ArrowChoice) are written out rather than left to the prelude class
;; defaults.  Those defaults build the derived combinators from `arr`
;; (and `ident`), and `arr`/`ident` are RETURN-TYPED: inside a default
;; body compiled against the abstract class parameter, their instance
;; cannot be picked statically and falls to a runtime return-typed
;; lookup that has no entry for a library type like `Kleisli`.  Defining
;; the combinators directly with `flatmap`/`pure` sidesteps that lookup
;; entirely (the same reason rackton/data/arrow-lazy defines its own).

(provide (all-defined-out))

(data (Kleisli m a b) (Kleisli (-> a (m b))))

;; Unwrap to the underlying monadic function.
(: run-kleisli (-> (Kleisli m a b) (-> a (m b))))
(define (run-kleisli k) (match k [(Kleisli f) f]))

;; --- Category -------------------------------------------------------
(instance ((Monad m) => (Category (Kleisli m)))
  (define ident (Kleisli (lambda (x) (pure x))))
  ;; comp g f = \a -> f a >>= g   (right-to-left, matching `.`)
  (define (comp g f)
    (match g
      [(Kleisli gf)
       (match f
         [(Kleisli ff) (Kleisli (lambda (a) (flatmap gf (ff a))))])])))

;; --- Arrow over the strict product Pair -----------------------------
(instance ((Monad m) => (Arrow (Kleisli m) Pair))
  (define (arr f) (Kleisli (lambda (x) (pure (f x)))))
  ;; first f = \(a, c) -> do b <- f a; pure (b, c)
  (define (on-first k)
    (match k
      [(Kleisli f)
       (Kleisli (lambda (q)
                  (match q
                    [(Pair a c) (flatmap (lambda (b) (pure (Pair b c))) (f a))])))]))
  (define (on-second k)
    (match k
      [(Kleisli g)
       (Kleisli (lambda (q)
                  (match q
                    [(Pair c a) (flatmap (lambda (b) (pure (Pair c b))) (g a))])))]))
  ;; f *** g = \(a, c) -> do b <- f a; d <- g c; pure (b, d)
  (define (split kf kg)
    (match kf
      [(Kleisli f)
       (match kg
         [(Kleisli g)
          (Kleisli (lambda (q)
                     (match q
                       [(Pair a c)
                        (flatmap (lambda (b)
                                   (flatmap (lambda (d) (pure (Pair b d))) (g c)))
                                 (f a))])))])]))
  ;; f &&& g = \a -> do b <- f a; c <- g a; pure (b, c)
  (define (fanout kf kg)
    (match kf
      [(Kleisli f)
       (match kg
         [(Kleisli g)
          (Kleisli (lambda (a)
                     (flatmap (lambda (b)
                                (flatmap (lambda (c) (pure (Pair b c))) (g a)))
                              (f a))))])])))

;; --- ArrowChoice over the strict coproduct Either -------------------
(instance ((Monad m) => (ArrowChoice (Kleisli m) Pair Either))
  ;; on-left f routes a Left through f and passes a Right straight on.
  (define (on-left k)
    (match k
      [(Kleisli f)
       (Kleisli (lambda (e)
                  (match e
                    [(Left a)  (flatmap (lambda (b) (pure (Left b))) (f a))]
                    [(Right x) (pure (Right x))])))]))
  (define (on-right k)
    (match k
      [(Kleisli g)
       (Kleisli (lambda (e)
                  (match e
                    [(Left x)  (pure (Left x))]
                    [(Right a) (flatmap (lambda (b) (pure (Right b))) (g a))])))]))
  ;; f +++ g routes each branch through its arrow, staying in the coproduct.
  (define (fork kf kg)
    (match kf
      [(Kleisli f)
       (match kg
         [(Kleisli g)
          (Kleisli (lambda (e)
                     (match e
                       [(Left a)  (flatmap (lambda (b) (pure (Left b))) (f a))]
                       [(Right c) (flatmap (lambda (d) (pure (Right d))) (g c))])))])]))
  ;; f ||| g routes each branch through its arrow and merges the results.
  (define (fanin kf kg)
    (match kf
      [(Kleisli f)
       (match kg
         [(Kleisli g)
          (Kleisli (lambda (e)
                     (match e
                       [(Left a)  (f a)]
                       [(Right b) (g b)])))])])))

;; --- ArrowApply -----------------------------------------------------
(instance ((Monad m) => (ArrowApply (Kleisli m) Pair))
  ;; arrow-app applies the wrapped arrow in the first component to the
  ;; argument in the second.
  (define arrow-app
    (Kleisli (lambda (q)
               (match q
                 [(Pair k x) (match k [(Kleisli f) (f x)])])))))

#lang rackton

;; rackton/data/cokleisli — the co-Kleisli category of a Comonad.
;;
;; `Cokleisli w a b` wraps a context-consuming function `w a -> b`.
;; Composing two such functions with `extend` is co-Kleisli composition,
;; so over any `Comonad w` the wrapper is a Category and an Arrow:
;;
;;   Category  ident = extract;  g . f = \w -> g (extend f w)
;;   Arrow     arr h = h . extract;  first/split map over the context and
;;             read the untouched component from `extract`
;;
;; The Arrow needs only `Comonad` (Functor for `fmap`, plus `extract`):
;; `&&&` feeds the SAME context to both arrows, and `***` splits it with
;; `fmap fst` / `fmap snd`, so no `ComonadApply` zip is required.
;;
;; ArrowChoice and ArrowApply are intentionally absent: there is no
;; lawful way to route a `w (Either a c)` down one branch of a general
;; comonad, nor to apply a wrapped co-Kleisli arrow, so the lawful stack
;; tops out at Arrow.
;;
;; As with rackton/data/kleisli, instance bodies match the wrapper inline
;; (keeping the wrapped function tied to `w`), and the derived Arrow
;; combinators are written out rather than left to the class defaults —
;; the defaults route through the return-typed `arr`, which has no
;; runtime instance entry for a library type.

(require rackton/control/comonad)

(provide (all-defined-out))

(data (Cokleisli w a b) (Cokleisli (-> (w a) b)))

;; Unwrap to the underlying context-consuming function.
(: run-cokleisli (-> (Cokleisli w a b) (-> (w a) b)))
(define (run-cokleisli k) (match k [(Cokleisli f) f]))

;; --- Category -------------------------------------------------------
(instance ((Comonad w) => (Category (Cokleisli w)))
  (define ident (Cokleisli (lambda (w) (extract w))))
  ;; comp g f = \w -> g (extend f w)   (f first, via extend, then g)
  (define (comp g f)
    (match g
      [(Cokleisli gf)
       (match f
         [(Cokleisli ff) (Cokleisli (lambda (w) (gf (extend ff w))))])])))

;; --- Arrow over the strict product Pair -----------------------------
(instance ((Comonad w) => (Arrow (Cokleisli w) Pair))
  (define (arr h) (Cokleisli (lambda (w) (h (extract w)))))
  ;; first f = \w -> (f (fmap fst w), snd (extract w))
  (define (on-first k)
    (match k
      [(Cokleisli f)
       (Cokleisli (lambda (w)
                    (Pair (f (fmap fst w)) (snd (extract w)))))]))
  (define (on-second k)
    (match k
      [(Cokleisli g)
       (Cokleisli (lambda (w)
                    (Pair (fst (extract w)) (g (fmap snd w)))))]))
  ;; f *** g = \w -> (f (fmap fst w), g (fmap snd w))
  (define (split kf kg)
    (match kf
      [(Cokleisli f)
       (match kg
         [(Cokleisli g)
          (Cokleisli (lambda (w)
                       (Pair (f (fmap fst w)) (g (fmap snd w)))))])]))
  ;; f &&& g = \w -> (f w, g w)   — the SAME context feeds both
  (define (fanout kf kg)
    (match kf
      [(Cokleisli f)
       (match kg
         [(Cokleisli g)
          (Cokleisli (lambda (w) (Pair (f w) (g w))))])])))

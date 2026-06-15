#lang rackton

;; Pair's Functor / Comonad / FunctorApply / ComonadApply instances.
;; `Pair e` is the ENV (a.k.a. reader / writer) comonad: it carries an
;; environment `e` alongside a focus `a`.  `Functor` and `Comonad` map
;; over / focus on the second component; `FunctorApply`/`ComonadApply`
;; combine the environments with `mappend`, so they need `Semigroup e`.

(require rackton/data/tuple
         rackton/control/comonad
         rackton/control/apply
         "../unit.rkt")

;; --- Functor (Pair a): maps the second component --------------------
(: p-fmap (Pair String Integer))
(define p-fmap (fmap (lambda (x) (+ x 1)) (Pair "env" 41)))

;; --- Comonad (Pair e) -----------------------------------------------
(: p-extract Integer)
(define p-extract (extract (Pair "env" 42)))

;; duplicate copies the env into the new outer layer
(: p-dup-outer-env String)
(define p-dup-outer-env (match (duplicate (Pair "env" 1)) [(Pair e _) e]))
(: p-dup-inner-env String)
(define p-dup-inner-env
  (match (duplicate (Pair "env" 1)) [(Pair _ (Pair e _)) e]))

;; extend with extract is the identity
(: p-ext Integer)
(define p-ext (extract (extend extract (Pair "env" 9))))

;; --- FunctorApply (Pair e): mappends the envs -----------------------
(: p-apply (Pair String Integer))
(define p-apply
  (apply (Pair "a" (lambda (x) (+ x 1))) (Pair "b" 41)))

;; --- ComonadApply (Pair e) ------------------------------------------
(: p-coapply (Pair String Integer))
(define p-coapply
  (coapply (Pair "a" (lambda (x) (* x 2))) (Pair "b" 21)))

(: suite (List Test))
(define suite
  (list
   (it "Functor (Pair a) maps the focus"
       (check-equal? p-fmap (Pair "env" 42)))
   (it "extract is the focus"
       (check-equal? p-extract 42))
   (it "duplicate copies the env inward"
       (all-checks
        (list (check-equal? p-dup-outer-env "env")
              (check-equal? p-dup-inner-env "env"))))
   (it "extend extract is identity on the focus"
       (check-equal? p-ext 9))
   (it "FunctorApply (Pair e) mappends envs"
       (check-equal? p-apply (Pair "ab" 42)))
   (it "ComonadApply (Pair e) mappends envs"
       (check-equal? p-coapply (Pair "ab" 42)))))

(: _ran Unit)
(define _ran (run-io (run-suite "tuple comonad/apply" suite)))

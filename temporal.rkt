#lang rackton

;; rackton/temporal — guarded streams via the ▷ ("later") modality.
;;
;; `Later a` (written ▷ in the literature) is a value available "one step
;; from now".  It is the modality behind guarded recursion: the only way to
;; recurse is `lob`, whose type IS Löb's theorem (▷ as the provability □),
;; and whose self-reference sits UNDER a `Later`, so a stream is productive
;; by construction — it always yields its head before deferring its tail.
;;
;; A guarded stream is `Signal a = SigCons a (Later (Signal a))` — head now,
;; tail later.  Because the tail's type is a `Later`, an UNGUARDED tail (a
;; forced value where a `Later` is required) is a TYPE ERROR: the modality
;; rejects the most common non-productive mistake (see tests/temporal-guard).
;;
;; ▷ is the one grade here that carries runtime content.  It is built on the
;; prelude-adjacent memoizing `Lazy`, so `adv` CACHES: a stream's prefix is
;; computed once, not re-derived on each step.
;;
;; (`Signal`, not `Stream`, because rackton/data/lazy already ships `Stream`.)

(require rackton/data/lazy)

(provide (data-out Later)
         next adv map-later map-later2 lob
         (data-out Signal)
         sig-head sig-tail sig-repeat sig-map sig-zip sig-iterate sig-take)

;; ===== ▷ : the "later" modality, a memoizing delayed cell =============

(data (Later a) (MkLater (Lazy a)) :abstract)

(: next (-> a (Later a)))                       ; a, available next step
(define (next x) (MkLater (make-lazy (lambda (u) x))))

(: adv (-> (Later a) a))                         ; advance one tick (memoized)
(define (adv l) (match l [(MkLater z) (force z)]))

(: map-later (-> (-> a b) (Later a) (Later b)))             ; ▷ is a functor
(define (map-later g l) (MkLater (make-lazy (lambda (u) (g (adv l))))))

(: map-later2 (-> (-> a b c) (Later a) (Later b) (Later c)))  ; …and applicative
(define (map-later2 g la lb)
  (MkLater (make-lazy (lambda (u) (g (adv la) (adv lb))))))

;; the guarded fixpoint — `(lob f) = f (delayed (lob f))`: the self-reference
;; is under a `Later`, so it is forced only when the next step is demanded.
(: lob (-> (-> (Later a) a) a))
(define (lob f) (f (MkLater (make-lazy (lambda (u) (lob f))))))

;; ===== guarded streams: head now, tail LATER =========================

(data (Signal a) (SigCons a (Later (Signal a))))

(: sig-head (-> (Signal a) a))
(define (sig-head s) (match s [(SigCons x t) x]))
(: sig-tail (-> (Signal a) (Later (Signal a))))
(define (sig-tail s) (match s [(SigCons x t) t]))

(: sig-repeat (-> a (Signal a)))                 ; x, x, x, …  (via lob)
(define (sig-repeat x) (lob (lambda (self) (SigCons x self))))

;; x, f x, f (f x), …  — recursion sits under map-later, so it is guarded
(: sig-iterate (-> (-> a a) a (Signal a)))
(define (sig-iterate f x)
  (SigCons x (map-later (lambda (s) (sig-iterate f s)) (next (f x)))))

(: sig-map (-> (-> a b) (Signal a) (Signal b)))
(define (sig-map g s)
  (SigCons (g (sig-head s)) (map-later (lambda (s2) (sig-map g s2)) (sig-tail s))))

(: sig-zip (-> (-> a b c) (Signal a) (Signal b) (Signal c)))
(define (sig-zip g s1 s2)
  (SigCons (g (sig-head s1) (sig-head s2))
           (map-later2 (lambda (t1 t2) (sig-zip g t1 t2)) (sig-tail s1) (sig-tail s2))))

;; CONSUME a signal: force exactly n steps.  Terminates — each step advances
;; one Later and the producers yield one SigCons before deferring.
(: sig-take (-> Integer (Signal a) (List a)))
(define (sig-take n s)
  (if (<= n 0) Nil (Cons (sig-head s) (sig-take (- n 1) (adv (sig-tail s))))))

#lang rackton

;; rackton/data/cokleisli — the co-Kleisli category of a Comonad.
;; `Cokleisli w a b` wraps a context-consuming function `w a -> b`;
;; composing with `extend` is co-Kleisli composition.  Over a Comonad it
;; forms a Category and an Arrow.  Exercised here over `Identity`.

(require rackton/data/cokleisli
         rackton/control/comonad
         "../unit.rkt")

(: f1 (Cokleisli Identity Integer Integer))
(define f1 (Cokleisli (lambda (w) (+ (extract w) 1))))
(: f2 (Cokleisli Identity Integer Integer))
(define f2 (Cokleisli (lambda (w) (* (extract w) 2))))

;; --- Category -------------------------------------------------------
(: comp-out Integer)
(define comp-out (run-cokleisli (comp f2 f1) (Identity 10)))   ; (10+1)*2 = 22

(: ident-c (Cokleisli Identity Integer Integer))
(define ident-c ident)
(: ident-out Integer)
(define ident-out (run-cokleisli ident-c (Identity 5)))

;; --- Arrow ----------------------------------------------------------
(: arred (Cokleisli Identity Integer Integer))
(define arred (arr (lambda (x) (+ x 1))))
(: arr-out Integer)
(define arr-out (run-cokleisli arred (Identity 41)))

(: first-out (Pair Integer Integer))
(define first-out (run-cokleisli (on-first f1) (Identity (Pair 10 99))))

(: split-out (Pair Integer Integer))
(define split-out (run-cokleisli (split f1 f2) (Identity (Pair 10 20))))  ; (11, 40)

(: fan-out (Pair Integer Integer))
(define fan-out (run-cokleisli (fanout f1 f2) (Identity 10)))             ; (11, 20)

(: suite (List Test))
(define suite
  (list
   (it "Category: comp threads through extend"
       (all-checks (list (check-equal? comp-out 22)
                         (check-equal? ident-out 5))))
   (it "Arrow: arr lifts a function on the extracted focus"
       (check-equal? arr-out 42))
   (it "Arrow: first runs on the first component"
       (check-equal? first-out (Pair 11 99)))
   (it "Arrow: *** runs both components"
       (check-equal? split-out (Pair 11 40)))
   (it "Arrow: &&& fans the same context to both"
       (check-equal? fan-out (Pair 11 20)))))

(: main Unit)
(define main (run-io (run-suite "rackton/data/cokleisli" suite)))

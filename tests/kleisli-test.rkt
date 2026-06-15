#lang rackton

;; rackton/data/kleisli — the Kleisli category of a Monad.  `Kleisli m a
;; b` wraps a monadic function `a -> m b`; composing such functions with
;; `flatmap` is exactly Kleisli composition.  Over a Monad it forms a
;; full Arrow (Category, Arrow, ArrowChoice, ArrowApply).  Exercised here
;; over `Maybe`.

(require rackton/data/kleisli
         "../unit.rkt")

(: k1 (Kleisli Maybe Integer Integer))
(define k1 (Kleisli (lambda (x) (Some (+ x 1)))))
(: k2 (Kleisli Maybe Integer Integer))
(define k2 (Kleisli (lambda (x) (Some (* x 2)))))

;; --- Category -------------------------------------------------------
(: comp-out (Maybe Integer))
(define comp-out (run-kleisli (comp k2 k1) 10))      ; (10+1)*2 = 22

(: ident-k (Kleisli Maybe Integer Integer))
(define ident-k ident)
(: ident-out (Maybe Integer))
(define ident-out (run-kleisli ident-k 5))

;; --- Arrow ----------------------------------------------------------
(: arred (Kleisli Maybe Integer Integer))
(define arred (arr (lambda (x) (+ x 1))))
(: arr-out (Maybe Integer))
(define arr-out (run-kleisli arred 41))

(: first-out (Maybe (Pair Integer Integer)))
(define first-out (run-kleisli (on-first k1) (Pair 10 99)))

(: split-out (Maybe (Pair Integer Integer)))
(define split-out (run-kleisli (split k1 k2) (Pair 10 20)))   ; (11, 40)

(: fan-out (Maybe (Pair Integer Integer)))
(define fan-out (run-kleisli (fanout k1 k2) 10))              ; (11, 20)

;; --- ArrowChoice ----------------------------------------------------
(: left-l (Maybe (Either Integer Integer)))
(define left-l (run-kleisli (on-left k1) (Left 10)))         ; Left 11
(: left-r (Maybe (Either Integer Integer)))
(define left-r (run-kleisli (on-left k1) (ann (Right 5) (Either Integer Integer))))

(: fanin-l (Maybe Integer))
(define fanin-l (run-kleisli (fanin k1 k2) (ann (Left 10) (Either Integer Integer))))
(: fanin-r (Maybe Integer))
(define fanin-r (run-kleisli (fanin k1 k2) (ann (Right 10) (Either Integer Integer))))

;; --- ArrowApply -----------------------------------------------------
(: app (Kleisli Maybe (Pair (Kleisli Maybe Integer Integer) Integer) Integer))
(define app arrow-app)
(: app-out (Maybe Integer))
(define app-out (run-kleisli app (Pair k1 10)))             ; Some 11

(: suite (List Test))
(define suite
  (list
   (it "Category: comp threads through flatmap"
       (all-checks (list (check-equal? comp-out (Some 22))
                         (check-equal? ident-out (Some 5)))))
   (it "Arrow: arr lifts a pure function"
       (check-equal? arr-out (Some 42)))
   (it "Arrow: first runs on the first component"
       (check-equal? first-out (Some (Pair 11 99))))
   (it "Arrow: *** runs both components"
       (check-equal? split-out (Some (Pair 11 40))))
   (it "Arrow: &&& fans out"
       (check-equal? fan-out (Some (Pair 11 20))))
   (it "ArrowChoice: left routes the Left branch"
       (all-checks (list (check-equal? left-l (Some (Left 11)))
                         (check-equal? left-r (Some (Right 5))))))
   (it "ArrowChoice: ||| merges branches"
       (all-checks (list (check-equal? fanin-l (Some 11))
                         (check-equal? fanin-r (Some 20)))))
   (it "ArrowApply: arrow-app applies a wrapped arrow"
       (check-equal? app-out (Some 11)))))

(: _ran Unit)
(define _ran (run-io (run-suite "rackton/data/kleisli" suite)))

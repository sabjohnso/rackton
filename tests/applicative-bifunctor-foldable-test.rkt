#lang rackton

;; Applicative tier, Bifunctor, Foldable.

(require "../unit.rkt")

;; ----- Applicative fapply over Maybe ---------------------------
(: maybe-ap1 (Maybe Integer))
(define maybe-ap1 (fapply (Some (lambda (x) (+ x 1))) (Some 41)))

(: maybe-ap2 (Maybe Integer))
(define maybe-ap2 (fapply None (Some 41)))

;; ----- Applicative fapply over Result --------------------------
(: result-ap (Result String Integer))
(define result-ap (fapply (Ok (lambda (x) (* x 2))) (Ok 21)))

;; ----- Applicative fapply over List ----------------------------
(: list-ap (List Integer))
(define list-ap
  (fapply (Cons (lambda (x) (+ x 10))
             (Cons (lambda (x) (* x 2)) Nil))
       (Cons 1 (Cons 2 Nil))))

;; ----- liftA2 default over Maybe ----------------------------
(: lifted-add (Maybe Integer))
(define lifted-add
  (liftA2 (lambda (x y) (+ x y)) (Some 3) (Some 4)))

;; ----- Applicative fapply over IO ------------------------------
(: io-ap (IO Integer))
(define io-ap
  (fapply (pure-io (lambda (x) (+ x 100))) (pure-io 5)))

;; ----- Bifunctor bimap over Pair ----------------------------
(: pair-bimapped (Pair Integer String))
(define pair-bimapped
  (bimap (lambda (x) (+ x 1))
         (lambda (s) (string-append s "!"))
         (Pair 41 "hi")))

;; ----- Bifunctor bimap over Result --------------------------
(: result-bimapped (Result Integer Integer))
(define result-bimapped
  (bimap (lambda (e) (string-length e))
         (lambda (v) (* v 10))
         (Ok 7)))

(: result-bimapped-err (Result Integer Integer))
(define result-bimapped-err
  (bimap (lambda (e) (string-length e))
         (lambda (v) (* v 10))
         (Err "oops")))

;; ----- Bifunctor first / second defaults --------------------
(: pair-first (Pair Integer String))
(define pair-first
  (first (lambda (x) (+ x 100)) (Pair 1 "k")))

(: pair-second (Pair Integer String))
(define pair-second
  (second (lambda (s) (string-append s "?")) (Pair 1 "k")))

;; ----- Foldable foldr over List -----------------------------
(: sum-list (-> (List Integer) Integer))
(define (sum-list xs)
  (foldr (lambda (a b) (+ a b)) 0 xs))

(: sum-result Integer)
(define sum-result (sum-list (Cons 1 (Cons 2 (Cons 3 Nil)))))

;; ----- Foldable foldr over Maybe ----------------------------
(: fold-some Integer)
(define fold-some
  (foldr (lambda (a b) (+ a b)) 100 (Some 5)))

(: fold-none Integer)
(define fold-none
  (foldr (lambda (a b) (+ a b)) 100 (ann None (Maybe Integer))))

;; ----- Foldable defaults: length, null?, to-list, sum -------
(: list-len Integer)
(define list-len (length (Cons 1 (Cons 2 (Cons 3 Nil)))))

(: maybe-len Integer)
(define maybe-len (length (Some 99)))

(: maybe-empty-len Integer)
(define maybe-empty-len (length (ann None (Maybe Integer))))

(: list-sum-default Integer)
(define list-sum-default (sum (Cons 10 (Cons 20 Nil))))

(: io-ap-result Integer)
(define io-ap-result (run-io io-ap))

;; ---------- assertions ------------------------------------------

(: suite (List Test))
(define suite
  (list
   (it "Applicative fapply on Maybe"
       (all-checks
        (list (check-equal? maybe-ap1 (Some 42))
              (check-equal? maybe-ap2 None))))
   (it "Applicative fapply on Result"
       (check-equal? result-ap (Ok 42)))
   (it "Applicative fapply on List (cartesian)"
       (check-equal? list-ap
                     (Cons 11 (Cons 12 (Cons 2 (Cons 4 Nil))))))
   (it "Applicative liftA2 on Maybe"
       (check-equal? lifted-add (Some 7)))
   (it "Applicative fapply on IO"
       (check-equal? io-ap-result 105))
   (it "Bifunctor bimap on Pair"
       (check-equal? pair-bimapped (Pair 42 "hi!")))
   (it "Bifunctor bimap on Result Ok"
       (check-equal? result-bimapped (Ok 70)))
   (it "Bifunctor bimap on Result Err"
       (check-equal? result-bimapped-err (Err 4)))
   (it "Bifunctor first on Pair"
       (check-equal? pair-first (Pair 101 "k")))
   (it "Bifunctor second on Pair"
       (check-equal? pair-second (Pair 1 "k?")))
   (it "Foldable foldr on List"
       (check-equal? sum-result 6))
   (it "Foldable foldr on Maybe"
       (all-checks
        (list (check-equal? fold-some 105)
              (check-equal? fold-none 100))))
   (it "Foldable length defaults"
       (all-checks
        (list (check-equal? list-len 3)
              (check-equal? maybe-len 1)
              (check-equal? maybe-empty-len 0))))
   (it "Foldable sum default"
       (check-equal? list-sum-default 30))))

(: _ran Unit)
(define _ran (run-io (run-suite "applicative-bifunctor-foldable" suite)))

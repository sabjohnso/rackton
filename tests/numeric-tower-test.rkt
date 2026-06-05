#lang rackton

;; Numeric tower — Rational + Complex types plus the
;; Integral / Real / Floating / RealFrac / RealFloat class hierarchy.

(require "../unit.rkt")

;; ----- 40.A Rational arithmetic -----------------------------

(: half Rational)
(define half (make-rational 1 2))

(: third Rational)
(define third (make-rational 1 3))

(: five-sixths Rational)
(define five-sixths (+ half third))

(: one-sixth Rational)
(define one-sixth (- half third))

(: half-shown String)
(define half-shown (show half))

;; ----- 40.A' Rational literal syntax (3/4 etc.) -------------

(: q Rational)
(define q 3/4)

(: neg-q Rational)
(define neg-q -3/4)

(: lit-sum Rational)
(define lit-sum (+ 1/4 1/2))

;; A rational literal also works as a match pattern.
(: classify (-> Rational String))
(define (classify r)
  (match r
    [3/4 "three-quarters"]
    [_   "other"]))

(: classified String)
(define classified (classify 3/4))

(: classified-other String)
(define classified-other (classify 1/2))

;; ----- 40.B Complex arithmetic ------------------------------

(: c1 Complex)
(define c1 (make-complex 1.0 2.0))

(: c2 Complex)
(define c2 (make-complex 3.0 4.0))

(: c-sum Complex)
(define c-sum (+ c1 c2))

(: c-prod Complex)
(define c-prod (* c1 c2))

(: c-mag Float)
(define c-mag (magnitude c2))

;; ----- 40.C Integral class ---------------------------------

(: int-div-result Integer)
(define int-div-result (div 17 5))

(: int-mod-result Integer)
(define int-mod-result (mod 17 5))

(: int-quot-result Integer)
(define int-quot-result (quot 17 5))

(: int-rem-result Integer)
(define int-rem-result (rem 17 5))

;; ----- 40.D Real class — to-rational ------------------------

(: from-int Rational)
(define from-int (to-rational 7))

(: from-rat Rational)
(define from-rat (to-rational half))

;; ----- 40.E Floating class — sqrt, pi, exp, log, trig -------

(: root-9 Float)
(define root-9 (sqrt 9.0))

(: pi-shown Float)
(define pi-shown pi)

(: exp-0 Float)
(define exp-0 (exp 0.0))

(: log-e Float)
(define log-e (log 2.718281828459045))

(: sin-0 Float)
(define sin-0 (sin 0.0))

;; ----- 40.F RealFrac class — floor / round / ceiling / truncate ----

(: floored Integer)
(define floored (floor-real 3.7))

(: rounded Integer)
(define rounded (round-real 3.5))

(: ceiled Integer)
(define ceiled (ceiling-real 3.2))

(: truncated Integer)
(define truncated (truncate-real -3.7))

;; ----- 40.G RealFloat class — is-nan? / is-infinite? --------

(: nan-is-nan Boolean)
(define nan-is-nan (is-nan? (float-div 0.0 0.0)))

(: inf-is-inf Boolean)
(define inf-is-inf (is-infinite? (float-div 1.0 0.0)))

(: finite-is-nan Boolean)
(define finite-is-nan (is-nan? 1.0))

(: suite (List Test))
(define suite
  (list
   (it "Rational arithmetic"
       (all-checks
        (list (check-equal? five-sixths (make-rational 5 6))
              (check-equal? one-sixth   (make-rational 1 6))
              (check-equal? half-shown  "1/2"))))
   (it "Rational literal syntax"
       (all-checks
        (list (check-equal? q                (make-rational 3 4))
              (check-equal? neg-q            (make-rational -3 4))
              (check-equal? lit-sum          (make-rational 3 4))
              (check-equal? classified       "three-quarters")
              (check-equal? classified-other "other"))))
   (it "Complex arithmetic"
       (all-checks
        (list (check-equal? c-sum  (make-complex  4.0  6.0))
              (check-equal? c-prod (make-complex -5.0 10.0))
              (check-true   (< (abs-float (- c-mag 5.0)) 0.000001)))))
   (it "Integral class on Integer"
       (all-checks
        (list (check-equal? int-div-result   3)
              (check-equal? int-mod-result   2)
              (check-equal? int-quot-result  3)
              (check-equal? int-rem-result   2))))
   (it "Real class to-rational"
       (all-checks
        (list (check-equal? from-int (make-rational 7 1))
              (check-equal? from-rat half))))
   (it "Floating class basics"
       (all-checks
        (list (check-equal? root-9 3.0)
              (check-true (< (abs-float (- pi-shown 3.141592653589793)) 0.000001))
              (check-equal? exp-0 1.0)
              (check-true (< (abs-float (- log-e 1.0)) 0.000001))
              (check-equal? sin-0 0.0))))
   (it "RealFrac floor/round/ceiling/truncate"
       (all-checks
        (list (check-equal? floored   3)
              (check-equal? rounded   4)
              (check-equal? ceiled    4)
              (check-equal? truncated -3))))
   (it "RealFloat is-nan? / is-infinite?"
       (all-checks
        (list (check-true  nan-is-nan)
              (check-true  inf-is-inf)
              (check-false finite-is-nan))))))

(: _ran Unit)
(define _ran (run-io (run-suite "numeric tower" suite)))

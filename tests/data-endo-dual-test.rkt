#lang rackton

;; rackton/data/monoid — the Endo (endomorphism under composition) and
;; Dual (Semigroup with arguments flipped) monoids.

(require rackton/data/monoid
         "../unit.rkt")

;; --- Endo: <> is composition, mempty is identity ----------------
(: endo-comp Integer)
(define endo-comp
  ((app-endo (<> (Endo (lambda (n) (+ n 1)))
                 (Endo (lambda (n) (* n 2))))) 3))

(: endo-id Integer)
(define endo-id ((app-endo (ann mempty (Endo Integer))) 7))

;; --- Dual: <> flips the inner Semigroup, mempty lifts inner ------
(: dual-flip String)
(define dual-flip (get-dual (<> (Dual "a") (Dual "b"))))

(: dual-mempty String)
(define dual-mempty (get-dual (ann mempty (Dual String))))

(: suite (List Test))
(define suite
  (list
   (it "Endo: composition monoid"
       (all-checks
        (list (check-equal? endo-comp 7)
              (check-equal? endo-id 7))))
   (it "Dual: flipped Semigroup"
       (all-checks
        (list (check-equal? dual-flip "ba")
              (check-equal? dual-mempty ""))))))

(: _ran Unit)
(define _ran (run-io (run-suite "rackton/data/monoid (Endo/Dual)" suite)))

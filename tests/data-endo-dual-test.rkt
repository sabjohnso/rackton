#lang racket/base

;; rackton/data/monoid — the Endo (endomorphism under composition) and
;; Dual (Semigroup with arguments flipped) monoids.

(require rackunit
         "../main.rkt")

(rackton
  (require rackton/data/monoid)

  ;; --- Endo: <> is composition, mempty is identity ----------------
  (: endo-comp Integer)
  (define endo-comp
    ((app-endo (<> (MkEndo (lambda (n) (+ n 1)))
                   (MkEndo (lambda (n) (* n 2))))) 3))

  (: endo-id Integer)
  (define endo-id ((app-endo (ann mempty (Endo Integer))) 7))

  ;; --- Dual: <> flips the inner Semigroup, mempty lifts inner ------
  (: dual-flip String)
  (define dual-flip (get-dual (<> (MkDual "a") (MkDual "b"))))

  (: dual-mempty String)
  (define dual-mempty (get-dual (ann mempty (Dual String)))))

;; ---------- assertions ---------------------------------------

(test-case "Endo: composition monoid"
  ;; (+1) . (*2) applied to 3  =>  3*2 + 1  =>  7
  (check-equal? endo-comp 7)
  ;; identity leaves the argument untouched
  (check-equal? endo-id 7))

(test-case "Dual: flipped Semigroup"
  ;; "a" <> "b" flipped  =>  "b" <> "a"  =>  "ba"
  (check-equal? dual-flip "ba")
  ;; mempty lifts the inner monoid's identity
  (check-equal? dual-mempty ""))

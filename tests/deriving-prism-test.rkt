#lang rackton

;; Auto-derive prisms for data sum types.

(require rackton/data/lens
         "../unit.rkt")

;; ----- Maybe-ish type with Prism deriving --------------
;; Use a local copy (`Opt`) so we don't clash with the prelude
;; Maybe — the test only verifies that Prism deriving emits
;; one prism per ctor.

(data (Opt a)
  Absent
  (Present a)
  #:deriving Prism Eq Show)

(: prev-absent-on-absent (Maybe Unit))
(define prev-absent-on-absent (preview Opt-Absent-prism Absent))

(: prev-absent-on-present (Maybe Unit))
(define prev-absent-on-present
  (preview Opt-Absent-prism (Present 7)))

(: rev-absent (Opt Integer))
(define rev-absent (review Opt-Absent-prism MkUnit))

(: prev-present-on-present (Maybe Integer))
(define prev-present-on-present
  (preview Opt-Present-prism (Present 7)))

(: prev-present-on-absent (Maybe Integer))
(define prev-present-on-absent (preview Opt-Present-prism Absent))

(: rev-present (Opt Integer))
(define rev-present (review Opt-Present-prism 99))

;; ----- Result-like (two unary ctors) -------------------

(data (Either2 e a)
  (Lft e)
  (Rgt a)
  #:deriving Prism Eq Show)

(: prev-lft-on-lft (Maybe String))
(define prev-lft-on-lft (preview Either2-Lft-prism (Lft "err")))

(: prev-rgt-on-rgt (Maybe Integer))
(define prev-rgt-on-rgt (preview Either2-Rgt-prism (Rgt 42)))

(: rev-lft (Either2 String Integer))
(define rev-lft (review Either2-Lft-prism "boom"))

;; ----- mixed-arity skipping ----------------------------
;; ADT mixing a nullary and a single-field ctor — both get prisms.
;; (A 2+-field ctor in a Prism-derived type is a compile error; see
;; tests/deriving-prism-arity-error-test.rkt.)

(data Tri
  Empty
  (One Integer)
  #:deriving Prism Eq Show)

(: prev-empty-on-empty (Maybe Unit))
(define prev-empty-on-empty (preview Tri-Empty-prism Empty))

(: prev-one-on-one (Maybe Integer))
(define prev-one-on-one (preview Tri-One-prism (One 7)))

(: suite (List Test))
(define suite
  (list
   (it "Absent prism: preview / review"
       (all-checks
        (list (check-equal? prev-absent-on-absent   (Some MkUnit))
              (check-equal? prev-absent-on-present  None)
              (check-equal? rev-absent              Absent))))
   (it "Present prism: preview / review"
       (all-checks
        (list (check-equal? prev-present-on-present (Some 7))
              (check-equal? prev-present-on-absent  None)
              (check-equal? rev-present             (Present 99)))))
   (it "Either2 derived prisms"
       (all-checks
        (list (check-equal? prev-lft-on-lft (Some "err"))
              (check-equal? prev-rgt-on-rgt (Some 42))
              (check-equal? rev-lft         (Lft "boom")))))
   (it "nullary + single-field ctors both get prisms"
       (all-checks
        (list (check-equal? prev-empty-on-empty (Some MkUnit))
              (check-equal? prev-one-on-one     (Some 7)))))))

(: _ran Unit)
(define _ran (run-io (run-suite "deriving Prism" suite)))

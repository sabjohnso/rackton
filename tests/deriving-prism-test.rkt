#lang racket/base

;; Auto-derive prisms for data sum types.

(require rackunit
         "../main.rkt")

(rackton
  (require rackton/data/lens)
  ;; ----- Maybe-ish type with Prism deriving --------------
  ;; Use a local copy (`Opt`) so we don't clash with the prelude
  ;; Maybe — the test only verifies that Prism deriving emits
  ;; one prism per ctor.

  (data (Opt a)
    Absent
    (Present a)
    #:deriving Prism)

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
    #:deriving Prism)

  (: prev-lft-on-lft (Maybe String))
  (define prev-lft-on-lft (preview Either2-Lft-prism (Lft "err")))

  (: prev-rgt-on-rgt (Maybe Integer))
  (define prev-rgt-on-rgt (preview Either2-Rgt-prism (Rgt 42)))

  (: rev-lft (Either2 String Integer))
  (define rev-lft (review Either2-Lft-prism "boom"))

  ;; ----- mixed-arity skipping ----------------------------
  ;; ADT with one 0-arg ctor, one 1-arg ctor, one 2-arg ctor.
  ;; Prism deriving emits prisms for the 0/1-arg ones and silently
  ;; skips the 2-arg one.  Test verifies the 0/1-arg prisms exist
  ;; and work; the 2-arg ctor still works as a value but has no
  ;; prism.

  (data Tri
    Empty
    (One Integer)
    (Two Integer Integer)
    #:deriving Prism)

  (: prev-empty-on-empty (Maybe Unit))
  (define prev-empty-on-empty (preview Tri-Empty-prism Empty))

  (: prev-one-on-one (Maybe Integer))
  (define prev-one-on-one (preview Tri-One-prism (One 7)))

  ;; Two-arg ctor still constructs values fine even without prism.
  (: a-two Tri)
  (define a-two (Two 1 2)))

;; ---------- assertions ---------------------------------------

(test-case "Absent prism: preview / review"
  (check-equal? prev-absent-on-absent   (Some MkUnit))
  (check-equal? prev-absent-on-present  None)
  (check-equal? rev-absent              Absent))

(test-case "Present prism: preview / review"
  (check-equal? prev-present-on-present (Some 7))
  (check-equal? prev-present-on-absent  None)
  (check-equal? rev-present             (Present 99)))

(test-case "Either2 derived prisms"
  (check-equal? prev-lft-on-lft (Some "err"))
  (check-equal? prev-rgt-on-rgt (Some 42))
  (check-equal? rev-lft         (Lft "boom")))

(test-case "Mixed-arity: 0/1-arg derived; 2-arg skipped"
  (check-equal? prev-empty-on-empty (Some MkUnit))
  (check-equal? prev-one-on-one     (Some 7))
  (check-equal? a-two               (Two 1 2)))

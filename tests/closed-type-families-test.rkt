#lang racket/base

;; Closed and open standalone type families (Feature 1).
;;
;; A closed family `(type-family (F p…) [pat… = rhs] …)` reduces by ordered,
;; apartness-gated clause matching.  An open family `(type-family (F p…))`
;; extended by standalone `(type-instance (F T…) = U)` equations reduces by
;; coherent single-instance lookup.  Either way the application is rewritten
;; to its right-hand side during unification, so a value of the reduced type
;; type-checks.  A symbolic application stays stuck (rigid).

(require rackunit
         (for-syntax racket/base)
         "../main.rkt")

(define-syntax-rule (compile-error-message form ...)
  (with-handlers ([exn:fail? exn-message])
    (eval #'(rackton form ...)
          (variable-reference->namespace (#%variable-reference)))
    (fail "expected a compile error but the program compiled")))

;; ----- closed family: ordered clause reduction ----------------------

(rackton
  (data PBool PTrue PFalse)

  (type-family (Sel b t e)
    [PTrue  t e = t]
    [PFalse t e = e])

  ;; (Sel PTrue Integer String) reduces to Integer, so 5 : Integer fits.
  (: x (Sel PTrue Integer String))
  (define x 5)

  ;; (Sel PFalse Integer String) reduces to String.
  (: y (Sel PFalse Integer String))
  (define y "hi"))

(test-case "closed family reduces by ordered clause match"
  (check-equal? x 5)
  (check-equal? y "hi"))

;; ----- open family: coherent single-instance reduction --------------

(rackton
  (type-family (Elem c))
  (type-instance (Elem Boolean) = Integer)
  (type-instance (Elem String)  = String)

  (: eb (Elem Boolean))
  (define eb 5)
  (: es (Elem String))
  (define es "hello"))

(test-case "open family reduces by coherent instance lookup"
  (check-equal? eb 5)
  (check-equal? es "hello"))

;; ----- negative: a wrong reduced type is rejected -------------------

(test-case "a value of the wrong reduced type is a compile error"
  ;; (Sel2 PTrue2 Integer String) is Integer, but "oops" is a String.
  (define msg (compile-error-message
               (data PBool2 PTrue2 PFalse2)
               (type-family (Sel2 b t e)
                 [PTrue2  t e = t]
                 [PFalse2 t e = e])
               (: bad (Sel2 PTrue2 Integer String))
               (define bad "oops")))
  (check-true (string? msg)))

;; ----- first-match + catch-all on ground arguments ------------------

(rackton
  (type-family (Choose a)
    [Integer = String]      ; specific clause
    [b       = Integer])    ; catch-all

  (: c1 (Choose Integer))   ; first clause ⇒ String
  (define c1 "hi")
  (: c2 (Choose Boolean))   ; catch-all ⇒ Integer
  (define c2 5))

(test-case "closed family: specific clause wins, catch-all otherwise"
  (check-equal? c1 "hi")
  (check-equal? c2 5))

;; ----- apartness: a symbolic application stays stuck ----------------

(test-case "a symbolic closed family does not fire the catch-all (apartness)"
  ;; If apartness were ignored, `(Choose3 a)` would wrongly reduce to
  ;; Integer (the catch-all), making `(define (f x) x)` type-check.  With
  ;; apartness, `(Choose3 a)` is stuck (Integer clause could still apply
  ;; under a := Integer), so a stuck type cannot unify with Integer.
  (define msg (compile-error-message
               (type-family (Choose3 a)
                 [Integer = String]
                 [b       = Integer])
               (: f (-> (Choose3 a) Integer))
               (define (f x) x)))
  (check-true (string? msg)))

;; ----- open-family coherence: overlapping instances rejected --------

(test-case "open family rejects overlapping type-instance equations"
  (define msg (compile-error-message
               (type-family (Ov a))
               (type-instance (Ov (Pair a b))       = a)
               (type-instance (Ov (Pair Integer b)) = b)))  ; overlaps above
  (check-regexp-match #rx"overlap" msg))

;; ----- the family's kind is inferred from its clauses (Phase 2) ------

;; `Pick`'s first parameter is used as the promoted tag `KT`/`KF`, so its
;; kind is inferred to be `KB`.  Applying `Pick` to `Integer` (kind `*`)
;; in the first position is therefore a KIND error — caught at the use
;; site, before any reduction.
(test-case "an ill-kinded family argument is a kind error"
  (define msg (compile-error-message
               (data KB KT KF)
               (type-family (Pick b t e)
                 [KT t e = t]
                 [KF t e = e])
               (: bad (Pick Integer Integer String))
               (define bad 5)))
  (check-regexp-match #rx"kind" msg))

;; The well-kinded uses above (Sel PTrue …, Elem Boolean …) still compile,
;; which confirms inference assigns the families usable kinds rather than
;; over-constraining them.

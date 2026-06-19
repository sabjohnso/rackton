#lang racket/base

;; Data families (Feature 3): a `data-family` declares a type constructor
;; with no constructors of its own; each `data-instance` introduces fresh
;; constructors whose result type is the family applied to specific
;; arguments (GADT-style), lowered to ordinary structs.  Different
;; instances may use different representations.

(require rackunit
         (for-syntax racket/base)
         "../main.rkt")

(define-syntax-rule (compile-error-message form ...)
  (with-handlers ([exn:fail? exn-message])
    (eval #'(rackton form ...)
          (variable-reference->namespace (#%variable-reference)))
    (fail "expected a compile error but the program compiled")))

;; ----- distinct per-instance representations ------------------------

(rackton
  (data-family (Arr a))
  (data-instance (Arr Boolean) (MkBits Integer))    ; a bitset, as an Integer
  (data-instance (Arr Integer) (MkInts String))     ; a packed string

  ;; Construct a value of each instance — the constructor fixes the index.
  (: bits (Arr Boolean))
  (define bits (MkBits 5))
  (: ints (Arr Integer))
  (define ints (MkInts "1 2 3"))

  ;; Pattern-match at a concrete instance: only that instance's
  ;; constructor is in scope for the match.
  (: popcount (-> (Arr Boolean) Integer))
  (define (popcount a) (match a [(MkBits n) n]))

  (: contents (-> (Arr Integer) String))
  (define (contents a) (match a [(MkInts s) s]))

  (: pc Integer)
  (define pc (popcount bits))
  (: cs String)
  (define cs (contents ints)))

(test-case "data-instance constructors build and match per instance"
  (check-equal? pc 5)
  (check-equal? cs "1 2 3"))

;; ----- the family's kind is inferred from its instances (Phase 2) ---

;; `Val` is indexed by a promoted `Ty2` tag, not by a `*`-kinded type.
;; Its kind (Ty2 -> *) must be inferred from the instance heads, or the
;; index `TI` (kind Ty2) would be rejected against a `*` placeholder.
(rackton
  (data Ty2 TI TB)
  (data-family (Val a))
  (data-instance (Val TI) (VI Integer))
  (data-instance (Val TB) (VB String))

  (: vi (Val TI))
  (define vi (VI 5))
  (: vb (Val TB))
  (define vb (VB "x"))

  (: getI (-> (Val TI) Integer))
  (define (getI v) (match v [(VI n) n]))
  (: gi Integer)
  (define gi (getI vi)))

(test-case "a data family indexed by a promoted kind infers that kind"
  (check-equal? gi 5))

;; ----- an ill-kinded index is rejected ------------------------------

(test-case "an ill-kinded data-family index is a kind error"
  ;; Val3 is fixed to kind (Ty3 -> *) by its instances; using it at
  ;; Integer (kind *) is a kind error.
  (define msg (compile-error-message
               (data Ty3 T3a T3b)
               (data-family (Val3 a))
               (data-instance (Val3 T3a) (V3 Integer))
               (: bad (Val3 Integer))
               (define bad (V3 0))))
  (check-regexp-match #rx"kind" msg))

;; ----- coherence: overlapping instance heads rejected (Phase 3) -----

(test-case "overlapping data-instance heads are rejected"
  (define msg (compile-error-message
               (data-family (DF a))
               (data-instance (DF (Pair a b))       (C1 a))
               (data-instance (DF (Pair Integer b)) (C2 b))))  ; overlaps above
  (check-regexp-match #rx"overlap" msg))

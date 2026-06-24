#lang rackton

;; rackton/effects — typed algebraic effects as an INDEXED (graded) monad.
;;
;; `Eff row a` is a computation producing an `a` while using the effects
;; recorded in `row` (a PHANTOM type index — pure compile-time tracking).
;; `ebind` UNIONS the rows of the two computations, so the type accumulates
;; every effect used; handlers DISCHARGE a label (shrink the row); and
;; `run-eff` is gated on the EMPTY row — so an UNHANDLED effect is a TYPE
;; ERROR, which is the whole point of an effect system.
;;
;; The row is a PRODUCT OF PRESENCE FLAGS — one slot per effect, each
;; `Present` or `Absent` — so `Union` is componentwise through one tiny `Or`
;; family and the encoding scales by adding a slot (no 2^n table).  v1 fixes
;; the alphabet to {Except, Writer}; open/extensible rows are future work.
;;
;; A graded monad's bind changes the row type, so `Eff` is NOT a `Monad`
;; instance and `do`-notation does not apply — chain `ebind` explicitly.
;; `with-except` / `with-writer` widen a row so the branches of an `if` agree.

(provide (data-out Eff)
         (data-out EffRow)
         (data-out Present)
         (data-out Absent)
         epure ebind with-except with-writer throw tell
         handle-except handle-writer run-eff)

;; ===== the effect row: a product of presence flags ====================

(data Present)     ;; an effect is in the row
(data Absent)      ;; an effect is not in the row

;; the row: one slot per effect — (EffRow <except> <writer>)
(data (EffRow ex wr))

;; flag join: Present unless both Absent
(type-family (Or a b)
  [Absent Absent = Absent]
  [a b = Present])

;; row union: componentwise
(type-family (Union r s)
  [(EffRow ex1 wr1) (EffRow ex2 wr2) = (EffRow (Or ex1 ex2) (Or wr1 wr2))])

;; discharge one label: set its slot to Absent
(type-family (DropExcept r) [(EffRow ex wr) = (EffRow Absent wr)])
(type-family (DropWriter r) [(EffRow ex wr) = (EffRow ex Absent)])

;; the empty row (no effects) and the two singletons, as readable aliases
(define-alias (NoFx)     (EffRow Absent  Absent))
(define-alias (ExceptFx) (EffRow Present Absent))
(define-alias (WriterFx) (EffRow Absent  Present))

;; ===== the indexed monad ==============================================
;; Sealed: the only way to obtain an `Eff` is the operations below, so the
;; row is always an honest record of the effects actually used.  Internal
;; rep: a result-or-error plus a String log.

(data (Eff row a) (MkEff (Pair (Either String a) (List String))) #:abstract)

(: run-rep (-> (Eff row a) (Pair (Either String a) (List String))))
(define (run-rep e) (match e [(MkEff p) p]))

(: epure (-> a (Eff NoFx a)))
(define (epure x) (MkEff (Pair (Right x) Nil)))

(: ebind (-> (Eff r a) (-> a (Eff s b)) (Eff (Union r s) b)))
(define (ebind e k)
  (match (run-rep e)
    [(Pair res log1)
     (match res
       [(Left err) (MkEff (Pair (Left err) log1))]          ;; Except short-circuits
       [(Right x)  (match (run-rep (k x))
                     [(Pair res2 log2) (MkEff (Pair res2 (append log1 log2)))])])]))

;; Widen a computation's row by ADDING one (unused) effect — SOUND because
;; the row is an upper bound, so claiming an effect you don't use is just
;; conservative.  The runtime is the identity; only the row type grows.
;; Per-effect (not a general `Union r s`) so the target row is a concrete
;; `EffRow` the unifier can solve — needed to make the two branches of an
;; `if` agree (e.g. a `throw` branch and a pure branch).
(: with-except (-> (Eff (EffRow Absent wr) a) (Eff (EffRow Present wr) a)))
(define (with-except e) (match e [(MkEff p) (MkEff p)]))
(: with-writer (-> (Eff (EffRow ex Absent) a) (Eff (EffRow ex Present) a)))
(define (with-writer e) (match e [(MkEff p) (MkEff p)]))

;; ===== the effect operations (smart constructors set the row) =========
(: throw (-> String (Eff ExceptFx a)))
(define (throw msg) (MkEff (Pair (Left msg) Nil)))

(: tell (-> String (Eff WriterFx Unit)))
(define (tell s) (MkEff (Pair (Right Unit) (Cons s Nil))))

;; ===== handlers: DISCHARGE a label (shrink the row) ===================
;; `handle-except` makes the result total — it can no longer throw, and the
;; Either becomes the value.  `handle-writer` moves the accumulated log into
;; the value and resets it.

(: handle-except (-> (Eff r a) (Eff (DropExcept r) (Either String a))))
(define (handle-except e)
  (match (run-rep e) [(Pair res log) (MkEff (Pair (Right res) log))]))

(: handle-writer (-> (Eff r a) (Eff (DropWriter r) (Pair a (List String)))))
(define (handle-writer e)
  (match (run-rep e)
    [(Pair res log)
     (match res
       [(Right x)  (MkEff (Pair (Right (Pair x log)) Nil))]
       [(Left err) (MkEff (Pair (Left err) Nil))])]))   ;; Except still open: propagate

;; ===== run: ONLY an empty-row computation may run =====================
(: run-eff (-> (Eff NoFx a) a))
(define (run-eff e)
  (match (run-rep e)
    [(Pair (Right x) _) x]
    [(Pair (Left _) _)  (panic "run-eff: unreachable — empty row cannot throw")]))

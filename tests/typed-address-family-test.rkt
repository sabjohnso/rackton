#lang racket/base

;; Finite, ground-lookup Γ for non-looping typed assembly (TypedAssembly.org,
;; Approach A): the per-address stack/shape table Γ is an associated type
;; keyed on the LITERAL instruction address.  `(ShapeAt 0)` reduces to the
;; shape at address 0, etc.  Because the IP does not loop, every address that
;; appears is a literal, so Γ is only ever applied to ground arguments — it
;; never needs symbolic/recursive reduction.
;;
;; The payoff: at each literal address the stack shape is statically fixed, so
;; the per-address dispatch is TOTAL — the off-shape constructor is excluded by
;; the type, with no `panic` arm.

(require rackunit
         "../main.rkt")

(rackton
  ;; Γ : the per-address shape table, as an associated type keyed on the
  ;; literal address.  One instance per address (the "generated" table).
  ;; The shapes are ordinary `*`-kinded types (here Integer / Boolean stand
  ;; in for two distinct stack shapes), since a family result is kind `*`.
  (protocol (CodeAt (n :: Nat))
    (:type ShapeAt))
  (instance (CodeAt 0) (:type (ShapeAt = Integer)))
  (instance (CodeAt 1) (:type (ShapeAt = Boolean)))

  ;; A value indexed by its shape (stands in for the typed stack).
  (data (Cell s)
    (CellA : (Cell Integer))
    (CellB : (Cell Boolean)))

  ;; Finite, non-looping instruction-pointer evidence: addresses 0 and 1.
  (data (IP n)
    (IP0 : (IP 0))
    (IP1 : (IP 1)))

  ;; At address n the cell must have shape (ShapeAt n).  Matching the IP
  ;; refines n to a literal, after which (ShapeAt n) reduces to the concrete
  ;; shape — so each inner cell-match is TOTAL (one arm; the other ctor is
  ;; ruled out by its shape index).
  (: fetch (-> (IP n) (Cell (ShapeAt n)) Boolean))
  (define (fetch ip c)
    (match ip
      [(IP0) (match c [(CellA) #t])]
      [(IP1) (match c [(CellB) #f])]))

  (: r0 Boolean) (define r0 (fetch IP0 CellA))
  (: r1 Boolean) (define r1 (fetch IP1 CellB)))

(test-case "a ground-lookup Γ makes per-address dispatch total"
  (check-equal? r0 #t)
  (check-equal? r1 #f))

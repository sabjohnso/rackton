#lang racket/base

;; Phase 54: polymorphic record updates.
;;
;; `(update RECORD [field expr] ...)` returns a new record with the
;; named fields replaced by the corresponding expressions.  The type
;; of the result matches the type of the original record.  Unknown
;; fields and ill-typed update values are rejected at compile time.

(require rackunit
         "../main.rkt")

(rackton
  (define-struct Point
    [x : Integer]
    [y : Integer])

  ;; ----- 54.A single-field update ----------------------------
  (: shift-x Point)
  (define shift-x (update (Point 1 2) [x 99]))

  ;; ----- 54.B multi-field update -----------------------------
  (: move Point)
  (define move (update (Point 1 2) [x 99] [y 88]))

  ;; ----- 54.C parametric record ------------------------------
  (define-struct (Box a)
    [value : a]
    [tag   : String])

  (: r-box1 (Box Integer))
  (define r-box1 (update (Box 7 "old") [value 99]))

  (: r-box2 (Box Integer))
  (define r-box2 (update (Box 7 "old") [tag "new"]))

  ;; ----- helpers ---------------------------------------------
  (: point-eq? (-> Point (-> Integer (-> Integer Boolean))))
  (define (point-eq? p a b)
    (and (= (Point-x p) a) (= (Point-y p) b)))

  (: box-int-eq? (-> (Box Integer) (-> Integer (-> String Boolean))))
  (define (box-int-eq? b v t)
    (and (= (Box-value b) v) (== (Box-tag b) t))))

(test-case "single-field update"
  (check-true (point-eq? shift-x 99 2)))

(test-case "multi-field update"
  (check-true (point-eq? move 99 88)))

(test-case "parametric record: update value field"
  (check-true (box-int-eq? r-box1 99 "old")))

(test-case "parametric record: update tag field"
  (check-true (box-int-eq? r-box2 7 "new")))

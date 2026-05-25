#lang racket/base

;; Lenses / optics library.  Simple (getter, setter)
;; encoding — `Lens s a` packs the projection and re-injection
;; needed to focus on a sub-part of a structure.

(require rackunit
         "../main.rkt")

(rackton
  (define-struct Point
    [x : Integer]
    [y : Integer])

  ;; Hand-written field lenses for Point.
  (: x-lens (Lens Point Integer))
  (define x-lens
    (MkLens (lambda (p) (Point-x p))
            (lambda (p v) (Point v (Point-y p)))))

  (: y-lens (Lens Point Integer))
  (define y-lens
    (MkLens (lambda (p) (Point-y p))
            (lambda (p v) (Point (Point-x p) v))))

  (: p0 Point)
  (define p0 (Point 3 7))

  ;; view / set / over on flat record.
  (: x-val   Integer)
  (define x-val   (view x-lens p0))

  (: p-set-x Point)
  (define p-set-x (set x-lens 99 p0))

  (: p-bump-y Point)
  (define p-bump-y (over y-lens (lambda (n) (+ n 1)) p0))

  ;; Nested: a Segment with two endpoints.  Compose lenses to
  ;; reach the start-point's x.
  (define-struct Segment
    [start  : Point]
    [end    : Point])

  (: start-lens (Lens Segment Point))
  (define start-lens
    (MkLens (lambda (s) (Segment-start s))
            (lambda (s v) (Segment v (Segment-end s)))))

  (: end-lens (Lens Segment Point))
  (define end-lens
    (MkLens (lambda (s) (Segment-end s))
            (lambda (s v) (Segment (Segment-start s) v))))

  (: start-x-lens (Lens Segment Integer))
  (define start-x-lens (lens-compose start-lens x-lens))

  (: seg Segment)
  (define seg (Segment (Point 1 2) (Point 10 20)))

  (: seg-start-x Integer)
  (define seg-start-x (view start-x-lens seg))

  (: seg-shifted Segment)
  (define seg-shifted (set start-x-lens 42 seg))

  (: seg-over    Segment)
  (define seg-over (over start-x-lens (lambda (n) (* n 100)) seg)))

;; ---------- assertions ---------------------------------------

(test-case "view returns the focused field"
  (check-equal? x-val 3))

(test-case "set replaces the focused field"
  (check-equal? p-set-x  (Point 99 7))
  (check-equal? (Point-x p-set-x) 99)
  (check-equal? (Point-y p-set-x) 7))

(test-case "over transforms the focused field"
  (check-equal? p-bump-y (Point 3 8)))

(test-case "composed lens views through nesting"
  (check-equal? seg-start-x 1))

(test-case "composed lens sets only the focused position"
  (check-equal? seg-shifted (Segment (Point 42 2) (Point 10 20))))

(test-case "composed lens over transforms only the focused position"
  (check-equal? seg-over (Segment (Point 100 2) (Point 10 20))))

#lang rackton

;; Auto-derive field lenses for struct via
;; `#:deriving Lens`.  Each field `f` of struct `T` gets a named
;; `T-f-lens` definition.

(require rackton/data/lens
         "../unit.rkt")

;; ----- 47.A flat struct ------------------------------------

(struct Point
  [x : Integer]
  [y : Integer]
  #:deriving Eq Show Lens)

(: p0 Point)
(define p0 (Point 3 7))

(: x-val Integer)
(define x-val (view Point-x-lens p0))

(: y-val Integer)
(define y-val (view Point-y-lens p0))

(: p-set-x Point)
(define p-set-x (set Point-x-lens 99 p0))

(: p-bump-y Point)
(define p-bump-y (over Point-y-lens (lambda (n) (+ n 1)) p0))

;; ----- 47.B parametric struct -------------------------------

(struct (Box a)
  [value : a]
  #:deriving Eq Show Lens)

(: b0 (Box String))
(define b0 (Box "hi"))

(: b-val String)
(define b-val (view Box-value-lens b0))

(: b-set (Box String))
(define b-set (set Box-value-lens "hello" b0))

;; ----- 47.C composed lenses through nesting ------------------

(struct Segment
  [start : Point]
  [end   : Point]
  #:deriving Eq Show Lens)

(: start-x-lens (Lens Segment Integer))
(define start-x-lens (lens-compose Segment-start-lens Point-x-lens))

(: seg Segment)
(define seg (Segment (Point 1 2) (Point 10 20)))

(: seg-start-x Integer)
(define seg-start-x (view start-x-lens seg))

(: seg-shifted Segment)
(define seg-shifted (set start-x-lens 42 seg))

(: seg-doubled Segment)
(define seg-doubled
  (over start-x-lens (lambda (n) (* n 2)) seg))

(: suite (List Test))
(define suite
  (list
   (it "derived lenses view flat fields"
       (all-checks
        (list (check-equal? x-val 3)
              (check-equal? y-val 7))))
   (it "derived lenses set + over"
       (all-checks
        (list (check-equal? p-set-x  (Point 99 7))
              (check-equal? p-bump-y (Point 3 8)))))
   (it "derived parametric lens"
       (all-checks
        (list (check-equal? b-val "hi")
              (check-equal? b-set (Box "hello")))))
   (it "composed derived lenses through nesting"
       (all-checks
        (list (check-equal? seg-start-x 1)
              (check-equal? seg-shifted (Segment (Point 42 2) (Point 10 20)))
              (check-equal? seg-doubled (Segment (Point 2 2)  (Point 10 20))))))))

(: main Unit)
(define main (run-io (run-suite "deriving Lens" suite)))

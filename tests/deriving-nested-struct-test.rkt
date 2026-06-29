#lang rackton

;; Regression: deriving Eq / Ord / Show on a struct whose fields are
;; themselves structs that derive the same classes.  This used to crash
;; at use with `match: no matching clause for #<procedure:$==:Point>`:
;; the field type's Eq impl (a dict) leaked into the outer derived
;; method's match subject because the synthesized method references
;; shared a syntax object with the value references, and method/dict
;; resolution is keyed by that syntax object.

(require "../unit.rkt")

(struct Point   [x : Integer] [y : Integer] :deriving Eq Ord Show)
(struct Segment [start : Point] [end : Point] :deriving Eq Ord Show)

(: s1 Segment) (define s1 (Segment (Point 1 2) (Point 3 4)))
(: s2 Segment) (define s2 (Segment (Point 1 2) (Point 3 4)))
(: s3 Segment) (define s3 (Segment (Point 1 2) (Point 9 9)))

(: suite (List Test))
(define suite
  (list
   (it "== over nested-struct fields"
       (all-checks (list (check-true  (== s1 s2))
                         (check-false (== s1 s3)))))
   (it "< lexicographic over nested-struct fields"
       (all-checks (list (check-true  (< s1 s3))
                         (check-false (< s3 s1)))))
   (it "show over nested-struct fields"
       (check-equal? (show s1) "(Segment (Point 1 2) (Point 3 4))"))
   (it "field access is intact (not a leaked dict)"
       (check-equal? (Point-x (Segment-start s1)) 1))))

(: main Unit)
(define main (run-io (run-suite "deriving nested struct" suite)))

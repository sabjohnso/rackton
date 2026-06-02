#lang rackton

;; rackton/data/map — Data.Map parity additions.  Maps are opaque, so
;; results are observed via lookup / size / member?.

(require rackton/data/map
         "../unit.rkt")

(: m (Map Integer String))
(define m (map-from-list (list (Pair 1 "a") (Pair 2 "b"))))

(: mem1 Boolean) (define mem1 (map-member? 1 m))
(: mem3 Boolean) (define mem3 (map-member? 3 m))
(: emp0 Boolean) (define emp0 (map-empty? (ann empty-map (Map Integer String))))
(: emp1 Boolean) (define emp1 (map-empty? m))
(: sz   Integer) (define sz (map-size m))
(: tolen Integer) (define tolen (length (map-to-list m)))

(: single (Maybe String))
(define single (map-lookup 5 (map-singleton 5 "z")))

(: fwd-hit  String) (define fwd-hit  (map-find-with-default "d" 2 m))
(: fwd-miss String) (define fwd-miss (map-find-with-default "d" 9 m))

(: adj (Maybe String))
(define adj (map-lookup 1 (map-adjust (lambda (v) (<> v "!")) 1 m)))
(: iw (Maybe String))
(define iw (map-lookup 1 (map-insert-with (lambda (new old) (<> new old)) 1 "x" m)))

(: uni (Maybe String))
(define uni (map-lookup 1 (map-union (map-singleton 1 "L") (map-singleton 1 "R"))))
(: uniw (Maybe String))
(define uniw (map-lookup 1 (map-union-with (lambda (a b) (<> a b))
                                           (map-singleton 1 "L") (map-singleton 1 "R"))))

(: diff1 Boolean) (define diff1 (map-member? 1 (map-difference m (map-singleton 1 "z"))))
(: diff2 Boolean) (define diff2 (map-member? 2 (map-difference m (map-singleton 1 "z"))))

(: isect (Maybe Integer))
(define isect
  (map-lookup 2 (map-intersection-with (lambda (a b) (+ a b))
                                       (map-from-list (list (Pair 1 10) (Pair 2 20)))
                                       (map-from-list (list (Pair 2 200) (Pair 3 300))))))

(: mm (Maybe Integer))
(define mm (map-lookup 1 (map-map (lambda (v) (* v 2)) (map-from-list (list (Pair 1 10))))))
(: mmk (Maybe Integer))
(define mmk (map-lookup 1 (map-map-with-key (lambda (k v) (+ k v))
                                            (map-from-list (list (Pair 1 10))))))

(: filt1 Boolean)
(define filt1 (map-member? 1 (map-filter (lambda (v) (> v 1))
                                         (map-from-list (list (Pair 1 0) (Pair 2 5))))))
(: filt2 Boolean)
(define filt2 (map-member? 2 (map-filter (lambda (v) (> v 1))
                                         (map-from-list (list (Pair 1 0) (Pair 2 5))))))
(: filtk Boolean)
(define filtk (map-member? 1 (map-filter-with-key (lambda (k v) (> k 1))
                                                  (map-from-list (list (Pair 1 5) (Pair 2 5))))))

(: suite (List Test))
(define suite
  (list
   (it "membership / size"
       (all-checks
        (list (check-equal? mem1 #t) (check-equal? mem3 #f)
              (check-equal? emp0 #t) (check-equal? emp1 #f)
              (check-equal? sz 2) (check-equal? tolen 2))))
   (it "singleton / find-with-default"
       (all-checks
        (list (check-equal? single (Some "z"))
              (check-equal? fwd-hit "b") (check-equal? fwd-miss "d"))))
   (it "adjust / insert-with"
       (all-checks
        (list (check-equal? adj (Some "a!"))
              (check-equal? iw  (Some "xa")))))
   (it "union / union-with"
       (all-checks
        (list (check-equal? uni  (Some "L"))
              (check-equal? uniw (Some "LR")))))
   (it "difference / intersection"
       (all-checks
        (list (check-equal? diff1 #f) (check-equal? diff2 #t)
              (check-equal? isect (Some 220)))))
   (it "map / filter"
       (all-checks
        (list (check-equal? mm  (Some 20))
              (check-equal? mmk (Some 11))
              (check-equal? filt1 #f) (check-equal? filt2 #t)
              (check-equal? filtk #f))))))

(: _ran Unit)
(define _ran (run-io (run-suite "rackton/data/map" suite)))

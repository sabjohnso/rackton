#lang rackton

;; rackton/data/set — Data.Set parity additions.  Sets are opaque, so
;; results are observed via member? / size.

(require rackton/data/set
         "../unit.rkt")

(: emp0 Boolean) (define emp0 (set-empty? (ann empty-set (Set Integer))))
(: emp1 Boolean) (define emp1 (set-empty? (set-singleton 1)))

(: s12 (Set Integer)) (define s12 (set-from-list (list 1 2 2 1)))
(: sz Integer)        (define sz (set-size s12))

(: u-mem Boolean) (define u-mem (set-member? 3 (set-union (set-from-list (list 1 2))
                                                          (set-from-list (list 2 3)))))
(: u-sz  Integer) (define u-sz (set-size (set-union (set-from-list (list 1 2))
                                                    (set-from-list (list 2 3)))))
(: i-mem2 Boolean) (define i-mem2 (set-member? 2 (set-intersection (set-from-list (list 1 2))
                                                                   (set-from-list (list 2 3)))))
(: i-mem1 Boolean) (define i-mem1 (set-member? 1 (set-intersection (set-from-list (list 1 2))
                                                                   (set-from-list (list 2 3)))))
(: d-mem1 Boolean) (define d-mem1 (set-member? 1 (set-difference (set-from-list (list 1 2))
                                                                 (set-from-list (list 2 3)))))
(: d-mem2 Boolean) (define d-mem2 (set-member? 2 (set-difference (set-from-list (list 1 2))
                                                                 (set-from-list (list 2 3)))))

(: sub-yes Boolean) (define sub-yes (set-subset? (set-from-list (list 1 2))
                                                 (set-from-list (list 1 2 3))))
(: sub-no Boolean)  (define sub-no  (set-subset? (set-from-list (list 1 4))
                                                 (set-from-list (list 1 2 3))))
(: disj-yes Boolean) (define disj-yes (set-disjoint? (set-from-list (list 1 2))
                                                     (set-from-list (list 3 4))))
(: disj-no Boolean)  (define disj-no  (set-disjoint? (set-from-list (list 1 2))
                                                     (set-from-list (list 2 3))))

(: map-mem Boolean) (define map-mem (set-member? 20 (set-map (lambda (x) (* x 10))
                                                             (set-from-list (list 1 2)))))
(: filt-mem1 Boolean) (define filt-mem1 (set-member? 1 (set-filter (lambda (x) (> x 1))
                                                                   (set-from-list (list 1 2 3)))))
(: filt-mem2 Boolean) (define filt-mem2 (set-member? 2 (set-filter (lambda (x) (> x 1))
                                                                   (set-from-list (list 1 2 3)))))
(: fold-sum Integer) (define fold-sum (set-foldr (lambda (x acc) (+ x acc)) 0
                                                 (set-from-list (list 1 2 3))))

(: suite (List Test))
(define suite
  (list
   (it "empty / from-list dedup"
       (all-checks
        (list (check-equal? emp0 #t) (check-equal? emp1 #f)
              (check-equal? sz 2))))
   (it "union / intersection / difference"
       (all-checks
        (list (check-equal? u-mem #t) (check-equal? u-sz 3)
              (check-equal? i-mem2 #t) (check-equal? i-mem1 #f)
              (check-equal? d-mem1 #t) (check-equal? d-mem2 #f))))
   (it "subset / disjoint"
       (all-checks
        (list (check-equal? sub-yes #t) (check-equal? sub-no #f)
              (check-equal? disj-yes #t) (check-equal? disj-no #f))))
   (it "map / filter / foldr"
       (all-checks
        (list (check-equal? map-mem #t)
              (check-equal? filt-mem1 #f) (check-equal? filt-mem2 #t)
              (check-equal? fold-sum 6))))))

(: main Unit)
(define main (run-io (run-suite "rackton/data/set" suite)))

#lang rackton

;; rackton/data/monoid (All/Any) + rackton/data/semigroup
;; (Min/Max/First/Last).

(require rackton/data/monoid
         rackton/data/semigroup
         "../unit.rkt")

;; All / Any via mappend
(: all-tt Boolean) (define all-tt (get-all (mappend (MkAll #t) (MkAll #t))))
(: all-tf Boolean) (define all-tf (get-all (mappend (MkAll #t) (MkAll #f))))
(: any-ft Boolean) (define any-ft (get-any (mappend (Any #f) (Any #t))))
(: any-ff Boolean) (define any-ff (get-any (mappend (Any #f) (Any #f))))

;; All / Any via mconcat (mempty resolves at the element type)
(: all-mc  Boolean) (define all-mc  (get-all (mconcat (list (MkAll #t) (MkAll #t) (MkAll #f)))))
(: all-mc0 Boolean) (define all-mc0 (get-all (mconcat (ann Nil (List All)))))
(: any-mc0 Boolean) (define any-mc0 (get-any (mconcat (ann Nil (List Any)))))

;; Min / Max / First / Last
(: mn Integer) (define mn (get-min   (mappend (Min 3) (Min 7))))
(: mx Integer) (define mx (get-max   (mappend (Max 3) (Max 7))))
(: ft Integer) (define ft (get-first (mappend (First 1) (First 2))))
(: lt Integer) (define lt (get-last  (mappend (Last 1) (Last 2))))

(: suite (List Test))
(define suite
  (list
   (it "All / Any"
       (all-checks
        (list (check-equal? all-tt #t) (check-equal? all-tf #f)
              (check-equal? any-ft #t) (check-equal? any-ff #f)
              (check-equal? all-mc #f)
              (check-equal? all-mc0 #t)
              (check-equal? any-mc0 #f))))
   (it "Min / Max / First / Last"
       (all-checks
        (list (check-equal? mn 3) (check-equal? mx 7)
              (check-equal? ft 1) (check-equal? lt 2))))))

(: main Unit)
(define main (run-io (run-suite "rackton/data/monoid+semigroup" suite)))

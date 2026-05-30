#lang racket/base

;; rackton/data/monoid (All/Any) + rackton/data/semigroup
;; (Min/Max/First/Last).

(require rackunit
         "../main.rkt")

(rackton
  (require rackton/data/monoid)
  (require rackton/data/semigroup)

  ;; All / Any via <>
  (: all-tt Boolean) (define all-tt (get-all (<> (MkAll #t) (MkAll #t))))
  (: all-tf Boolean) (define all-tf (get-all (<> (MkAll #t) (MkAll #f))))
  (: any-ft Boolean) (define any-ft (get-any (<> (MkAny #f) (MkAny #t))))
  (: any-ff Boolean) (define any-ff (get-any (<> (MkAny #f) (MkAny #f))))

  ;; All / Any via mconcat (mempty resolves at the element type)
  (: all-mc  Boolean) (define all-mc  (get-all (mconcat (list (MkAll #t) (MkAll #t) (MkAll #f)))))
  (: all-mc0 Boolean) (define all-mc0 (get-all (mconcat (ann Nil (List All)))))
  (: any-mc0 Boolean) (define any-mc0 (get-any (mconcat (ann Nil (List Any)))))

  ;; Min / Max / First / Last
  (: mn Integer) (define mn (get-min   (<> (MkMin 3) (MkMin 7))))
  (: mx Integer) (define mx (get-max   (<> (MkMax 3) (MkMax 7))))
  (: ft Integer) (define ft (get-first (<> (MkFirst 1) (MkFirst 2))))
  (: lt Integer) (define lt (get-last  (<> (MkLast 1) (MkLast 2)))))

;; ---------- assertions ---------------------------------------

(test-case "All / Any"
  (check-equal? all-tt #t) (check-equal? all-tf #f)
  (check-equal? any-ft #t) (check-equal? any-ff #f)
  (check-equal? all-mc #f)
  (check-equal? all-mc0 #t)      ; mempty All = #t
  (check-equal? any-mc0 #f))     ; mempty Any = #f

(test-case "Min / Max / First / Last"
  (check-equal? mn 3) (check-equal? mx 7)
  (check-equal? ft 1) (check-equal? lt 2))

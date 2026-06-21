#lang rackton

;; rackton/data/list/nonempty — NonEmpty list accessors and operations.

(require rackton/data/list/nonempty
         "../unit.rkt")

(: ne (NonEmpty Integer)) (define ne (nonempty 1 (list 2 3)))
(: h Integer)        (define h (ne-head ne))
(: t (List Integer)) (define t (ne-tail ne))
(: tl (List Integer))(define tl (ne-to-list ne))
(: ln Integer)       (define ln (ne-length ne))
(: consed (List Integer)) (define consed (ne-to-list (ne-cons 0 ne)))
(: mapped (List Integer)) (define mapped (ne-to-list (ne-map (lambda (x) (* x 10)) ne)))
(: ffl  (Maybe Integer))
(define ffl  (match (ne-from-list (list 4 5)) [(Some n) (Some (ne-head n))] [(None) None]))
(: ffl0 (Maybe Integer))
(define ffl0 (match (ne-from-list (ann Nil (List Integer))) [(Some n) (Some (ne-head n))] [(None) None]))

(: suite (List Test))
(define suite
  (list
   (it "NonEmpty"
       (all-checks
        (list (check-equal? h 1)
              (check-equal? t (list 2 3))
              (check-equal? tl (list 1 2 3))
              (check-equal? ln 3)
              (check-equal? consed (list 0 1 2 3))
              (check-equal? mapped (list 10 20 30))
              (check-equal? ffl  (Some 4))
              (check-equal? ffl0 None))))))

(: main Unit)
(define main (run-io (run-suite "rackton/data/list/nonempty" suite)))

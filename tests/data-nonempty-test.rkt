#lang racket/base
(require rackunit "../main.rkt")

(rackton
  (require rackton/data/list/nonempty)
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
  (define ffl0 (match (ne-from-list (ann Nil (List Integer))) [(Some n) (Some (ne-head n))] [(None) None])))

(define (lst . xs) (let loop ([xs xs]) (if (null? xs) Nil (Cons (car xs) (loop (cdr xs))))))

(test-case "NonEmpty"
  (check-equal? h 1)
  (check-equal? t (lst 2 3))
  (check-equal? tl (lst 1 2 3))
  (check-equal? ln 3)
  (check-equal? consed (lst 0 1 2 3))
  (check-equal? mapped (lst 10 20 30))
  (check-equal? ffl  (Some 4))
  (check-equal? ffl0 None))

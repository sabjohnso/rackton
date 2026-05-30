#lang racket/base

;; rackton/data/maybe — Data.Maybe parity additions.

(require rackunit
         "../main.rkt")

(rackton
  (require rackton/data/maybe)

  (: r-cat (List Integer))
  (define r-cat (cat-maybes (list (Some 1) None (Some 2) None (Some 3))))
  (: r-mapm (List Integer))
  (define r-mapm
    (map-maybe (lambda (x) (if (> x 2) (Some (* x 10)) None)) (list 1 2 3 4)))
  (: r-tolist  (List Integer)) (define r-tolist  (maybe->list (Some 7)))
  (: r-tolist0 (List Integer)) (define r-tolist0 (maybe->list (ann None (Maybe Integer))))
  (: r-tomaybe  (Maybe Integer)) (define r-tomaybe  (list->maybe (list 9 8 7)))
  (: r-tomaybe0 (Maybe Integer)) (define r-tomaybe0 (list->maybe (ann Nil (List Integer))))
  (: r-fromjust Integer) (define r-fromjust (from-just (Some 42)))
  ;; existing helpers still present
  (: r-maybe Integer)    (define r-maybe (maybe 0 (lambda (x) (+ x 1)) (Some 5)))
  (: r-isjust Boolean)   (define r-isjust (is-just (Some 1))))

;; ---------- assertions ---------------------------------------

(test-case "catMaybes / mapMaybe"
  (check-equal? r-cat  (Cons 1 (Cons 2 (Cons 3 Nil))))
  (check-equal? r-mapm (Cons 30 (Cons 40 Nil))))

(test-case "list interop"
  (check-equal? r-tolist   (Cons 7 Nil))
  (check-equal? r-tolist0  Nil)
  (check-equal? r-tomaybe  (Some 9))
  (check-equal? r-tomaybe0 None)
  (check-equal? r-fromjust 42))

(test-case "existing helpers"
  (check-equal? r-maybe 6)
  (check-equal? r-isjust #t))

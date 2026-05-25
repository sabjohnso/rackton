#lang rackton

;; (data-out T) — exports the type T plus every constructor.
;; `helper` is intentionally not exported.

(define-data (Box a) (MkBox a))

(: helper (-> Integer Integer))
(define (helper n) (+ n 1))

(provide (data-out Box))

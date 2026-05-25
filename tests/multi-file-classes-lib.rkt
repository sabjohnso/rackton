#lang rackton

;; A Rackton library that defines a Container class and an instance for
;; a user-declared Stack type.

(provide (all-defined-out))

(define-data (Stack a) Empty (Push a (Stack a)))

(define-class (Container (f :: (-> * *)))
  (: empty? (-> (f a) Boolean))
  (: size   (-> (f a) Integer)))

(define-instance (Container Stack)
  (define (empty? s)
    (match s
      [(Empty)      #t]
      [(Push _ _)   #f]))
  (define (size s)
    (match s
      [(Empty)        0]
      [(Push _ rest)  (+ 1 (size rest))])))

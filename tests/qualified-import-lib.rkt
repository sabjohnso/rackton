#lang rackton

;; Fixture for qualified-import-test.rkt: a library whose names are meant
;; to be imported under a colon prefix via `(qualified-in …)`.

(provide (data-out Stack) depth)

(data (Stack a)
  Empty
  (Push a (Stack a)))

(: depth (-> (Stack a) Integer))
(define (depth s)
  (match s
    [Empty          0]
    [(Push _ rest)  (+ 1 (depth rest))]))

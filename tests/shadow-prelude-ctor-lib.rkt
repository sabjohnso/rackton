#lang rackton

;; A module that locally REDEFINES a prelude constructor name (`Cons`) as
;; a constructor of its own type.  The type sidecar must publish this
;; `Cons`, or importers fall back to the prelude's `Cons : a -> List a ->
;; List a` and mis-type every use of the imported constructor.

(provide
 (data-out Nonempty-List)
 ne-length)

(data (Nonempty-List a)
  (Sole a)
  (Cons a (Nonempty-List a)))

(: ne-length (-> (Nonempty-List a) Integer))
(define (ne-length xs)
  (match xs
    [(Sole _)    1]
    [(Cons _ ys) (+ 1 (ne-length ys))]))

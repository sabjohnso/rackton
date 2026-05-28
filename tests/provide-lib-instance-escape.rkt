#lang rackton

;; Instances always escape, even when the (provide ...) form makes no
;; mention of them.  Here we export only the Color type and its
;; constructors; the (Eq Color) instance still has to cross the
;; module boundary so a client can call `==` on Color values.

(data Color  Red  Green  Blue)

(instance (Eq Color)
  (define (== a b)
    (match a
      [Red   (match b [Red   #t] [_ #f])]
      [Green (match b [Green #t] [_ #f])]
      [Blue  (match b [Blue  #t] [_ #f])])))

(provide (data-out Color))

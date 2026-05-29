#lang rackton

;; Enabler: an opaque (data T) whose runtime values come from a host
;; struct, declaring the runtime dispatch tag so an instance's positional
;; methods register under the SAME tag the runtime carries.

(provide (all-defined-out))

(data (Widget a) #:runtime-tag $widget)

(foreign make-widget (-> a (Widget a)) #:from "opaque-tag-rt.rkt")
(foreign widget-get  (-> (Widget a) a) #:from "opaque-tag-rt.rkt")

(instance (Functor Widget)
  (define (fmap f w) (make-widget (f (widget-get w)))))

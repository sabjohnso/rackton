#lang racket/base

;; Companion runtime for the opaque-dispatch-tag enabler test: Widget
;; values are `$widget` structs, so (dispatch-tag w) is '$widget.

(provide make-widget widget-get)

(struct $widget (v) #:transparent)
(define (make-widget v) ($widget v))
(define (widget-get w) ($widget-v w))

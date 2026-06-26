#lang rackton

;; Fixture for require-subforms-cross-module-test.rkt: a small library
;; whose bindings are imported through the require sub-forms (only-in,
;; rename-in, prefix-in, except-in).

(provide greet answer shout)

(: greet (-> String String))
(define (greet s) (string-append "hi " s))

(: answer Integer)
(define answer 42)

(: shout (-> String String))
(define (shout s) (string-append s "!"))

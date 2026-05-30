#lang rackton

;; rackton/data/bool — Data.Bool.  (not / and / or are in the prelude.)

(provide (all-defined-out))

;; (bool f t cond) — t when cond is #t, f otherwise (Haskell's `bool`,
;; argument order false-then-true).
(: bool (-> a (-> a (-> Boolean a))))
(define (bool f t cond) (if cond t f))

;; Always #t — reads well as the final guard alternative.
(: otherwise Boolean)
(define otherwise #t)

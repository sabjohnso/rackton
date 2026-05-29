#lang rackton

;; rackton/data/maybe — Data.Maybe.  Additive helpers over the prelude's
;; Maybe type.  Phase-0 spike doubling as the first real stdlib module:
;; proves an additive module type-checks, runs, and imports by
;; collection path (rackton/data/maybe).

(provide (all-defined-out))

;; Eliminator: (maybe default f m) — f applied to the Some payload, or
;; the default when None.
(: maybe (-> b (-> (-> a b) (-> (Maybe a) b))))
(define (maybe d f m)
  (match m
    [(None)   d]
    [(Some x) (f x)]))

(: is-just (-> (Maybe a) Boolean))
(define (is-just m) (match m [(None) #f] [(Some _) #t]))

(: is-nothing (-> (Maybe a) Boolean))
(define (is-nothing m) (match m [(None) #t] [(Some _) #f]))

(: from-maybe (-> a (-> (Maybe a) a)))
(define (from-maybe d m)
  (match m
    [(None)   d]
    [(Some x) x]))

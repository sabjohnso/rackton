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

;; from-just: the payload, or panic on None (Haskell's @tt{fromJust}).
;; Prefer @racket[from-maybe] / @racket[maybe] when a total result is
;; possible.
(: from-just (-> (Maybe a) a))
(define (from-just m)
  (match m
    [(Some x) x]
    [(None)   (panic "from-just: None")]))

;; mapMaybe: map and keep only the @racket[Some] results.
(: map-maybe (-> (-> a (Maybe b)) (-> (List a) (List b))))
(define (map-maybe f xs)
  (match xs
    [(Nil)      Nil]
    [(Cons h t) (match (f h)
                  [(Some b) (Cons b (map-maybe f t))]
                  [(None)   (map-maybe f t)])]))

;; catMaybes: drop the @racket[None]s.
(: cat-maybes (-> (List (Maybe a)) (List a)))
(define (cat-maybes ms) (map-maybe (lambda (m) m) ms))

;; maybeToList / listToMaybe.
(: maybe->list (-> (Maybe a) (List a)))
(define (maybe->list m) (match m [(None) Nil] [(Some x) (Cons x Nil)]))

(: list->maybe (-> (List a) (Maybe a)))
(define (list->maybe xs) (match xs [(Nil) None] [(Cons h _) (Some h)]))

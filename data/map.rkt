#lang rackton

;; rackton/data/map — Data.Map.  Immutable key/value maps, moved out of
;; the auto-prelude (Phase 2 slim).  The runtime lives in
;; private/containers-runtime (Racket immutable hashes) and is reached
;; via `foreign`; keys compare by structural equality.

(provide (all-defined-out))

(data (Map k v))

(foreign empty-map (Map k v)
         #:from rackton/private/containers-runtime)
(foreign map-insert (-> k (-> v (-> (Map k v) (Map k v))))
         #:from rackton/private/containers-runtime)
(foreign map-lookup (-> k (-> (Map k v) (Maybe v)))
         #:from rackton/private/containers-runtime)
(foreign map-delete (-> k (-> (Map k v) (Map k v)))
         #:from rackton/private/containers-runtime)
(foreign map-keys (-> (Map k v) (List k))
         #:from rackton/private/containers-runtime)
(foreign map-values (-> (Map k v) (List v))
         #:from rackton/private/containers-runtime)
(foreign map-size (-> (Map k v) Integer)
         #:from rackton/private/containers-runtime)
(foreign map-fold (-> (-> k (-> v (-> b b))) (-> b (-> (Map k v) b)))
         #:from rackton/private/containers-runtime)

;; group-by — bucket a list by a key function (pure Rackton over the
;; foreign map ops).
(: group-by (-> (-> a k) (-> (List a) (Map k (List a)))))
(define (group-by key xs)
  (foldr (lambda (x m)
           (let ([k (key x)])
             (match (map-lookup k m)
               [(None)     (map-insert k (Cons x Nil) m)]
               [(Some lst) (map-insert k (Cons x lst) m)])))
         empty-map
         xs))

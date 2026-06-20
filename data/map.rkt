#lang rackton

;; rackton/data/map — Data.Map.  Immutable key/value maps.  The `Map`
;; type and the constructor primitives (empty-map / map-insert) are
;; promoted into the prelude (so the {..} literal needs no import); this
;; module adds the rest of Data.Map.  The derived runtime lives in
;; private/containers-runtime (Racket immutable hashes) and is reached
;; via `foreign`; keys compare by structural equality.

(provide (all-defined-out))

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

;; ===== Data.Map parity =============================================
;;
;; All pure Rackton over the foreign primitives above.  Keys compare by
;; the runtime's structural equality (no @racket[(Eq k)] constraint).

(: map-member? (-> k (-> (Map k v) Boolean)))
(define (map-member? k m) (match (map-lookup k m) [(Some _) #t] [(None) #f]))

(: map-empty? (-> (Map k v) Boolean))
(define (map-empty? m) (== (map-size m) 0))

(: map-singleton (-> k (-> v (Map k v))))
(define (map-singleton k v) (map-insert k v empty-map))

(: map-elems (-> (Map k v) (List v)))
(define (map-elems m) (map-values m))

(: map-to-list (-> (Map k v) (List (Pair k v))))
(define (map-to-list m)
  (map-fold (lambda (k v acc) (Cons (Pair k v) acc)) Nil m))

(: map-from-list (-> (List (Pair k v)) (Map k v)))
(define (map-from-list ps)
  (foldr (lambda (p m) (map-insert (fst p) (snd p) m)) empty-map ps))

(: map-find-with-default (-> v (-> k (-> (Map k v) v))))
(define (map-find-with-default d k m)
  (match (map-lookup k m) [(Some v) v] [(None) d]))

;; apply f to the value at k, if present.
(: map-adjust (-> (-> v v) (-> k (-> (Map k v) (Map k v)))))
(define (map-adjust f k m)
  (match (map-lookup k m)
    [(Some v) (map-insert k (f v) m)]
    [(None)   m]))

;; insert v at k, or @racket[(f v old)] when k is already present.
(: map-insert-with (-> (-> v (-> v v)) (-> k (-> v (-> (Map k v) (Map k v))))))
(define (map-insert-with f k v m)
  (match (map-lookup k m)
    [(Some old) (map-insert k (f v old) m)]
    [(None)     (map-insert k v m)]))

;; left-biased union (values from the first map win on shared keys).
(: map-union (-> (Map k v) (-> (Map k v) (Map k v))))
(define (map-union m1 m2)
  (map-fold (lambda (k v acc) (map-insert k v acc)) m2 m1))

(: map-union-with (-> (-> v (-> v v)) (-> (Map k v) (-> (Map k v) (Map k v)))))
(define (map-union-with f m1 m2)
  (map-fold (lambda (k v acc) (map-insert-with f k v acc)) m2 m1))

;; keys of m1 not present in m2 removed.
(: map-difference (-> (Map k v) (-> (Map k w) (Map k v))))
(define (map-difference m1 m2)
  (foldr (lambda (k acc) (map-delete k acc)) m1 (map-keys m2)))

(: map-intersection-with (-> (-> v (-> w x)) (-> (Map k v) (-> (Map k w) (Map k x)))))
(define (map-intersection-with f m1 m2)
  (map-fold (lambda (k v acc)
              (match (map-lookup k m2)
                [(Some v2) (map-insert k (f v v2) acc)]
                [(None)    acc]))
            empty-map m1))

(: map-map (-> (-> v w) (-> (Map k v) (Map k w))))
(define (map-map f m)
  (map-fold (lambda (k v acc) (map-insert k (f v) acc)) empty-map m))

(: map-map-with-key (-> (-> k (-> v w)) (-> (Map k v) (Map k w))))
(define (map-map-with-key f m)
  (map-fold (lambda (k v acc) (map-insert k (f k v) acc)) empty-map m))

(: map-filter (-> (-> v Boolean) (-> (Map k v) (Map k v))))
(define (map-filter p m)
  (map-fold (lambda (k v acc) (if (p v) (map-insert k v acc) acc)) empty-map m))

(: map-filter-with-key (-> (-> k (-> v Boolean)) (-> (Map k v) (Map k v))))
(define (map-filter-with-key p m)
  (map-fold (lambda (k v acc) (if (p k v) (map-insert k v acc) acc)) empty-map m))

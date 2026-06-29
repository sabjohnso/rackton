#lang racket/base

;; Derived runtime ops for rackton/data/map and rackton/data/set.
;;
;; The constructor primitives ($map / $set + empty-map / map-insert /
;; empty-set / set-insert) are promoted into the prelude and live in
;; private/prelude-runtime.rkt so the {..} / #{..} literals (and bare
;; Map/Set use) need no import.  This module imports those and derives
;; the rest of Data.Map / Data.Set (lookup, delete, fold, …), which the
;; rackton/data/map + rackton/data/set surfaces `foreign`-import.
;;
;; Backed by Racket's immutable hashes (so keys compare by `equal?`,
;; which lines up with Rackton structural ==; no Eq dict is threaded).

(require (only-in "prelude-runtime.rkt"
                  Some None Cons Nil
                  $map $map-h $set $set-h
                  empty-map map-insert empty-set set-insert)
         (only-in "dict.rkt" define/curried))

;; Re-export the promoted primitives too, so any `:from
;; containers-runtime` reference to them keeps resolving.
(provide empty-map map-insert map-lookup map-delete
         map-keys map-values map-size map-fold
         empty-set set-insert set-member? set-delete set-size set-to-list)

(define/curried (map-lookup k m)
  (cond [(hash-has-key? ($map-h m) k) (Some (hash-ref ($map-h m) k))]
        [else None]))
(define/curried (map-delete k m) ($map (hash-remove ($map-h m) k)))

;; Build a rackton List from a Racket sequence.
(define (rkt-seq->list xs)
  (let loop ([xs (reverse xs)] [acc Nil])
    (cond [(null? xs) acc]
          [else (loop (cdr xs) (Cons (car xs) acc))])))

(define (map-keys   m) (rkt-seq->list (hash-keys   ($map-h m))))
(define (map-values m) (rkt-seq->list (hash-values ($map-h m))))
(define (map-size   m) (hash-count ($map-h m)))

(define/curried (map-fold f z m)
  (for/fold ([acc z]) ([(k v) (in-hash ($map-h m))])
    (((f k) v) acc)))

(define/curried (set-member? x s) (hash-has-key? ($set-h s) x))
(define/curried (set-delete x s) ($set (hash-remove ($set-h s) x)))
(define (set-size s) (hash-count ($set-h s)))
(define (set-to-list s) (rkt-seq->list (hash-keys ($set-h s))))

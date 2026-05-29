#lang racket/base

;; Runtime impls for rackton/data/map and rackton/data/set.
;;
;; Moved out of prelude-runtime (Phase 2 slim) so Map / Set are no longer
;; auto-available: rackton/data/map and rackton/data/set `foreign`-import
;; these with Rackton types.  This is the companion-runtime pattern for
;; slimming runtime-backed content onto `foreign` — the impls stay
;; hand-written Racket, the typed surface lives in a #lang rackton module.
;;
;; Backed by Racket's immutable hashes (so keys compare by `equal?`,
;; which lines up with Rackton structural ==; no Eq dict is threaded).

(require (only-in "prelude-runtime.rkt" Some None Cons Nil)
         (only-in "dict.rkt" define/curried))

(provide empty-map map-insert map-lookup map-delete
         map-keys map-values map-size map-fold
         empty-set set-insert set-member? set-delete set-size set-to-list)

(struct $map (h) #:transparent)
(struct $set (h) #:transparent)

(define empty-map ($map (hash)))
(define empty-set ($set (hash)))

(define/curried (map-insert k v m) ($map (hash-set ($map-h m) k v)))
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

(define/curried (set-insert x s) ($set (hash-set ($set-h s) x #t)))
(define/curried (set-member? x s) (hash-has-key? ($set-h s) x))
(define/curried (set-delete x s) ($set (hash-remove ($set-h s) x)))
(define (set-size s) (hash-count ($set-h s)))
(define (set-to-list s) (rkt-seq->list (hash-keys ($set-h s))))

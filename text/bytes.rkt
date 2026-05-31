#lang rackton

;; rackton/text/bytes — derived Bytes operations (Data.ByteString-style).
;; The prelude ships the @racket[Bytes] type with @racket[bytes-length]
;; / @racket[bytes-ref] / @racket[bytes-append] / @racket[bytes->list] /
;; @racket[list->bytes] / @racket[string->bytes] / @racket[bytes->string]
;; / @racket[make-bytes]; these are the derived combinators, expressed
;; over the list round-trip and data/list's take/drop.

(require rackton/data/list)
(provide (all-defined-out))

;; the empty byte string.
(: bytes-empty Bytes)
(define bytes-empty (list->bytes Nil))

;; is the byte string empty?
(: bytes-null? (-> Bytes Boolean))
(define (bytes-null? b) (== (bytes-length b) 0))

;; the first n bytes.
(: bytes-take (-> Integer (-> Bytes Bytes)))
(define (bytes-take n b) (list->bytes (take n (bytes->list b))))

;; all but the first n bytes.
(: bytes-drop (-> Integer (-> Bytes Bytes)))
(define (bytes-drop n b) (list->bytes (drop n (bytes->list b))))

;; split into (first n, rest) — Haskell's splitAt.
(: bytes-split (-> Integer (-> Bytes (Pair Bytes Bytes))))
(define (bytes-split n b) (MkPair (bytes-take n b) (bytes-drop n b)))

;; concatenate a list of byte strings.
(: bytes-concat (-> (List Bytes) Bytes))
(define (bytes-concat bss) (foldr bytes-append bytes-empty bss))

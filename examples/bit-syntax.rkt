#lang rackton

;; bit-syntax.rkt — Erlang-style bit syntax in Rackton.
;;
;; The `bits` form builds and pattern-matches binary data segment by
;; segment, bit by bit.  Widths are bit-granular: a 4-bit field packs
;; against its neighbour with no byte boundary between them.  The same
;; form reads and writes, and a segment's width may be governed by a
;; value matched earlier in the same binary (a length prefix).
;;
;; Run it with `racket examples/bit-syntax.rkt`.

(require rackton/text/string)   ; string-append*

;; ----- A sub-byte header ------------------------------------------
;; The first two bytes of an IPv4 header: a 4-bit version, a 4-bit
;; header length, and an 8-bit type-of-service — 16 bits, packed.

(data Header (Header Integer Integer Integer))   ; version, ihl, tos

(: make-header (-> Integer (-> Integer (-> Integer Bitstring))))
(define (make-header version ihl tos)
  (bits [version 4] [ihl 4] [tos 8]))

(: parse-header (-> Bitstring (Maybe Header)))
(define (parse-header b)
  (match b
    [(bits [version 4] [ihl 4] [tos 8]) (Some (Header version ihl tos))]
    [_ None]))

(: header->string (-> Header String))
(define (header->string h)
  (match h
    [(Header version ihl tos)
     (string-append* "version=" (show version)
                     " ihl=" (show ihl)
                     " tos=" (show tos))]))

;; ----- A length-prefixed frame ------------------------------------
;; An 8-bit length, then exactly that many payload bytes.  On the way
;; back out, `len` (matched first) sizes the `payload` segment.

(: frame (-> Bytes Bitstring))
(define (frame payload)
  (bits [(bytes-length payload) 8] [payload _ binary]))

(: unframe (-> Bitstring (Maybe Bytes)))
(define (unframe b)
  (match b
    [(bits [len 8] [payload len binary]) (Some payload)]
    [_ None]))

;; ----- rendering helpers ------------------------------------------

(: show-bytes (-> (Maybe Bytes) String))
(define (show-bytes mb)
  (match mb
    [(Some b) (show b)]
    [None     "<<not byte-aligned>>"]))

(: show-payload (-> (Maybe Bytes) String))
(define (show-payload mb)
  (match mb
    [(Some b) (match (bytes->string b)
                [(Some s) s]
                [None     (show b)])]
    [None     "<<short frame>>"]))

(: report-header (-> Bitstring (IO Unit)))
(define (report-header b)
  (do [_ <- (println (string-append* "  bytes:  " (show-bytes (bitstring->bytes b))))]
    [_ <- (println (string-append* "  bits:   " (show (bitstring-length b))))]
    (match (parse-header b)
      [(Some h) (println (string-append* "  parsed: " (header->string h)))]
      [None     (println "  parsed: <<malformed>>")])))

;; ----- main -------------------------------------------------------

(: main (IO Unit))
(define main (do [_ <- (println "Erlang-style bit syntax:")]
               [_ <- (println "")]
               [_ <- (println "A 4+4+8-bit header (version 4, ihl 5, tos 0):")]
               [_ <- (report-header (make-header 4 5 0))]
               [_ <- (println "")]
               [_ <- (println "Length-prefixed frame round-trip:")]
               [_ <- (println (string-append* "  unframed: "
                                              (show-payload (unframe (frame (string->bytes "hello"))))))]
               (println "done.")))

#lang racket/base

;; Erlang-style bit syntax — phase 1: bit-level `bits` construction and
;; pattern matching over the `Bitstring` type.  See BitSyntax.org.
;;
;; Covers: byte-aligned and sub-byte construction, sub-byte field
;; extraction, variable-width (length-governed) segments, signed
;; integers, binary / bitstring tails, fall-through on short input, and
;; the bitstring-length / bytes->bitstring / bitstring->bytes API.

(require rackunit
         "../main.rkt")

(rackton

  ;; ---- construction -------------------------------------------------

  ;; Byte-aligned: 4-bit version + 4-bit IHL + 8-bit TOS = two bytes.
  (: hdr (-> Integer (Maybe Bytes)))
  (define (hdr tos) (bitstring->bytes (bits [4 4] [5 4] [tos 8])))

  ;; Sub-byte total that is still a whole byte: one packed byte.
  (: one-byte (-> Integer (-> Integer (Maybe Bytes))))
  (define (one-byte flag code)
    (bitstring->bytes (bits [flag 1] [code 7])))

  ;; A non-byte-aligned total has no byte image.
  (: three-bits (Maybe Bytes))
  (define three-bits (bitstring->bytes (bits [5 3])))

  (: three-bits-len Integer)
  (define three-bits-len (bitstring-length (bits [5 3])))

  ;; ---- matching: sub-byte fields ------------------------------------

  (: split-byte (-> Bitstring (Maybe (Pair Integer Integer))))
  (define (split-byte b)
    (match b
      [(bits [hi 4] [lo 4]) (Some (Pair hi lo))]
      [_ None]))

  ;; ---- matching: variable width governed by an earlier field --------

  (: unframe (-> Bitstring (Maybe Bytes)))
  (define (unframe b)
    (match b
      [(bits [len 8] [payload len binary]) (Some payload)]
      [_ None]))

  ;; ---- matching: signed integer -------------------------------------

  (: read-i8 (-> Bitstring (Maybe Integer)))
  (define (read-i8 b)
    (match b
      [(bits [n 8 signed]) (Some n)]
      [_ None]))

  ;; ---- matching: bitstring tail (non-byte-aligned remainder) --------

  (: tail-bits (-> Bitstring (Maybe Integer)))
  (define (tail-bits b)
    (match b
      [(bits [_ 5] [rest _ bitstring]) (Some (bitstring-length rest))]
      [_ None]))

  ;; ---- matching: exact-consumption / fall-through -------------------

  ;; The pattern wants exactly 8 bits; a longer or shorter input falls
  ;; through to None.
  (: exactly-8 (-> Bitstring (Maybe Integer)))
  (define (exactly-8 b)
    (match b
      [(bits [n 8]) (Some n)]
      [_ None]))

  ;; ---- round-trip ---------------------------------------------------

  (: roundtrip (-> Integer (-> Integer (Maybe (Pair Integer Integer)))))
  (define (roundtrip a b)
    (split-byte (bits [a 4] [b 4])))

  ;; A 3-bit value (not constructible from Racket code, since `bits` is a
  ;; Rackton form) used to exercise the too-short fall-through path.
  (: short-3 Bitstring)
  (define short-3 (bits [1 3])))

;; ====================================================================

(test-case "construction: byte-aligned header"
  ;; 0x4 0x5 0x00 -> bytes 0x45 0x00 = #"E\0"
  (check-equal? (hdr 0)   (Some #"E\0"))
  (check-equal? (hdr 255) (Some (bytes 69 255))))

(test-case "construction: sub-byte packed into one byte"
  ;; flag=1 (msb), code=3 -> 1000_0011 = 0x83 = 131
  (check-equal? ((one-byte 1) 3) (Some (bytes 131)))
  (check-equal? ((one-byte 0) 5) (Some (bytes 5))))

(test-case "construction: non-byte-aligned has no byte image but has length"
  (check-equal? three-bits None)
  (check-equal? three-bits-len 3))

(test-case "match: sub-byte field extraction"
  (check-equal? (split-byte (bytes->bitstring #"E")) (Some (Pair 4 5)))
  (check-equal? (split-byte (bytes->bitstring (bytes 131))) (Some (Pair 8 3))))

(test-case "match: variable width governed by earlier field"
  ;; len=3, then 3 payload bytes "abc", trailing byte ignored? no — exact.
  (check-equal? (unframe (bytes->bitstring (bytes 3 97 98 99))) (Some #"abc"))
  ;; len says 5 but only 2 bytes follow -> fall through
  (check-equal? (unframe (bytes->bitstring (bytes 5 97 98))) None))

(test-case "match: signed integer"
  (check-equal? (read-i8 (bytes->bitstring (bytes 255))) (Some -1))
  (check-equal? (read-i8 (bytes->bitstring (bytes 127))) (Some 127))
  (check-equal? (read-i8 (bytes->bitstring (bytes 128))) (Some -128)))

(test-case "match: bitstring tail keeps non-byte-aligned remainder"
  ;; 8 bits in, drop 5, remainder is 3 bits
  (check-equal? (tail-bits (bytes->bitstring #"A")) (Some 3)))

(test-case "match: exact consumption / fall-through"
  (check-equal? (exactly-8 (bytes->bitstring (bytes 42))) (Some 42))
  ;; 16 bits, pattern wants exactly 8 -> None
  (check-equal? (exactly-8 (bytes->bitstring (bytes 42 7))) None)
  ;; 3 bits, too short -> None
  (check-equal? (exactly-8 short-3) None))

(test-case "round-trip: build then destructure is identity"
  (check-equal? ((roundtrip 7) 9) (Some (Pair 7 9)))
  (check-equal? ((roundtrip 0) 0) (Some (Pair 0 0)))
  (check-equal? ((roundtrip 15) 15) (Some (Pair 15 15))))

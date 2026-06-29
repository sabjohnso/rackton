#lang racket/base

;; Rackton — Bitstring runtime (the pure bit algebra).
;;
;; A `Bitstring` is an arbitrary-length sequence of bits, the runtime
;; value behind the surface `bits` form (see BitSyntax.org).  It is
;; represented as an exact natural plus a bit length:
;;
;;   (struct bitstring (value len))   value : 0 ≤ value < 2^len
;;
;; with the FIRST-written segment in the MOST-significant bits
;; (big-endian bit order, matching Erlang's default).  Every operation
;; below is pure integer arithmetic, so this is the simple reference
;; implementation; a buffer-backed representation can replace it behind
;; this same interface if profiling ever demands it.
;;
;; This module depends on nothing but `racket/base`, so both the
;; pattern compiler (private/match.rkt, via for-template) and the
;; prelude runtime can require it without a cycle.  The codegen-emitted
;; reader closure for a `bits` pattern references the leaf operations
;; here directly.

(provide (struct-out bitstring)
         empty-bitstring
         bitstring-concat
         int->bitstring
         bytes->bitstring
         bitstring->bytes-exact
         bitstring-read
         bitstring-slice)

;; #:prefab so a value built in one module's namespace is recognised as
;; the same struct type in another (codegen emits references resolved in
;; codegen's own context — see private/repl-codegen-helpers note).
(struct bitstring (value len) #:prefab)

(define empty-bitstring (bitstring 0 0))

;; Low-`width`-bits mask.
(define (mask width) (- (arithmetic-shift 1 width) 1))

;; Append: a's bits occupy the high end, b's the low end.
(define (bitstring-concat a b)
  (bitstring (+ (arithmetic-shift (bitstring-value a) (bitstring-len b))
                (bitstring-value b))
             (+ (bitstring-len a) (bitstring-len b))))

;; A `width`-bit integer segment.  Takes the low `width` bits of v; for a
;; negative v this is exactly its two's-complement encoding (e.g. -1 at
;; width 8 → 255), so signed and unsigned share one path.
(define (int->bitstring v width)
  (bitstring (bitwise-and v (mask width)) width))

;; A byte string becomes a byte-aligned bitstring, MSB-first.
(define (bytes->bitstring b)
  (let loop ([i 0] [acc 0])
    (if (= i (bytes-length b))
        (bitstring acc (* 8 (bytes-length b)))
        (loop (+ i 1) (+ (arithmetic-shift acc 8) (bytes-ref b i))))))

;; The byte image of a bitstring, or #f when its length is not a whole
;; number of bytes.  (len = 0 → the empty byte string.)
(define (bitstring->bytes-exact bs)
  (define len (bitstring-len bs))
  (cond
    [(not (zero? (modulo len 8))) #f]
    [else
     (define n (quotient len 8))
     (define v (bitstring-value bs))
     (define out (make-bytes n))
     (let loop ([i 0])
       (cond
         [(= i n) out]
         [else
          (bytes-set! out i
                      (bitwise-and (arithmetic-shift v (- (* 8 (- n 1 i)))) #xff))
          (loop (+ i 1))]))]))

;; Read `width` bits at bit offset `off` (from the MSB) as an integer.
;; When `signed?` and the top bit is set, interpret as two's complement.
;; The caller guarantees off + width ≤ (bitstring-len bs).
(define (bitstring-read bs off width signed?)
  (define len (bitstring-len bs))
  (define raw (bitwise-and (arithmetic-shift (bitstring-value bs)
                                             (- (- len (+ off width))))
                           (mask width)))
  (if (and signed?
           (positive? width)
           (bitwise-bit-set? raw (- width 1)))
      (- raw (arithmetic-shift 1 width))
      raw))

;; Slice `width` bits at bit offset `off` (from the MSB) into a fresh
;; bitstring.  The caller guarantees off + width ≤ (bitstring-len bs).
(define (bitstring-slice bs off width)
  (define len (bitstring-len bs))
  (bitstring (bitwise-and (arithmetic-shift (bitstring-value bs)
                                            (- (- len (+ off width))))
                          (mask width))
             width))

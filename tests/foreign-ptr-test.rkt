#lang rackton

;; rackton/foreign/ptr — Foreign.Ptr / Foreign.Marshal core: opaque Ptr,
;; raw allocation, typed peek/poke, pointer arithmetic, C strings.
;; UNSAFE by design (manual free, no bounds checks) — mirrors Haskell's
;; Foreign.

(require rackton/foreign/ptr
         "../unit.rkt")

;; int write/read round-trip
(: int-rt (IO Integer))
(define int-rt
  (do [p <- (malloc-bytes size-of-int)]
    [_ <- (poke-int p 4242)]
    [v <- (peek-int p)]
    [_ <- (free-ptr p)]
    (pure v)))

;; pointer arithmetic: a two-int buffer
(: arith (IO Integer))
(define arith
  (do [p <- (malloc-bytes (* 2 size-of-int))]
    [_ <- (poke-int p 10)]
    [_ <- (poke-int (ptr-plus p size-of-int) 20)]
    [a <- (peek-int p)]
    [b <- (peek-int (ptr-plus p size-of-int))]
    [_ <- (free-ptr p)]
    (pure (+ a b))))

;; double round-trip
(: dbl-rt (IO Float))
(define dbl-rt
  (do [p <- (malloc-bytes size-of-double)]
    [_ <- (poke-double p 3.5)]
    [v <- (peek-double p)]
    [_ <- (free-ptr p)]
    (pure v)))

;; single-byte round-trip
(: byte-rt (IO Integer))
(define byte-rt
  (do [p <- (malloc-bytes 1)]
    [_ <- (poke-byte p 200)]
    [v <- (peek-byte p)]
    [_ <- (free-ptr p)]
    (pure v)))

;; NULL pointer
(: is-null Boolean) (define is-null (ptr-null? null-ptr))

;; C string round-trip
(: cstr-rt (IO String))
(define cstr-rt
  (do [p <- (string->c-string "hello")]
    [s <- (c-string->string p)]
    [_ <- (free-ptr p)]
    (pure s)))

;; ---------- assertions ---------------------------------------

(: suite (List Test))
(define suite
  (list
    (it "int / arithmetic"
        (all-checks
          (list (check-equal? (run-io int-rt) 4242)
                (check-equal? (run-io arith) 30))))
    (it "double / byte"
        (all-checks
          (list (check-true (< (abs (- (run-io dbl-rt) 3.5)) 1e-9))
                (check-equal? (run-io byte-rt) 200))))
    (it "null pointer"
        (check-true is-null))
    (it "C string round-trip"
        (check-equal? (run-io cstr-rt) "hello"))))

(: test-main (IO Unit))
(define test-main (run-suite "foreign-ptr" suite))

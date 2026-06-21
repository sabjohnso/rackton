#lang rackton

;; rackton/data/bits — bitwise operations over Integer (Data.Bits).

(require rackton/data/bits
         "../unit.rkt")

(: r-and Integer) (define r-and (bit-and 12 10))
(: r-or  Integer) (define r-or  (bit-or  12 10))
(: r-xor Integer) (define r-xor (bit-xor 12 10))
(: r-not Integer) (define r-not (bit-not 0))

(: r-shl Integer) (define r-shl (bit-shift-left  1 4))
(: r-shr Integer) (define r-shr (bit-shift-right 16 2))

(: r-test-t Boolean) (define r-test-t (bit-test 4 2))
(: r-test-f Boolean) (define r-test-f (bit-test 4 0))

(: r-set   Integer) (define r-set   (bit-set 0 3))
(: r-clear Integer) (define r-clear (bit-clear 15 0))

(: r-count Integer) (define r-count (bit-count 7))
(: r-count0 Integer)(define r-count0 (bit-count 0))

;; ---------- assertions ---------------------------------------

(: suite (List Test))
(define suite
  (list
   (it "logical ops"
       (all-checks
        (list (check-equal? r-and 8)    ; 1100 & 1010 = 1000
              (check-equal? r-or  14)   ; 1100 | 1010 = 1110
              (check-equal? r-xor 6)    ; 1100 ^ 1010 = 0110
              (check-equal? r-not -1)))) ; complement of 0 (two's complement)
   (it "shifts"
       (all-checks
        (list (check-equal? r-shl 16)
              (check-equal? r-shr 4))))
   (it "test / set / clear"
       (all-checks
        (list (check-true  r-test-t)
              (check-false r-test-f)
              (check-equal? r-set 8)     ; set bit 3 of 0
              (check-equal? r-clear 14)))) ; clear bit 0 of 1111
   (it "popcount"
       (all-checks
        (list (check-equal? r-count 3)
              (check-equal? r-count0 0))))))

(: main Unit)
(define main (run-io (run-suite "data-bits" suite)))

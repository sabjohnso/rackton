#lang rackton

;; rackton/numeric/show — Numeric radix conversions: show/read in
;; hex, octal, binary, decimal.

(require rackton/numeric/show
         "../unit.rkt")

(: h-255 String) (define h-255 (num-show-hex 255))
(: o-8   String) (define o-8   (num-show-oct 8))
(: b-5   String) (define b-5   (num-show-bin 5))

(: rh-ok  Integer) (define rh-ok  (match (num-read-hex "ff")  [(Some n) n] [(None) -1]))
(: rh-bad Integer) (define rh-bad (match (num-read-hex "zz")  [(Some n) n] [(None) -1]))
(: ro-ok  Integer) (define ro-ok  (match (num-read-oct "10")  [(Some n) n] [(None) -1]))
(: ro-bad Integer) (define ro-bad (match (num-read-oct "9")   [(Some n) n] [(None) -1]))
(: rd-ok  Integer) (define rd-ok  (match (num-read-dec "42")  [(Some n) n] [(None) -1]))
(: rd-bad Integer) (define rd-bad (match (num-read-dec "x")   [(Some n) n] [(None) -1]))

(: suite (List Test))
(define suite
  (list
   (it "show in radix"
       (all-checks
        (list (check-equal? h-255 "ff")
              (check-equal? o-8   "10")
              (check-equal? b-5   "101"))))
   (it "read in radix (valid)"
       (all-checks
        (list (check-equal? rh-ok 255)
              (check-equal? ro-ok 8)
              (check-equal? rd-ok 42))))
   (it "read in radix (invalid -> None)"
       (all-checks
        (list (check-equal? rh-bad -1)
              (check-equal? ro-bad -1)
              (check-equal? rd-bad -1))))))

(: _ran Unit)
(define _ran (run-io (run-suite "rackton/numeric/show" suite)))

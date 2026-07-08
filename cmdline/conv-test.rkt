#lang rackton

;; Step 0 of RacktonCmdline.org: the converter round-trip law.
;;
;; For every predefined converter, parsing what it printed yields the
;; original value back:  parse (print x) = Ok x.  Each property feeds
;; 100 generated values through `conv-print` then `conv-parse`.
;;
;; This is the RED test: the converters in conv.rkt are stubbed, so
;; every property fails (parse returns Err).

(require rackton/cmdline/conv
         rackton/data/result
         "../unit.rkt")

;; `integer->float` is a prelude primitive; whole-number floats
;; round-trip cleanly through show/read.
(: gen-float (Gen Float))
(define gen-float (fmap integer->float (int-range -100000 100000)))

(: gen-char (Gen Char))
(define gen-char (element-of (list #\a #\b #\Z #\0 #\space #\!)))

(: suite (List Test))
(define suite
  (list
    (it-prop "conv-string round-trips"
             (for-all gen-string
                      (lambda (s)
                        (match (conv-parse conv-string (conv-print conv-string s))
                          [(Ok y)  (== y s)]
                          [(Err _) #f]))))

    (it-prop "conv-int round-trips"
             (for-all (int-range -1000000 1000000)
                      (lambda (n)
                        (match (conv-parse conv-int (conv-print conv-int n))
                          [(Ok y)  (== y n)]
                          [(Err _) #f]))))

    (it-prop "conv-bool round-trips"
             (for-all bool
                      (lambda (b)
                        (match (conv-parse conv-bool (conv-print conv-bool b))
                          [(Ok y)  (== y b)]
                          [(Err _) #f]))))

    (it-prop "conv-float round-trips"
             (for-all gen-float
                      (lambda (x)
                        (match (conv-parse conv-float (conv-print conv-float x))
                          [(Ok y)  (== y x)]
                          [(Err _) #f]))))

    (it-prop "conv-char round-trips"
             (for-all gen-char
                      (lambda (c)
                        (match (conv-parse conv-char (conv-print conv-char c))
                          [(Ok y)  (== y c)]
                          [(Err _) #f]))))))

(: test-main (IO Unit))
(define test-main (run-suite "rackton/cmdline/conv" suite))

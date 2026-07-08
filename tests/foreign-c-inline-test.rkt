#lang rackton

;; foreign-c — the inline C-function import surface form.

(require "../unit.rkt")

;; pure, 1-arg
(foreign-c c-cbrt2 (-> Float Float)
           :lib #f :symbol "cbrt" :sig (double -> double))
;; pure, 2-arg
(foreign-c c-hypot2 (-> Float (-> Float Float))
           :lib #f :symbol "hypot" :sig (double double -> double))
;; pure int, default (process) library
(foreign-c c-abs2 (-> Integer Integer)
           :lib #f :symbol "abs" :sig (int -> int))
;; effectful, 0-arg -> IO value
(foreign-c c-getpid (IO Integer)
           :lib #f :symbol "getpid" :sig (-> int))

(: r1 Float)   (define r1 (c-cbrt2 27.0))
(: r2 Float)   (define r2 (c-hypot2 3.0 4.0))
(: r3 Integer) (define r3 (c-abs2 -7))
(: r4 Integer) (define r4 (run-io c-getpid))

;; ---------- assertions ---------------------------------------

(: suite (List Test))
(define suite
  (list
    (it "pure C functions"
        (all-checks
          (list (check-true (< (abs (- r1 3.0)) 1e-9))
                (check-true (< (abs (- r2 5.0)) 1e-9))
                (check-equal? r3 7))))
    (it "effectful (IO) C function"
        (check-true (> r4 0)))))   ; getpid is a positive process id

(: test-main (IO Unit))
(define test-main (run-suite "foreign-c-inline" suite))

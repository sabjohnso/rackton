#lang rackton

;; reactive-signals.rkt — guarded reactive streams with rackton/temporal.
;;
;; Every value here is an INFINITE signal, defined by guarded recursion (the
;; recursive reference always sits under a `Later`, so the stream is
;; productive — it yields each value before deferring the rest).  `sig-take`
;; forces only the finite prefix a consumer asks for.
;;
;; We build a base signal (`nats`), DERIVE others pointwise (`sig-map`,
;; `sig-zip`), and thread an accumulator through a guarded scan (`running-sum`).
;;
;; Run it with `racket examples/reactive-signals.rkt`.

(require rackton/temporal)

;; base: 0, 1, 2, 3, …  (sig-iterate's recursion is guarded under map-later)
(: nats (Signal Integer))
(define nats (sig-iterate (lambda (n) (+ n 1)) 0))

;; pointwise map: n -> n*n
(: squares (Signal Integer))
(define squares (sig-map (lambda (n) (* n n)) nats))

;; Fibonacci: iterate a paired state, then project the first component
(: fibs (Signal Integer))
(define fibs
  (sig-map (lambda (p) (match p [(Pair a b) a]))
           (sig-iterate (lambda (p) (match p [(Pair a b) (Pair b (+ a b))]))
                        (Pair 0 1))))

;; pointwise combine of two signals
(: summed (Signal Integer))
(define summed (sig-zip (lambda (a b) (+ a b)) nats fibs))

;; a guarded SCAN: thread a running total through the stream.
;;   out_0 = in_0,  out_n = out_{n-1} + in_n
(: scan-from (-> Integer (Signal Integer) (Signal Integer)))
(define (scan-from acc s)
  (SigCons (+ acc (sig-head s))
           (map-later (lambda (t) (scan-from (+ acc (sig-head s)) t)) (sig-tail s))))
(: running-sum (-> (Signal Integer) (Signal Integer)))
(define (running-sum s) (scan-from 0 s))

;; ===== output ======================================================
(: ints->str (-> (List Integer) String))
(define (ints->str xs)
  (foldr (lambda (n acc) (string-append (integer->string n) (string-append " " acc))) "" xs))

(: show-sig (-> String (Signal Integer) (IO Unit)))
(define (show-sig label s)
  (println (string-append label (ints->str (sig-take 8 s)))))

(: main (IO Unit))
(define main (do [_ <- (println "Guarded reactive streams (rackton/temporal)")]
               [_ <- (println "")]
               [_ <- (show-sig "nats        : " nats)]
               [_ <- (show-sig "squares     : " squares)]
               [_ <- (show-sig "fibonacci   : " fibs)]
               [_ <- (show-sig "nats + fibs : " summed)]
               [_ <- (show-sig "running sum : " (running-sum nats))]
               [_ <- (println "")]
               [_ <- (println "All infinite and productive — sig-take 8 forces only the prefix.")]
               (pure Unit)))

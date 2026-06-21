#lang rackton

;; Concurrency primitives — threads, MVars, channels.
;; Every test uses wait-thread / take-mvar to synchronize so the
;; assertions don't race the spawned thread.

(require rackton/control/concurrent
         "../unit.rkt")

;; ----- 36.A simple fork + MVar handoff ----------------------

(: child-writes (IO Integer))
(define child-writes
  (do [box  <- new-empty-mvar]
      [tid  <- (fork-io (put-mvar box 42))]
      [val  <- (take-mvar box)]
      [_    <- (wait-thread tid)]
    (pure val)))

;; ----- 36.B Channel producer/consumer round-trip -------------

(: chan-roundtrip (IO Integer))
(define chan-roundtrip
  (do [ch  <- new-chan]
      [tid <- (fork-io (send-chan ch 99))]
      [v   <- (recv-chan ch)]
      [_   <- (wait-thread tid)]
    (pure v)))

;; ----- 36.C multi-thread atomic counter ----------------------
;; Spawn five threads, each doing (modify-mvar (+1)).  Wait for
;; all of them, then read the final value.  Concretely makes sure
;; the modify-mvar primitive is atomic — racy code would lose
;; updates and report < 5.

(: spawn-increment
   (-> (MVar Integer) (IO ThreadId)))
(define (spawn-increment counter)
  (fork-io (modify-mvar counter (lambda (n) (+ n 1)))))

(: five-increments (IO Integer))
(define five-increments
  (do [counter <- (new-mvar 0)]
      [t1 <- (spawn-increment counter)]
      [t2 <- (spawn-increment counter)]
      [t3 <- (spawn-increment counter)]
      [t4 <- (spawn-increment counter)]
      [t5 <- (spawn-increment counter)]
      [_  <- (wait-thread t1)]
      [_  <- (wait-thread t2)]
      [_  <- (wait-thread t3)]
      [_  <- (wait-thread t4)]
      [_  <- (wait-thread t5)]
    (read-mvar counter)))

;; ----- 36.D read-mvar is non-destructive ---------------------

(: read-twice (IO (Pair Integer Integer)))
(define read-twice
  (do [box <- (new-mvar 7)]
      [a   <- (read-mvar box)]
      [b   <- (read-mvar box)]
    (pure (Pair a b))))

;; ---------- assertions ---------------------------------------

(: r-child Integer)       (define r-child (run-io child-writes))
(: r-chan Integer)        (define r-chan (run-io chan-roundtrip))
(: r-five Integer)        (define r-five (run-io five-increments))
(: r-twice (Pair Integer Integer)) (define r-twice (run-io read-twice))

(: suite (List Test))
(define suite
  (list
   (it "fork-io + MVar handoff"
       (check-equal? r-child 42))
   (it "channel producer / consumer round-trip"
       (check-equal? r-chan 99))
   (it "five threads each modify-mvar (+1) atomically"
       (check-equal? r-five 5))
   (it "read-mvar is non-destructive"
       (check-equal? r-twice (Pair 7 7)))))

(: main Unit)
(define main (run-io (run-suite "concurrency-primitives" suite)))

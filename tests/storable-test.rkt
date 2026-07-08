#lang rackton

;; Storable — polymorphic peek / poke (prelude class, instances in
;; rackton/foreign/ptr).

(require rackton/foreign/ptr
         "../unit.rkt")

;; poke (dispatches on the Integer value) then peek (return-typed at Integer)
(: int-rt (IO Integer))
(define int-rt
  (do [p <- (malloc-bytes size-of-int)]
    [_ <- (poke p 99)]
    [v <- (ann (peek p) (IO Integer))]
    [_ <- (free-ptr p)]
    (pure v)))

;; the same polymorphic peek / poke at Float
(: flt-rt (IO Float))
(define flt-rt
  (do [p <- (malloc-bytes size-of-double)]
    [_ <- (poke p 2.5)]
    [v <- (ann (peek p) (IO Float))]
    [_ <- (free-ptr p)]
    (pure v)))

(: suite (List Test))
(define suite
  (list
    (it "Storable Integer"
        (check-equal? (run-io int-rt) 99))
    (it "Storable Float"
        (check-true (< (abs-float (- (run-io flt-rt) 2.5)) 1e-9)))))

(: test-main (IO Unit))
(define test-main (run-suite "Storable" suite))

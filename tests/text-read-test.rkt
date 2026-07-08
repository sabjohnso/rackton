#lang rackton

;; rackton/text/read — parse Strings to typed values (Text.Read).
;; readMaybe can't be polymorphic without a Read class, so these are
;; type-specific: read-int / read-float / read-bool, each (Maybe a).

(require rackton/text/read
         "../unit.rkt")

(: ri     Integer) (define ri     (match (read-int "42") [(Some n) n] [(None) -1]))
(: ri-bad Boolean) (define ri-bad (match (read-int "x")  [(None) #t] [(Some _) #f]))

(: rf     Float)   (define rf     (match (read-float "3.14") [(Some x) x] [(None) -1.0]))
(: rf-int Float)   (define rf-int (match (read-float "5")    [(Some x) x] [(None) -1.0]))
(: rf-bad Boolean) (define rf-bad (match (read-float "abc")  [(None) #t] [(Some _) #f]))

(: rb-t   Boolean) (define rb-t   (match (read-bool "True")  [(Some b) b] [(None) #f]))
(: rb-f   Boolean) (define rb-f   (match (read-bool "False") [(Some b) b] [(None) #t]))
(: rb-bad Boolean) (define rb-bad (match (read-bool "nope")  [(None) #t] [(Some _) #f]))

(: suite (List Test))
(define suite
  (list
    (it "read-int"
        (all-checks
          (list (check-equal? ri 42)
                (check-true ri-bad))))
    (it "read-float"
        (all-checks
          (list (check-true (< (abs (- rf 3.14)) 1e-9))
                (check-true (< (abs (- rf-int 5.0)) 1e-9))
                (check-true rf-bad))))
    (it "read-bool (round-trips show)"
        (all-checks
          (list (check-true  rb-t)
                (check-false rb-f)
                (check-true  rb-bad))))))

(: test-main (IO Unit))
(define test-main (run-suite "rackton/text/read" suite))

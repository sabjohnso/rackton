#lang rackton

;; rackton/system split — each System.* module stands alone when
;; required directly (the umbrella rackton/system still works too,
;; covered by the existing system tests).

(require rackton/system/environment
         rackton/system/directory
         rackton/system/file
         rackton/system/random
         rackton/system/time
         rackton/system/ref
         rackton/system/exception
         rackton/data/result
         "../unit.rkt")

;; ref round-trip
(: ref-rt (IO Integer))
(define ref-rt
  (let& ([r  (make-ref 1)]
         [_  (write-ref r 42)]
         [v  (read-ref r)])
    (pure-io v)))

;; file write/read round-trip in a temp path
(: file-rt (IO String))
(define file-rt
  (let& ([_ (write-file "/tmp/rackton-system-split.txt" "hello")]
         [s (read-file "/tmp/rackton-system-split.txt")])
    (pure-io s)))

;; exists? after delete
(: gone (IO Boolean))
(define gone
  (let& ([_  (write-file "/tmp/rackton-system-split-2.txt" "x")]
         [_  (delete-file "/tmp/rackton-system-split-2.txt")]
         [b  (file-exists? "/tmp/rackton-system-split-2.txt")])
    (pure-io b)))

;; try catches a raised IO error
(: caught (IO Boolean))
(define caught
  (let& ([res (try (raise-io "boom"))])
    (pure-io (match res [(Err _) #t] [(Ok _) #f]))))

;; random within range, env lookup of a missing var
;; [5,6) is the singleton {5}
(: rand-in (IO Boolean))
(define rand-in
  (let& ([n (random-integer 5 6)]) (pure-io (== n 5))))

(: missing-env (IO Boolean))
(define missing-env
  (let& ([m (getenv "RACKTON_DEFINITELY_UNSET_VAR_XYZ")])
    (pure-io (match m [(None) #t] [(Some _) #f]))))

(: r-ref Integer)     (define r-ref (run-io ref-rt))
(: r-file String)     (define r-file (run-io file-rt))
(: r-gone Boolean)    (define r-gone (run-io gone))
(: r-caught Boolean)  (define r-caught (run-io caught))
(: r-rand Boolean)    (define r-rand (run-io rand-in))
(: r-env Boolean)     (define r-env (run-io missing-env))

(: suite (List Test))
(define suite
  (list
    (it "ref"         (check-equal? r-ref 42))
    (it "file"        (check-equal? r-file "hello"))
    (it "directory"   (check-false  r-gone))
    (it "exception"   (check-true   r-caught))
    (it "random"      (check-true   r-rand))
    (it "environment" (check-true   r-env))))

(: test-main (IO Unit))
(define test-main (run-suite "rackton/system split" suite))

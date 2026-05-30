#lang racket/base

;; rackton/system split — each System.* module stands alone when
;; required directly (the umbrella rackton/system still works too,
;; covered by the existing system tests).

(require rackunit
         "../main.rkt")

(rackton
  (require rackton/system/environment
           rackton/system/directory
           rackton/system/file
           rackton/system/random
           rackton/system/time
           rackton/system/ref
           rackton/system/exception)

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
      (pure-io (match m [(None) #t] [(Some _) #f])))))

;; ---------- assertions ---------------------------------------

(test-case "ref"       (check-equal? (run-io ref-rt) 42))
(test-case "file"      (check-equal? (run-io file-rt) "hello"))
(test-case "directory" (check-false  (run-io gone)))
(test-case "exception" (check-true   (run-io caught)))
(test-case "random"    (check-true   (run-io rand-in)))
(test-case "environment" (check-true (run-io missing-env)))

#lang racket/base

;; Smoke test for examples/bit-syntax.rkt: instantiating its `module+
;; main` runs `main`, which builds and parses a sub-byte header and
;; round-trips a length-prefixed frame.  We capture stdout and check
;; the stable lines.

(require rackunit
         racket/runtime-path)

(define-runtime-path bit-syntax-example "../examples/bit-syntax.rkt")

(define out
  (parameterize ([current-output-port (open-output-string)])
    (dynamic-require `(submod ,bit-syntax-example main) #f)
    (get-output-string (current-output-port))))

(test-case "header packs sub-byte fields into two bytes"
           (check-regexp-match #rx"bytes:  #\"E\\\\0\"" out)
           (check-regexp-match #rx"bits:   16" out))

(test-case "header parses back to its fields"
           (check-regexp-match #rx"parsed: version=4 ihl=5 tos=0" out))

(test-case "length-prefixed frame round-trips its payload"
           (check-regexp-match #rx"unframed: hello" out))

#lang racket/base

;; Smoke test for examples/network-headers.rkt: loading the module runs
;; its `main`, which builds a UDP-over-IPv4 datagram with bit syntax and
;; parses it back.  We capture stdout and check the stable lines.

(require rackunit
         racket/runtime-path)

(define-runtime-path network-headers-example "../examples/network-headers.rkt")

(define out
  (parameterize ([current-output-port (open-output-string)])
    (dynamic-require network-headers-example #f)
    (get-output-string (current-output-port))))

(test-case "datagram is 32 bytes (20 IP + 8 UDP + 4 data)"
  (check-regexp-match #rx"on the wire \\(256 bits\\)" out)
  ;; first header byte is 0x45 = version 4, IHL 5
  (check-regexp-match #rx"#\"E" out))

(test-case "IPv4 header parses back to its fields"
  (check-regexp-match #rx"IPv4 version=4 ihl=5" out)
  (check-regexp-match #rx"total length: 32" out)
  (check-regexp-match #rx"protocol:     17" out)
  (check-regexp-match #rx"source:       192[.]168[.]0[.]10" out)
  (check-regexp-match #rx"dest:         8[.]8[.]8[.]8" out))

(test-case "UDP header and payload parse back"
  (check-regexp-match #rx"src port: 40000" out)
  (check-regexp-match #rx"dst port: 53" out)
  (check-regexp-match #rx"length:   12" out)
  (check-regexp-match #rx"data:     PING" out))

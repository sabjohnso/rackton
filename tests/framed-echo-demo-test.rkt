#lang racket/base

;; Smoke test for examples/framed-echo.rkt: instantiating its `module+
;; main` runs `main`, which drives a loopback TCP framed echo and a UDP
;; request/reply and prints the results.  We capture stdout and check
;; the stable lines (the server's "client from …:PORT" line carries an
;; ephemeral port, so it is matched loosely).

(require rackunit
         racket/port
         racket/runtime-path)

(define-runtime-path framed-echo-example "../examples/framed-echo.rkt")

(define out
  (parameterize ([current-output-port (open-output-string)])
    (dynamic-require `(submod ,framed-echo-example main) #f)
    (get-output-string (current-output-port))))

(test-case "framed-echo round-trips a framed TCP message"
           (check-regexp-match #rx"client: got back Hello, framed world!" out))

(test-case "framed-echo gets a UDP reply"
           (check-regexp-match #rx"udp: server said ack:ping" out))

(test-case "framed-echo logs the connecting peer"
           (check-regexp-match #rx"server: client from 127[.]0[.]0[.]1:[0-9]+" out))

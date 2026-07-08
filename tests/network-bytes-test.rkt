#lang rackton

;; Byte-oriented socket I/O carries arbitrary octets intact — including
;; bytes that are not valid UTF-8 — which the String path would corrupt.
;; The payload below (0, 255, 128, 10, 65) is deliberately not valid
;; UTF-8, so an exact round-trip proves the Bytes path is byte-faithful.

(require rackton/network/tcp
         rackton/network/udp
         rackton/control/concurrent
         "../unit.rkt")

(: payload Bytes)
(define payload (list->bytes (list 0 255 128 10 65)))

;; TCP: forked echo server reflects the raw bytes back.
(: tcp-binary (IO Boolean))
(define tcp-binary
  (let& ([lst  (listen-on 0)]
         [port (listener-port lst)]
         [_    (fork-io
                 (let& ([s (accept lst)]
                        [m (recv-bytes s 1024)]
                        [_ (match m
                             [(Some b) (send-bytes s b)]
                             [(None)   (pure-io Unit)])]
                        [_ (close s)])
                   (pure-io Unit)))]
         [c    (connect "127.0.0.1" port)]
         [_    (send-bytes c payload)]
         [r    (recv-bytes c 1024)]
         [_    (close c)]
         [_    (close-listener lst)])
    (pure-io (match r
               [(Some b) (== b payload)]
               [(None)   #f]))))

;; UDP: one datagram of raw bytes, read back exactly.
(: udp-binary (IO Boolean))
(define udp-binary
  (let& ([rx   udp-open]
         [_    (udp-bind rx "127.0.0.1" 0)]
         [port (udp-local-port rx)]
         [tx   udp-open]
         [_    (udp-send-to-bytes tx "127.0.0.1" port payload)]
         [got  (udp-recv-from-bytes rx 1024)]
         [_    (udp-close tx)]
         [_    (udp-close rx)])
    (pure-io (match got
               [(tuple b (tuple _ _)) (== b payload)]))))

(: suite (List Test))
(define suite
  (list
    (it "TCP send-bytes/recv-bytes round-trips non-UTF-8 octets"
        (check-true (run-io tcp-binary)))
    (it "UDP send-to-bytes/recv-from-bytes round-trips non-UTF-8 octets"
        (check-true (run-io udp-binary)))))

(: test-main (IO Unit))
(define test-main (run-suite "network/bytes" suite))

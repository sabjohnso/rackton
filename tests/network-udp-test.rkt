#lang rackton

;; rackton/network/udp — open / bind / send-to / recv-from / close.
;; A receiver socket binds an ephemeral loopback port; a sender datagrams
;; one message to it; recv-from returns the payload together with the
;; sender's address, so the assertion checks both the message and that
;; the reported peer host is loopback.

(require rackton/network/udp
         "../unit.rkt")

;; Send one datagram and read it back with its sender address.
(: datagram-roundtrip (IO (Tuple String String)))
(define datagram-roundtrip
  (let& ([rx   udp-open]
         [_    (udp-bind rx "127.0.0.1" 0)]
         [port (udp-local-port rx)]
         [tx   udp-open]
         [_    (udp-send-to tx "127.0.0.1" port "pong")]
         [got  (udp-recv-from rx 1024)]
         [_    (udp-close tx)]
         [_    (udp-close rx)])
    (pure-io (match got
               [(tuple msg (tuple host _)) (tuple msg host)]))))

(: suite (List Test))
(define suite
  (list
    (it "send-to / recv-from delivers the datagram"
        (check-equal? (tref (run-io datagram-roundtrip) 0) "pong"))
    (it "recv-from reports the loopback sender host"
        (check-equal? (tref (run-io datagram-roundtrip) 1) "127.0.0.1"))))

(: test-main (IO Unit))
(define test-main (run-suite "network/udp" suite))

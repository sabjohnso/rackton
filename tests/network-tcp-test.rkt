#lang rackton

;; rackton/network/tcp — connect / listen / accept / send / recv / close.
;; A loopback round-trip: a forked server accepts one connection, echoes
;; the bytes it receives, and closes; the client connects, sends, and
;; reads the echo back.  `listen 0` asks the OS for an ephemeral port,
;; recovered with `listener-port` so the test never collides with a
;; fixed port already in use.

(require rackton/network/tcp
         rackton/control/concurrent
         "../unit.rkt")

;; One forked echo server, one client, asserting the payload round-trips.
(: echo-roundtrip (IO String))
(define echo-roundtrip
  (let& ([lst  (listen-on 0)]
         [port (listener-port lst)]
         [_    (fork-io
                 (let& ([s  (accept lst)]
                        [m  (recv s 1024)]
                        [_  (match m
                              [(Some msg) (send s msg)]
                              [(None)     (pure-io Unit)])]
                        [_  (close s)])
                   (pure-io Unit)))]
         [c    (connect "127.0.0.1" port)]
         [_    (send c "ping")]
         [r    (recv c 1024)]
         [_    (close c)]
         [_    (close-listener lst)])
    (pure-io (match r
               [(Some s) s]
               [(None)   "<eof>"]))))

;; recv after the peer closes its end yields None (end-of-file).
(: eof-gives-none (IO Boolean))
(define eof-gives-none
  (let& ([lst  (listen-on 0)]
         [port (listener-port lst)]
         [_    (fork-io
                 (let& ([s (accept lst)]
                        [_ (close s)])
                   (pure-io Unit)))]
         [c    (connect "127.0.0.1" port)]
         [r    (recv c 1024)]
         [_    (close c)]
         [_    (close-listener lst)])
    (pure-io (match r
               [(None)   #t]
               [(Some _) #f]))))

(: suite (List Test))
(define suite
  (list
    (it "connect/send/recv echoes the payload over loopback"
        (check-equal? (run-io echo-roundtrip) "ping"))
    (it "recv yields None after the peer closes"
        (check-true (run-io eof-gives-none)))))

(: test-main (IO Unit))
(define test-main (run-suite "network/tcp" suite))

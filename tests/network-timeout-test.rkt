#lang rackton

;; Timeouts, non-blocking polling (timeout 0), and peer address.
;; Timing assertions stay coarse: we only distinguish "idle ⇒ times out"
;; from "data present ⇒ arrives", never exact durations, so the tests do
;; not race.  Idle waits use a short 50 ms budget; cases that expect data
;; give a generous 2000 ms so a slow forked server still wins.

(require rackton/network/tcp
         rackton/network/udp
         rackton/control/concurrent
         "../unit.rkt")

;; --- accept-timeout ------------------------------------------------

;; No client connects within the window → None.
(: accept-times-out (IO Boolean))
(define accept-times-out
  (let& ([lst (listen-on 0)]
         [r   (accept-timeout lst 50)]
         [_   (close-listener lst)])
    (pure-io (match r [(None) #t] [(Some _) #f]))))

;; A client does connect → Some socket.
(: accept-succeeds (IO Boolean))
(define accept-succeeds
  (let& ([lst  (listen-on 0)]
         [port (listener-port lst)]
         [_    (fork-io (let& ([c (connect "127.0.0.1" port)])
                          (pure-io Unit)))]
         [r    (accept-timeout lst 2000)]
         [_    (close-listener lst)])
    (pure-io (match r [(Some _) #t] [(None) #f]))))

;; --- recv-timeout: the three outcomes ------------------------------

;; Peer stays connected but silent → RecvTimeout.
(: recv-times-out (IO Boolean))
(define recv-times-out
  (let& ([lst  (listen-on 0)]
         [port (listener-port lst)]
         [_    (fork-io (let& ([s (accept lst)]
                               [_ (recv s 1024)]   ; block until client leaves
                               [_ (close s)])
                          (pure-io Unit)))]
         [c    (connect "127.0.0.1" port)]
         [r    (recv-timeout c 1024 50)]
         [_    (close c)]
         [_    (close-listener lst)])
    (pure-io (match r [(RecvTimeout) #t] [_ #f]))))

;; Peer sends data → Got.
(: recv-gets-data (IO Boolean))
(define recv-gets-data
  (let& ([lst  (listen-on 0)]
         [port (listener-port lst)]
         [_    (fork-io (let& ([s (accept lst)]
                               [_ (send s "hi")]
                               [_ (recv s 1024)]
                               [_ (close s)])
                          (pure-io Unit)))]
         [c    (connect "127.0.0.1" port)]
         [r    (recv-timeout c 1024 2000)]
         [_    (close c)]
         [_    (close-listener lst)])
    (pure-io (match r [(Got s) (== s "hi")] [_ #f]))))

;; Peer closes → PeerClosed.
(: recv-sees-close (IO Boolean))
(define recv-sees-close
  (let& ([lst  (listen-on 0)]
         [port (listener-port lst)]
         [_    (fork-io (let& ([s (accept lst)]
                               [_ (close s)])
                          (pure-io Unit)))]
         [c    (connect "127.0.0.1" port)]
         [r    (recv-timeout c 1024 2000)]
         [_    (close c)]
         [_    (close-listener lst)])
    (pure-io (match r [(PeerClosed) #t] [_ #f]))))

;; --- udp-recv-from-timeout -----------------------------------------

;; Nothing sent → None.
(: udp-times-out (IO Boolean))
(define udp-times-out
  (let& ([rx udp-bind-open]
         [r  (udp-recv-from-timeout rx 1024 50)]
         [_  (udp-close rx)])
    (pure-io (match r [(None) #t] [(Some _) #f]))))

;; Datagram present → Some.
(: udp-gets-data (IO Boolean))
(define udp-gets-data
  (let& ([rx   udp-bind-open]
         [port (udp-local-port rx)]
         [tx   udp-open]
         [_    (udp-send-to tx "127.0.0.1" port "yo")]
         [r    (udp-recv-from-timeout rx 1024 2000)]
         [_    (udp-close tx)]
         [_    (udp-close rx)])
    (pure-io (match r [(Some _) #t] [(None) #f]))))

;; helper: an opened socket bound to an ephemeral loopback port.
(: udp-bind-open (IO UDPSocket))
(define udp-bind-open
  (let& ([s udp-open]
         [_ (udp-bind s "127.0.0.1" 0)])
    (pure-io s)))

;; --- peer-address --------------------------------------------------

;; The client's remote address is the loopback host and the server's port.
(: peer-is-server (IO Boolean))
(define peer-is-server
  (let& ([lst  (listen-on 0)]
         [port (listener-port lst)]
         [_    (fork-io (let& ([s (accept lst)]
                               [_ (recv s 1024)]
                               [_ (close s)])
                          (pure-io Unit)))]
         [c    (connect "127.0.0.1" port)]
         [a    (peer-address c)]
         [_    (close c)]
         [_    (close-listener lst)])
    (pure-io (match a
               [(tuple host p) (and (== host "127.0.0.1") (== p port))]))))

(: suite (List Test))
(define suite
  (list
   (it "accept-timeout returns None when idle"        (check-true (run-io accept-times-out)))
   (it "accept-timeout returns Some on a connection"  (check-true (run-io accept-succeeds)))
   (it "recv-timeout returns RecvTimeout when silent" (check-true (run-io recv-times-out)))
   (it "recv-timeout returns Got on data"             (check-true (run-io recv-gets-data)))
   (it "recv-timeout returns PeerClosed on close"     (check-true (run-io recv-sees-close)))
   (it "udp-recv-from-timeout returns None when idle" (check-true (run-io udp-times-out)))
   (it "udp-recv-from-timeout returns Some on data"   (check-true (run-io udp-gets-data)))
   (it "peer-address reports the loopback server"     (check-true (run-io peer-is-server)))))

(: main Unit)
(define main (run-io (run-suite "network/timeout" suite)))

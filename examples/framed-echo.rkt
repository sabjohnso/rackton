#lang rackton

;; framed-echo.rkt — length-prefixed framing over TCP, plus a UDP
;; request/reply, end to end on loopback.
;;
;; TCP is a byte *stream*: one `recv-bytes` may return part of a message,
;; or several messages glued together.  A real protocol therefore frames
;; its messages.  Here each frame is a 4-byte big-endian length followed
;; by exactly that many payload bytes, so the reader always knows where a
;; message ends.  `recv-exactly` loops until it has the bytes the header
;; promised, which is the whole point of the byte-oriented API.
;;
;; Run it:  racket examples/framed-echo.rkt

(require rackton/network/tcp
         rackton/network/udp
         rackton/control/concurrent
         rackton/text/bytes)

;; ----- 32-bit big-endian length codec ------------------------------

(: u32->bytes (-> Integer Bytes))
(define (u32->bytes n)
  (list->bytes
    (list (mod (div n 16777216) 256)
          (mod (div n 65536) 256)
          (mod (div n 256) 256)
          (mod n 256))))

(: bytes->u32 (-> Bytes Integer))
(define (bytes->u32 b)
  (match (bytes->list b)
    [(list b3 b2 b1 b0)
     (+ (* b3 16777216) (+ (* b2 65536) (+ (* b1 256) b0)))]
    [_ 0]))

;; ----- framed read/write over a Socket -----------------------------

;; Read exactly `need` bytes, looping over the stream; None if the peer
;; closes before they all arrive.
(: recv-exactly (-> Socket (-> Integer (IO (Maybe Bytes)))))
(define (recv-exactly sock need)
  (if (<= need 0)
    (pure-io (Some bytes-empty))
    (do [chunk <- (recv-bytes sock need)]
      (match chunk
        [(None)   (pure-io None)]
        [(Some b) (do [rest <- (recv-exactly sock (- need (bytes-length b)))]
                    (pure-io (match rest
                               [(None)   None]
                               [(Some r) (Some (bytes-append b r))])))]))))

;; A frame: 4-byte length header, then the payload.
(: send-frame (-> Socket (-> Bytes (IO Unit))))
(define (send-frame sock payload)
  (send-bytes sock (bytes-append (u32->bytes (bytes-length payload)) payload)))

(: recv-frame (-> Socket (IO (Maybe Bytes))))
(define (recv-frame sock)
  (do [hdr <- (recv-exactly sock 4)]
    (match hdr
      [(None)   (pure-io None)]
      [(Some h) (recv-exactly sock (bytes->u32 h))])))

;; ----- TCP: a framed echo server + client --------------------------

;; Accept one client, log who it is, echo one frame back, and close.
(: serve-one (-> Listener (IO Unit)))
(define (serve-one lst)
  (do [sock <- (accept lst)]
    [addr <- (peer-address sock)]
    [_    <- (match addr
               [(tuple host port)
                (println (string-append "server: client from "
                                        (string-append host
                                                       (string-append ":" (show port)))))])]
    [msg  <- (recv-frame sock)]
    [_    <- (match msg
               [(Some b) (send-frame sock b)]
               [(None)   (pure-io Unit)])]
    (close sock)))

(: tcp-demo (IO Unit))
(define tcp-demo
  (do [lst  <- (listen-on 0)]
    [port <- (listener-port lst)]
    [_    <- (fork-io (serve-one lst))]
    [sock <- (connect "127.0.0.1" port)]
    [_    <- (send-frame sock (string->bytes "Hello, framed world!"))]
    [echo <- (recv-frame sock)]
    [_    <- (println (match echo
                        [(Some b) (string-append "client: got back " (bytes->string-lossy b))]
                        [(None)   "client: connection closed early"]))]
    [_    <- (close sock)]
    (close-listener lst)))

;; ----- UDP: request / reply with a client-side timeout -------------

;; Bind a responder; reply to whoever sends, addressed back to them.
(: respond-once (-> UDPSocket (IO Unit)))
(define (respond-once sock)
  (do [req <- (udp-recv-from sock 1024)]
    (match req
      [(tuple msg (tuple host port))
       (udp-send-to sock host port (string-append "ack:" msg))])))

(: udp-demo (IO Unit))
(define udp-demo
  (do [server <- udp-open]
    [_      <- (udp-bind server "127.0.0.1" 0)]
    [port   <- (udp-local-port server)]
    [_      <- (fork-io (respond-once server))]
    [client <- udp-open]
    [_      <- (udp-send-to client "127.0.0.1" port "ping")]
    ;; wait up to a second for the reply; None would mean it was lost.
    [reply  <- (udp-recv-from-timeout client 1024 1000)]
    [_      <- (println (match reply
                          [(Some (tuple b _)) (string-append "udp: server said " (bytes->string-lossy b))]
                          [(None)             "udp: no reply within 1s"]))]
    [_      <- (udp-close client)]
    (udp-close server)))

;; ----- main --------------------------------------------------------

(: main (IO Unit))
(define main (do [_ <- tcp-demo]
               udp-demo))

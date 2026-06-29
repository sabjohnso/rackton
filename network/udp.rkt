#lang rackton

;; rackton/network/udp — UDP datagram sockets (Network.Socket-style).
;;
;; A `UDPSocket` is opaque (a host socket behind `foreign`).  The wire is
;; byte-oriented: `udp-send-to-bytes` / `udp-recv-from-bytes` are the
;; primitives, with `udp-send-to` / `udp-recv-from` as UTF-8 String
;; convenience wrappers.  `recv-from` returns the payload paired with the
;; sender's (host, port), so a server can reply.  Failures raise in IO
;; and are caught with `try` from rackton/system/exception.
;;
;; `udp-recv-from-timeout` gives up after a millisecond budget (0 ms =
;; non-blocking poll); UDP has no end-of-file, so None means "timed out".

(require rackton/text/bytes)
(provide (all-defined-out))

;; Opaque socket type.
(data UDPSocket)

;; udp-open: create an unbound socket (usable immediately for send-to).
(foreign udp-open (IO UDPSocket)
         :from rackton/private/prelude-runtime :as udp-open-prim)

;; udp-bind sock host port: bind to a local address so the socket can
;; receive.  An empty host ("") binds every local interface; port 0 asks
;; the OS for an ephemeral port, recovered with `udp-local-port`.
(foreign udp-bind (-> UDPSocket (-> String (-> Integer (IO Unit))))
         :from rackton/private/prelude-runtime :as udp-bind-prim)

;; udp-local-port: the local port the socket is bound to.
(foreign udp-local-port (-> UDPSocket (IO Integer))
         :from rackton/private/prelude-runtime :as udp-local-port-prim)

;; --- byte-oriented primitives --------------------------------------

;; udp-send-to-bytes sock host port b: send one datagram of raw bytes.
(foreign udp-send-to-bytes (-> UDPSocket (-> String (-> Integer (-> Bytes (IO Unit)))))
         :from rackton/private/prelude-runtime :as udp-send-to-bytes-prim)

;; udp-recv-from-bytes sock n: block for one datagram (up to n bytes),
;; giving the raw payload paired with the sender's (host, port).
(foreign udp-recv-from-bytes
         (-> UDPSocket (-> Integer (IO (Tuple Bytes (Tuple String Integer)))))
         :from rackton/private/prelude-runtime :as udp-recv-from-bytes-prim)

;; udp-recv-from-timeout sock n ms: udp-recv-from-bytes with a deadline,
;; None if no datagram arrives in time (0 ms = non-blocking poll).
(foreign udp-recv-from-timeout
         (-> UDPSocket (-> Integer (-> Integer
              (IO (Maybe (Tuple Bytes (Tuple String Integer)))))))
         :from rackton/private/prelude-runtime :as udp-recv-from-timeout-prim)

;; --- String convenience layer --------------------------------------

;; udp-send-to sock host port msg: send the UTF-8 encoding of a String.
(: udp-send-to (-> UDPSocket (-> String (-> Integer (-> String (IO Unit))))))
(define (udp-send-to sock host port msg)
  (udp-send-to-bytes sock host port (string->bytes msg)))

;; udp-recv-from sock n: receive one datagram, payload decoded UTF-8 with
;; the replacement char, paired with the sender's (host, port).  Use
;; udp-recv-from-bytes for byte-exact reads.
(: udp-recv-from (-> UDPSocket (-> Integer (IO (Tuple String (Tuple String Integer))))))
(define (udp-recv-from sock n)
  (do [r <- (udp-recv-from-bytes sock n)]
      (pure (match r
              [(tuple b addr) (tuple (bytes->string-lossy b) addr)]))))

(foreign udp-close (-> UDPSocket (IO Unit))
         :from rackton/private/prelude-runtime :as udp-close-prim)

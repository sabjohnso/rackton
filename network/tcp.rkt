#lang rackton

;; rackton/network/tcp — TCP sockets (Network.Simple-style).
;;
;; A `Socket` is one connected, duplex endpoint; a `Listener` accepts
;; incoming connections.  Both are opaque (host objects behind
;; `foreign`).  The wire is byte-oriented: `send-bytes` / `recv-bytes`
;; are the primitives, and the `send` / `recv` String pair are thin
;; convenience wrappers (UTF-8, with a lossy read).  Failures (connection
;; refused, a write to a closed peer, …) raise in IO and are caught with
;; `try` from rackton/system/exception, exactly like file I/O.
;;
;; Every blocking read/accept has a millisecond-deadline variant.  Pass a
;; 0 ms budget for a non-blocking poll: it returns immediately, reporting
;; "nothing yet" rather than waiting.

(require rackton/system/exception
         rackton/data/result
         rackton/text/bytes)
;; recv-bytes-timeout-prim is the host shim behind recv-bytes-timeout;
;; it is an implementation detail, not part of the module's interface.
(provide (except-out (all-defined-out) recv-bytes-timeout-prim))

;; Opaque endpoint types.
(data Socket)
(data Listener)

;; The outcome of a timed receive.  Unlike `recv`'s `(Maybe …)`, a
;; deadline read must tell a closed peer (`PeerClosed`) apart from "no
;; data arrived in time" (`RecvTimeout`) — both would otherwise be None.
(data (RecvResult a)
  (Got a)
  PeerClosed
  RecvTimeout)

;; --- client --------------------------------------------------------

;; connect host port: open a connection to a TCP server.
(foreign connect (-> String (-> Integer (IO Socket)))
         #:from rackton/private/prelude-runtime #:as tcp-connect-prim)

;; --- server --------------------------------------------------------

;; listen-on port: start accepting on a port (port 0 asks the OS for an
;; ephemeral one; recover it with `listener-port`).  Named `listen-on`,
;; not `listen`, because the prelude's MonadWriter class already exports
;; `listen`; a bare `listen` here would clash on import.
(foreign listen-on (-> Integer (IO Listener))
         #:from rackton/private/prelude-runtime #:as tcp-listen-prim)

;; accept: block until a client connects, returning its Socket.
(foreign accept (-> Listener (IO Socket))
         #:from rackton/private/prelude-runtime #:as tcp-accept-prim)

;; accept-timeout listener ms: like accept, but give up after ms
;; milliseconds with None (0 ms = non-blocking poll).
(foreign accept-timeout (-> Listener (-> Integer (IO (Maybe Socket))))
         #:from rackton/private/prelude-runtime #:as tcp-accept-timeout-prim)

;; listener-port: the local port the listener is bound to.
(foreign listener-port (-> Listener (IO Integer))
         #:from rackton/private/prelude-runtime #:as tcp-listener-port)

;; peer-address: the connected peer's (host, port).
(foreign peer-address (-> Socket (IO (Tuple String Integer)))
         #:from rackton/private/prelude-runtime #:as tcp-peer-address-prim)

;; --- transfer (bytes are the primitive) ----------------------------

;; send-bytes sock b: write the whole byte string and flush.
(foreign send-bytes (-> Socket (-> Bytes (IO Unit)))
         #:from rackton/private/prelude-runtime #:as tcp-send-bytes-prim)

;; recv-bytes sock n: read up to n bytes as (Some b); None at end-of-file
;; (the peer closed).  Returns as soon as any data is available — it does
;; not wait to fill n.
(foreign recv-bytes (-> Socket (-> Integer (IO (Maybe Bytes))))
         #:from rackton/private/prelude-runtime #:as tcp-recv-bytes-prim)

;; recv-bytes-timeout sock n ms: recv-bytes with a deadline, reporting
;; timeout and peer-close distinctly (see RecvResult).  The host primitive
;; speaks nested Maybe (None / Some None / Some (Some b)); we map it onto
;; the ADT here.
(foreign recv-bytes-timeout-prim
         (-> Socket (-> Integer (-> Integer (IO (Maybe (Maybe Bytes))))))
         #:from rackton/private/prelude-runtime #:as tcp-recv-bytes-timeout-prim)

(: recv-bytes-timeout (-> Socket (-> Integer (-> Integer (IO (RecvResult Bytes))))))
(define (recv-bytes-timeout sock n ms)
  (do [r <- (recv-bytes-timeout-prim sock n ms)]
      (pure (match r
              [(None)          RecvTimeout]
              [(Some (None))   PeerClosed]
              [(Some (Some b)) (Got b)]))))

;; --- transfer (String convenience layer) ---------------------------

;; send sock s: send the UTF-8 encoding of a String.
(: send (-> Socket (-> String (IO Unit))))
(define (send sock s) (send-bytes sock (string->bytes s)))

;; recv sock n: read up to n bytes, decoded UTF-8 with the replacement
;; char (a multi-byte sequence split across the n-byte boundary decodes
;; lossily); None at end-of-file.  Use recv-bytes for byte-exact reads.
(: recv (-> Socket (-> Integer (IO (Maybe String)))))
(define (recv sock n)
  (do [mb <- (recv-bytes sock n)]
      (pure (fmap bytes->string-lossy mb))))

;; recv-timeout sock n ms: the String view of recv-bytes-timeout.
(: recv-timeout (-> Socket (-> Integer (-> Integer (IO (RecvResult String))))))
(define (recv-timeout sock n ms)
  (do [r <- (recv-bytes-timeout sock n ms)]
      (pure (match r
              [(Got b)       (Got (bytes->string-lossy b))]
              [(PeerClosed)  PeerClosed]
              [(RecvTimeout) RecvTimeout]))))

;; --- closing -------------------------------------------------------

(foreign close (-> Socket (IO Unit))
         #:from rackton/private/prelude-runtime #:as tcp-close-prim)

(foreign close-listener (-> Listener (IO Unit))
         #:from rackton/private/prelude-runtime #:as tcp-close-listener)

;; with-connect host port action: connect, run the action, and close the
;; socket even if the action raises (the bracket pattern of
;; system/io's with-file).
(: with-connect (-> String (-> Integer (-> (-> Socket (IO r)) (IO r)))))
(define (with-connect host port action)
  (do [s <- (connect host port)]
      [r <- (try (action s))]
      [_ <- (close s)]
      (match r
        [(Ok v)  (pure v)]
        [(Err e) (raise-io e)])))

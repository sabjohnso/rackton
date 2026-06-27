#lang scribble/manual
@require[scribble/manual
         (for-label rackton rackton/network/tcp rackton/network/udp)]

@title[#:tag "stdlib-network" #:style 'toc]{@tt{rackton/network} — TCP and UDP sockets}

The @tt{network} family provides byte-oriented socket I/O in @racket[IO]:
connection-oriented streams over @racketmodname[rackton/network/tcp] and
connectionless datagrams over @racketmodname[rackton/network/udp].  There is
no umbrella module — the two share verbs (@racket[send-bytes],
@racket[close]) that would collide, so require the specific module you need.

Both modules are @emph{byte-oriented at the core}: @racket[send-bytes] /
@racket[recv-bytes] (and the UDP equivalents) move @racket[Bytes], which is
faithful for any payload.  A thin String layer (@racket[send] / @racket[recv]
/ @racket[udp-send-to] / @racket[udp-recv-from]) encodes UTF-8 for
convenience, decoding received bytes with the replacement char (see
@racket[bytes->string-lossy]); use the byte forms for binary protocols.

Operations fail by raising in @racket[IO] (connection refused, a write to a
closed peer, …); catch them with @racket[try] from
@racketmodname[rackton/system/exception], exactly as for file I/O.

Every blocking read or accept has a millisecond-deadline variant.  A
@racket[0] ms budget polls without blocking: it returns the "nothing yet"
answer immediately rather than waiting.

A worked end-to-end demo — length-prefixed framing over TCP plus a UDP
request/reply — lives in @tt{examples/framed-echo.rkt}.

@local-table-of-contents[]

@section{rackton/network/tcp}
@defmodule[rackton/network/tcp]

Connection-oriented byte streams (Network.Simple-style).  A @racket[Socket]
is one connected, duplex endpoint; a @racket[Listener] accepts incoming
connections.  Both are opaque host objects.

@defidform[#:kind "type" Socket]{An opaque, connected TCP endpoint —
  bidirectional, carrying bytes in both directions.}

@defidform[#:kind "type" Listener]{An opaque listening socket that accepts
  incoming connections.}

@deftogether[(
@defform[#:kind "type" #:id RecvResult #:literals (data Got PeerClosed RecvTimeout)
         (data (RecvResult a)
           (Got a)
           PeerClosed
           RecvTimeout)]
@defthing[#:kind "constructor" Got (-> a (RecvResult a))]
@defthing[#:kind "constructor" PeerClosed (RecvResult a)]
@defthing[#:kind "constructor" RecvTimeout (RecvResult a)])]{
  The outcome of a timed receive.  Unlike a plain @racket[(Maybe a)], a
  deadline read must tell a closed peer (@racket[PeerClosed]) apart from "no
  data arrived in time" (@racket[RecvTimeout]) — both would otherwise be
  @racket[None].}

@subsection{Connecting and listening}

@defproc[(connect [host String] [port Integer]) (IO Socket)]{
  Opens a connection to a TCP server at @racket[host] and @racket[port].}

@defproc[(listen-on [port Integer]) (IO Listener)]{
  Starts accepting connections on @racket[port].  Port @racket[0] asks the OS
  for an ephemeral port, which @racket[listener-port] then reports.  Named
  @racket[listen-on] rather than @tt{listen} to avoid the prelude's
  @tt{MonadWriter} @racket[listen].}

@defproc[(accept [listener Listener]) (IO Socket)]{
  Blocks until a client connects, returning the connected @racket[Socket].}

@defproc[(accept-timeout [listener Listener] [ms Integer]) (IO (Maybe Socket))]{
  Like @racket[accept], but gives up after @racket[ms] milliseconds with
  @racket[None].  A @racket[0] ms budget is a non-blocking poll.  A timeout
  consumes no pending connection.}

@defproc[(listener-port [listener Listener]) (IO Integer)]{
  The local port @racket[listener] is bound to (resolves an ephemeral
  @racket[0] bind to the OS-assigned number).}

@defproc[(peer-address [sock Socket]) (IO (Tuple String Integer))]{
  The connected peer's @racket[(Tuple host port)].}

@subsection{Sending and receiving}

@defproc[(send-bytes [sock Socket] [payload Bytes]) (IO Unit)]{
  Writes all of @racket[payload] and flushes.}

@defproc[(recv-bytes [sock Socket] [n Integer]) (IO (Maybe Bytes))]{
  Reads up to @racket[n] bytes as @racket[(Some b)]; @racket[None] at
  end-of-file (the peer closed).  Returns as soon as @emph{any} data is
  available — it does not wait to fill @racket[n] — so reassembling a known
  length means looping (see the framing example).}

@defproc[(recv-bytes-timeout [sock Socket] [n Integer] [ms Integer]) (IO (RecvResult Bytes))]{
  @racket[recv-bytes] with an @racket[ms]-millisecond deadline, reporting
  timeout (@racket[RecvTimeout]) and peer-close (@racket[PeerClosed])
  distinctly.  A @racket[0] ms budget is a non-blocking poll.}

@defproc[(send [sock Socket] [s String]) (IO Unit)]{
  Sends the UTF-8 encoding of @racket[s].  Convenience wrapper over
  @racket[send-bytes].}

@defproc[(recv [sock Socket] [n Integer]) (IO (Maybe String))]{
  Reads up to @racket[n] bytes, decoded UTF-8 with the replacement char
  (a multi-byte sequence split across the @racket[n]-byte boundary decodes
  lossily); @racket[None] at end-of-file.  Use @racket[recv-bytes] for
  byte-exact reads.}

@defproc[(recv-timeout [sock Socket] [n Integer] [ms Integer]) (IO (RecvResult String))]{
  The String view of @racket[recv-bytes-timeout].}

@subsection{Closing and brackets}

@defproc[(close [sock Socket]) (IO Unit)]{Closes a connected socket.}

@defproc[(close-listener [listener Listener]) (IO Unit)]{Closes a listener.}

@defproc[(with-connect [host String] [port Integer] [action (-> Socket (IO r))]) (IO r)]{
  Connects, runs @racket[action] on the socket, and closes it even if
  @racket[action] raises (the bracket pattern of @racket[with-file]).}

@section{rackton/network/udp}
@defmodule[rackton/network/udp]

Connectionless datagrams (Network.Socket-style).  A @racket[UDPSocket] is an
opaque host socket.  Receives report the sender's address, so a server can
reply.  UDP has no end-of-file, so a timed receive returns @racket[None] only
on timeout.

@defidform[#:kind "type" UDPSocket]{An opaque UDP datagram socket.}

@defthing[udp-open (IO UDPSocket)]{
  Creates an unbound socket (usable immediately for @racket[udp-send-to]).}

@defproc[(udp-bind [sock UDPSocket] [host String] [port Integer]) (IO Unit)]{
  Binds to a local address so @racket[sock] can receive.  An empty
  @racket[host] (@racket[""]) binds every local interface; port @racket[0]
  asks the OS for an ephemeral port, reported by @racket[udp-local-port].}

@defproc[(udp-local-port [sock UDPSocket]) (IO Integer)]{
  The local port @racket[sock] is bound to.}

@defproc[(udp-send-to-bytes [sock UDPSocket] [host String] [port Integer] [payload Bytes]) (IO Unit)]{
  Sends one datagram of raw bytes to @racket[host] and @racket[port].}

@defproc[(udp-recv-from-bytes [sock UDPSocket] [n Integer]) (IO (Tuple Bytes (Tuple String Integer)))]{
  Blocks for one datagram (up to @racket[n] bytes), giving the raw payload
  paired with the sender's @racket[(Tuple host port)].}

@defproc[(udp-recv-from-timeout [sock UDPSocket] [n Integer] [ms Integer]) (IO (Maybe (Tuple Bytes (Tuple String Integer))))]{
  @racket[udp-recv-from-bytes] with an @racket[ms]-millisecond deadline;
  @racket[None] if no datagram arrives in time.  A @racket[0] ms budget is a
  non-blocking poll.}

@defproc[(udp-send-to [sock UDPSocket] [host String] [port Integer] [msg String]) (IO Unit)]{
  Sends the UTF-8 encoding of @racket[msg].  Convenience wrapper over
  @racket[udp-send-to-bytes].}

@defproc[(udp-recv-from [sock UDPSocket] [n Integer]) (IO (Tuple String (Tuple String Integer)))]{
  Receives one datagram, payload decoded UTF-8 with the replacement char,
  paired with the sender's @racket[(Tuple host port)].  Use
  @racket[udp-recv-from-bytes] for byte-exact reads.}

@defproc[(udp-close [sock UDPSocket]) (IO Unit)]{Closes a UDP socket.}

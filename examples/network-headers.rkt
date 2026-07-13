#lang rackton

;; network-headers.rkt — writing and reading real network headers with
;; Erlang-style bit syntax.
;;
;; Builds a UDP-over-IPv4 datagram out of its header fields and parses it
;; back apart.  The headers are where bit syntax earns its keep: the
;; fields do not respect byte boundaries (IPv4 opens with a 4-bit version
;; and 4-bit header length packed into one byte, and carries a 3-bit
;; flags field glued to a 13-bit fragment offset), yet each is a named
;; segment of the obvious width.  The same `bits` form writes the header
;; and reads it; an IPv4 address is itself built and shown by splitting a
;; 32-bit segment into four octets.
;;
;; Run it with `racket examples/network-headers.rkt`.

(require rackton/text/string)   ; string-append*

;; ===== IPv4 addresses =============================================
;; A 32-bit address, assembled from four octets and rendered back to a
;; dotted quad — both directions via `bits`.

(: make-ip (-> Integer (-> Integer (-> Integer (-> Integer Integer)))))
(define (make-ip a b c d)
  (match (bits [a 8] [b 8] [c 8] [d 8])
    [(bits [n 32]) n]
    [_ 0]))

(: ip->string (-> Integer String))
(define (ip->string addr)
  (match (bits [addr 32])
    [(bits [a 8] [b 8] [c 8] [d 8])
     (string-append* (show a) "." (show b) "." (show c) "." (show d))]
    [_ "?.?.?.?"]))

;; ===== UDP header (RFC 768) =======================================
;; Four 16-bit fields, then the application data.

(struct UDP
  [source-port : Integer]
  [dest-port   : Integer]
  [length      : Integer]   ; header + data
  [checksum    : Integer]
  [data        : Bytes])

(: encode-udp (-> Integer (-> Integer (-> Bytes Bitstring))))
(define (encode-udp src-port dst-port data)
  (bits [src-port 16]
        [dst-port 16]
        [(+ 8 (bytes-length data)) 16]   ; length = 8-byte header + data
        [0 16]                            ; checksum 0 = "not computed"
        [data _ binary]))

(: decode-udp (-> Bitstring (Maybe UDP)))
(define (decode-udp b)
  (match b
    [(bits [src-port 16] [dst-port 16] [length 16] [checksum 16]
           [data _ binary])
     (Some (UDP src-port dst-port length checksum data))]
    [_ None]))

;; ===== IPv4 header (RFC 791, no options) ==========================
;; Note the sub-byte fields: version+IHL share a byte, and the 3-bit
;; flags sit directly against the 13-bit fragment offset.  We keep the
;; fields we display and bind the rest to `_`.

(struct IPv4
  [version      : Integer]
  [ihl          : Integer]   ; header length, in 32-bit words
  [total-length : Integer]   ; header + payload, bytes
  [ttl          : Integer]   ; time to live
  [protocol     : Integer]   ; 17 = UDP
  [source       : Integer]   ; source address
  [dest         : Integer]   ; destination address
  [payload      : Bytes])

;; `total-len` is supplied by the caller (which knows the data size);
;; `payload` is another bitstring spliced in whole via a `bitstring`
;; segment.
(: encode-ipv4 (-> Integer (-> Integer (-> Integer (-> Bitstring Bitstring)))))
(define (encode-ipv4 src dst total-len payload)
  (bits [4 4]            ; version 4
        [5 4]            ; IHL 5 words = 20 bytes, no options
        [0 6]            ; DSCP
        [0 2]            ; ECN
        [total-len 16]
        [0 16]           ; identification
        [2 3]            ; flags: Don't Fragment
        [0 13]           ; fragment offset
        [64 8]           ; TTL
        [17 8]           ; protocol = UDP
        [0 16]           ; header checksum (0 here)
        [src 32]
        [dst 32]
        [payload _ bitstring]))

(: decode-ipv4 (-> Bitstring (Maybe IPv4)))
(define (decode-ipv4 b)
  (match b
    [(bits [version 4] [ihl 4] [_ 6] [_ 2]
           [total-len 16] [_ 16]
           [_ 3] [_ 13]
           [ttl 8] [protocol 8] [_ 16]
           [src 32] [dst 32]
           [payload _ binary])
     (Some (IPv4 version ihl total-len ttl protocol src dst payload))]
    [_ None]))

;; ===== build + parse a datagram ===================================

(: datagram (-> Bytes Bitstring))
(define (datagram data)
  (let ([udp (encode-udp 40000 53 data)])
    (encode-ipv4 (make-ip 192 168 0 10)
                 (make-ip 8 8 8 8)
                 (+ 28 (bytes-length data))   ; 20 IP + 8 UDP + data
                 udp)))

;; ===== rendering ==================================================

(: show-bytes (-> (Maybe Bytes) String))
(define (show-bytes mb)
  (match mb
    [(Some b) (show b)]
    [None     "<<not byte-aligned>>"]))

(: report-udp (-> Bytes (IO Unit)))
(define (report-udp payload)
  (match (decode-udp (bytes->bitstring payload))
    [(Some u)
     (let& ([_ (println (string-append* "    src port: " (show (UDP-source-port u))))]
            [_ (println (string-append* "    dst port: " (show (UDP-dest-port u))))]
            [_ (println (string-append* "    length:   " (show (UDP-length u))))])
       (match (bytes->string (UDP-data u))
         [(Some s) (println (string-append* "    data:     " s))]
         [None     (println "    data:     <<binary>>")]))]
    [None (println "    <<malformed UDP>>")]))

(: report-ipv4 (-> IPv4 (IO Unit)))
(define (report-ipv4 h)
  (let& ([_ (println (string-append* "  IPv4 version=" (show (IPv4-version h))
                                     " ihl=" (show (IPv4-ihl h))))]
         [_ (println (string-append* "    total length: " (show (IPv4-total-length h))))]
         [_ (println (string-append* "    ttl:          " (show (IPv4-ttl h))))]
         [_ (println (string-append* "    protocol:     " (show (IPv4-protocol h))))]
         [_ (println (string-append* "    source:       " (ip->string (IPv4-source h))))]
         [_ (println (string-append* "    dest:         " (ip->string (IPv4-dest h))))]
         [_ (println "  UDP payload:")])
    (report-udp (IPv4-payload h))))

;; ===== main =======================================================

(: main (IO Unit))
(define main (let ([packet (datagram (string->bytes "PING"))])
               (let& ([_ (println "UDP-over-IPv4 datagram via bit syntax:")]
                      [_ (println "")]
                      [_ (println (string-append* "on the wire ("
                                                  (show (bitstring-length packet)) " bits):"))]
                      [_ (println (string-append* "  " (show-bytes (bitstring->bytes packet))))]
                      [_ (println "")]
                      [_ (println "parsed back:")]
                      [_ (match (decode-ipv4 packet)
                           [(Some h) (report-ipv4 h)]
                           [None     (println "  <<malformed IPv4>>")])]
                      [_ (println "")])
                 (println "done."))))

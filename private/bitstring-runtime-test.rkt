#lang racket/base

;; Property tests for the Bitstring bit algebra (bitstring-runtime.rkt).
;;
;; The laws under test:
;;   - build-then-read is identity: concatenating width-w integer
;;     segments and reading each back at its offset recovers it;
;;   - bytes ⇄ bitstring round-trips, with length 8·(byte count);
;;   - a signed read inverts a signed segment over its whole range.

(module+ test
  (require rackunit
           rackcheck
           "bitstring-runtime.rkt")

  ;; ----- examples ----------------------------------------------------

  (check-equal? (bitstring->bytes-exact (int->bitstring 5 3)) #f) ; 3 bits
  (check-equal? (bitstring-len (int->bitstring 5 3)) 3)
  (check-equal? (bitstring->bytes-exact
                 (bitstring-concat (int->bitstring 4 4) (int->bitstring 5 4)))
                #"E")                                              ; 0x45
  (check-equal? (bitstring-read (bytes->bitstring (bytes 255)) 0 8 #t) -1)
  (check-equal? (bitstring-read (bytes->bitstring (bytes 128)) 0 8 #t) -128)

  ;; ----- generators --------------------------------------------------

  ;; One unsigned segment: a width in 1..16 and a value that fits it.
  (define gen:seg
    (gen:let ([w (gen:integer-in 1 16)]
              [r (gen:integer-in 0 1000000)])
      (cons (modulo r (expt 2 w)) w)))

  (define (build segs)
    (foldl (lambda (seg acc)
             (bitstring-concat acc (int->bitstring (car seg) (cdr seg))))
           empty-bitstring segs))

  ;; ----- properties --------------------------------------------------

  ;; build-then-read is identity, and the total length is the sum of widths.
  (check-property
   (property bits-build-read-identity ([segs (gen:list gen:seg #:max-length 8)])
     (define bs (build segs))
     (and (= (bitstring-len bs) (foldl + 0 (map cdr segs)))
          (let loop ([segs segs] [off 0])
            (cond
              [(null? segs) #t]
              [(= (bitstring-read bs off (cdr (car segs)) #f) (car (car segs)))
               (loop (cdr segs) (+ off (cdr (car segs))))]
              [else #f])))))

  ;; bytes ⇄ bitstring round-trips exactly.
  (check-property
   (property bytes-bitstring-roundtrip
             ([bs (gen:list (gen:integer-in 0 255) #:max-length 12)])
     (define b (apply bytes bs))
     (define bit (bytes->bitstring b))
     (and (= (bitstring-len bit) (* 8 (bytes-length b)))
          (equal? (bitstring->bytes-exact bit) b))))

  ;; A signed read inverts a signed segment across its full range.
  (check-property
   (property signed-read-inverts-build
             ([w (gen:integer-in 1 16)] [r (gen:integer-in 0 1000000)])
     (define lo (- (expt 2 (sub1 w))))
     (define hi (sub1 (expt 2 (sub1 w))))
     (define v (+ lo (modulo r (+ 1 (- hi lo)))))
     (= (bitstring-read (int->bitstring v w) 0 w #t) v))))

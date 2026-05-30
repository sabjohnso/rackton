#lang rackton

;; rackton/numeric/show — radix conversion for integers, in the spirit
;; of Haskell's Numeric module (showHex / showOct / readHex / readDec,
;; plus binary).  The host's @racket[number->string] and
;; @racket[string->number] take a radix argument, so each direction is
;; a single @racket[(racket …)] escape; the read direction builds a
;; @racket[Maybe] in the escape and returns @racket[None] when the
;; string isn't a valid integer in that base.
;;
;; The float formatters (showFFloat / showEFloat / showGFloat) need
;; racket/format's @racket[~r], which isn't reachable from an escape, so
;; they come in through @racket[foreign] runtime primitives instead.

(provide (all-defined-out))

;; --- show: Integer -> String ---------------------------------------

(: num-show-hex (-> Integer String))
(define (num-show-hex n) (racket String (n) (number->string n 16)))

(: num-show-oct (-> Integer String))
(define (num-show-oct n) (racket String (n) (number->string n 8)))

(: num-show-bin (-> Integer String))
(define (num-show-bin n) (racket String (n) (number->string n 2)))

;; --- read: String -> (Maybe Integer) -------------------------------
;; None unless the whole string parses to an exact integer in the base.

(: num-read-hex (-> String (Maybe Integer)))
(define (num-read-hex s)
  (racket (Maybe Integer) (s)
    (let ([v (string->number s 16)])
      (if (and v (exact-integer? v)) (Some v) None))))

(: num-read-oct (-> String (Maybe Integer)))
(define (num-read-oct s)
  (racket (Maybe Integer) (s)
    (let ([v (string->number s 8)])
      (if (and v (exact-integer? v)) (Some v) None))))

(: num-read-dec (-> String (Maybe Integer)))
(define (num-read-dec s)
  (racket (Maybe Integer) (s)
    (let ([v (string->number s 10)])
      (if (and v (exact-integer? v)) (Some v) None))))

;; --- float formatting (Numeric showFFloat / showEFloat / showGFloat) -
;; The precision is the number of digits after the decimal point;
;; None requests full precision.  The runtime primitives take a plain
;; Integer (negative = full), so the wrappers map the Maybe to it.

(foreign show-f-float (-> Integer (-> Float String))
         #:from rackton/private/prelude-runtime)
(foreign show-e-float (-> Integer (-> Float String))
         #:from rackton/private/prelude-runtime)
(foreign show-g-float (-> Integer (-> Float String))
         #:from rackton/private/prelude-runtime)

(: prec->int (-> (Maybe Integer) Integer))
(define (prec->int p) (match p [(None) -1] [(Some n) n]))

;; fixed-point notation, e.g. (num-show-f-float (Some 2) 3.14159) = "3.14".
(: num-show-f-float (-> (Maybe Integer) (-> Float String)))
(define (num-show-f-float p x) (show-f-float (prec->int p) x))

;; scientific notation, e.g. (num-show-e-float (Some 3) 245.7) = "2.457e2".
(: num-show-e-float (-> (Maybe Integer) (-> Float String)))
(define (num-show-e-float p x) (show-e-float (prec->int p) x))

;; general: fixed inside [0.1, 1e7), scientific outside.
(: num-show-g-float (-> (Maybe Integer) (-> Float String)))
(define (num-show-g-float p x) (show-g-float (prec->int p) x))

#lang rackton

;; rackton/numeric/show — radix conversion for integers, in the spirit
;; of Haskell's Numeric module (showHex / showOct / readHex / readDec,
;; plus binary).  The host's @racket[number->string] and
;; @racket[string->number] take a radix argument, so each direction is
;; a single @racket[(racket …)] escape; the read direction builds a
;; @racket[Maybe] in the escape and returns @racket[None] when the
;; string isn't a valid integer in that base.
;;
;; (showFFloat / showEFloat / showGFloat — fixed/scientific float
;; formatting — are NOT provided: they need racket/format's @racket[~r],
;; which isn't reachable from the escape scope.  See PLAN.org.)

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

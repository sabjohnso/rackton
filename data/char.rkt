#lang rackton

;; rackton/data/char — Data.Char.  Predicates and conversions over the
;; prelude's @racket[Char] type.  The prelude already ships
;; @racket[char-upcase] / @racket[char-downcase] /
;; @racket[char-alphabetic?] / @racket[char-numeric?] /
;; @racket[char-whitespace?] / @racket[char->integer] /
;; @racket[integer->char]; this module adds the rest, using
;; @racket[(racket …)] escapes for the racket/base char predicates the
;; prelude doesn't surface.

(provide (all-defined-out))

;; --- aliases over prelude primitives -------------------------------

(: ord (-> Char Integer))
(define (ord c) (char->integer c))

;; chr is total here (panics on an invalid code point, like Haskell's
;; @tt{chr}); the prelude's @racket[integer->char] is the safe
;; @racket[(Maybe Char)] version, which we unwrap.  (A `(racket …)`
;; escape can't reach racket/base's integer->char — the prelude shadows
;; the name in this module's scope.)
(: chr (-> Integer Char))
(define (chr n)
  (match (integer->char n)
    [(Some c) c]
    [(None)   (panic "chr: code point out of range")]))

(: to-upper (-> Char Char))
(define (to-upper c) (char-upcase c))

(: to-lower (-> Char Char))
(define (to-lower c) (char-downcase c))

;; --- predicates ----------------------------------------------------

(: digit? (-> Char Boolean))
(define (digit? c) (racket Boolean (c) (char<=? #\0 c #\9)))

(: hex-digit? (-> Char Boolean))
(define (hex-digit? c)
  (racket Boolean (c)
    (or (char<=? #\0 c #\9)
        (let ([d (char-downcase c)]) (char<=? #\a d #\f)))))

(: upper? (-> Char Boolean))
(define (upper? c) (racket Boolean (c) (char-upper-case? c)))

(: lower? (-> Char Boolean))
(define (lower? c) (racket Boolean (c) (char-lower-case? c)))

;; alpha? / space? alias the prelude's Unicode predicates.
(: alpha? (-> Char Boolean))
(define (alpha? c) (char-alphabetic? c))

(: alpha-num? (-> Char Boolean))
(define (alpha-num? c) (if (alpha? c) #t (char-numeric? c)))

(: space? (-> Char Boolean))
(define (space? c) (char-whitespace? c))

(: control? (-> Char Boolean))
(define (control? c) (racket Boolean (c) (char-iso-control? c)))

(: punctuation? (-> Char Boolean))
(define (punctuation? c) (racket Boolean (c) (char-punctuation? c)))

;; --- digit <-> value (ASCII) ---------------------------------------

(: digit->int (-> Char Integer))
(define (digit->int c) (- (ord c) 48))

(: int->digit (-> Integer Char))
(define (int->digit n) (chr (+ n 48)))

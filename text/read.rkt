#lang rackton

;; rackton/text/read — parse Strings back into typed values (Text.Read).
;;
;; Haskell's @racket[readMaybe] is @racket[Read a => String -> Maybe a];
;; Rackton has no Read class (and an HM language can't dispatch on the
;; expected result type the way Read does), so these are type-specific
;; readers, each returning @racket[(Maybe a)] — @racket[None] when the
;; String doesn't parse.  @racket[read-bool] accepts @racket["True"] /
;; @racket["False"], round-tripping the prelude's @racket[show] for
;; @racket[Boolean].

(provide (all-defined-out))

;; parse a decimal Integer (reuses the prelude's primitive).
(: read-int (-> String (Maybe Integer)))
(define (read-int s) (string->integer s))

;; parse any real number as a Float (an integer string reads as a Float
;; too, matching Haskell's `readMaybe @Double "5" == Just 5.0`).
(: read-float (-> String (Maybe Float)))
(define (read-float s)
  (racket (Maybe Float) (s)
    (let ([v (string->number s)])
      (if (and v (real? v)) (Some (exact->inexact v)) None))))

;; parse a Boolean written as "True" / "False".
(: read-bool (-> String (Maybe Boolean)))
(define (read-bool s)
  (racket (Maybe Boolean) (s)
    (cond
      [(string=? s "True")  (Some #t)]
      [(string=? s "False") (Some #f)]
      [else                 None])))

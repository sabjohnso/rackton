#lang rackton

;; rackton/data/coerce — a general, lawless coercion protocol, in the
;; style of Rust's From/Into.  `coerce` converts a source type to a
;; target type; it dispatches at runtime on the source, and the target
;; is resolved from the expected type (or an `(ann (coerce x) T)`
;; ascription when context does not fix it).
;;
;; The class is deliberately LAW-FREE: it must admit lossy coercions
;; (Float -> Integer truncates), so — like `Show` — it states no
;; algebraic law.  The only obligation is conditional: any reflexive
;; instance `(Coerce a a)`, should a user write one, must be the
;; identity.  No blanket diagonal instance ships, because it would
;; overlap almost every other instance.
;;
;; Instance heads dispatch on the SOURCE type, so each source must be a
;; concrete type constructor — a `((Real a) => (Coerce a T))` blanket
;; over a type variable is not expressible.  The shipped instances are
;; therefore the concrete numeric-tower conversions; each delegates to a
;; conversion that already exists behind numeric.conversions' `num-`
;; interface.  A value -> String coercion is left to the prelude's
;; `show`, which is already polymorphic over `Show`.

(provide (protocol-out Coerce))

(require rackton/numeric/conversions)

(protocol (Coerce a b)
  (: coerce (-> a b)))

;; --- widenings -----------------------------------------------------
;; Integer -> Float is exact only up to 2^53; larger magnitudes lose
;; precision (so it is not injective in general).  Integer -> Rational
;; is exact and injective.
(instance (Coerce Integer Float)
  (define (coerce x) (num-integer->float x)))

(instance (Coerce Integer Rational)
  (define (coerce x) (num-to-rational x)))

(instance (Coerce Rational Float)
  (define (coerce x) (num-rational->float x)))

;; --- narrowings (lossy — the class allows it) ----------------------
;; Float -> Integer truncates toward zero.
(instance (Coerce Float Integer)
  (define (coerce x) (num-float->integer x)))

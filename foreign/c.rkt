#lang rackton

;; rackton/foreign/c — a curated set of C library (libm) functions that
;; the prelude's Floating class doesn't already cover, bound through the
;; @racket[foreign] form.  This both adds genuinely useful numerics and
;; shows the general pattern for calling external C functions.
;;
;; THIS MODULE IS UNSAFE, like @racketmodname[rackton/foreign/ptr].  Each
;; binding crosses the @racket[get-ffi-obj] trust boundary: the Rackton
;; type (and, for inline @racket[foreign-c], the C @racket[#:sig]) is
;; asserted, never checked against the real C ABI.  A wrong type,
;; signature, or library corrupts memory or crashes the process.
;;
;; The pattern, for binding your OWN C function:
;;   1. Write a small Racket module that uses @racket[get-ffi-obj] /
;;      @racket[ffi-lib] from @racketmodname[ffi/unsafe] to produce a
;;      Racket procedure (curry multi-argument ones with define/curried).
;;   2. Import it here with @racket[(foreign name TYPE #:from that-module)]
;;      — the Rackton TYPE is the (unchecked) trust boundary.
;; The inline @racket[foreign-c] form is sugar over exactly this.
;;
;; These libm functions are treated as pure (like the prelude's trig).

(provide (all-defined-out))

;; cube root.
(foreign c-cbrt (-> Float Float)
         #:from rackton/private/clib-runtime)

;; sqrt(a^2 + b^2) without overflow.
(foreign c-hypot (-> Float (-> Float Float))
         #:from rackton/private/clib-runtime)

;; e^x - 1, accurate for x near 0.
(foreign c-expm1 (-> Float Float)
         #:from rackton/private/clib-runtime)

;; log(1 + x), accurate for x near 0.
(foreign c-log1p (-> Float Float)
         #:from rackton/private/clib-runtime)

;; the gamma function.
(foreign c-tgamma (-> Float Float)
         #:from rackton/private/clib-runtime)

;; log of the absolute value of gamma.
(foreign c-lgamma (-> Float Float)
         #:from rackton/private/clib-runtime)

;; the error function.
(foreign c-erf (-> Float Float)
         #:from rackton/private/clib-runtime)

;; the complementary error function (1 - erf).
(foreign c-erfc (-> Float Float)
         #:from rackton/private/clib-runtime)

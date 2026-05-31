#lang racket/base

;; private/clib-runtime.rkt — runtime for rackton/foreign/c.
;;
;; A curated set of libm functions that the prelude doesn't already
;; provide, bound through Racket's `get-ffi-obj`.  These demonstrate
;; binding an external C function: each is a plain Racket procedure that
;; foreign/c imports with a trusted Rackton type.  Multi-argument
;; bindings are wrapped with define/curried so Rackton's n-ary /
;; partial-application calling convention works.
;;
;; Kept in its own module so ffi/unsafe loads only when foreign/c is
;; required.  These math functions are treated as pure (the trust
;; boundary), matching how the prelude types sin / cos / sqrt.

(require ffi/unsafe
         (only-in "dict.rkt" define/curried))

(provide c-cbrt c-hypot c-expm1 c-log1p c-tgamma c-lgamma c-erf c-erfc)

;; The C math library.  "6" is the glibc soname version; "" falls back
;; to the unversioned name on platforms that use it.
(define libm (ffi-lib "libm" '("6" "")))

(define (bind-1 name) (get-ffi-obj name libm (_fun _double -> _double)))
(define (bind-2 name) (get-ffi-obj name libm (_fun _double _double -> _double)))

(define c-cbrt   (bind-1 "cbrt"))    ; cube root
(define c-expm1  (bind-1 "expm1"))   ; e^x - 1, accurate near 0
(define c-log1p  (bind-1 "log1p"))   ; log(1+x), accurate near 0
(define c-tgamma (bind-1 "tgamma"))  ; gamma function
(define c-lgamma (bind-1 "lgamma"))  ; log |gamma|
(define c-erf    (bind-1 "erf"))     ; error function
(define c-erfc   (bind-1 "erfc"))    ; complementary error function

;; hypot is two-argument; curry it for Rackton's calling convention.
(define raw-hypot (bind-2 "hypot"))
(define/curried (c-hypot a b) (raw-hypot a b))

#lang rackton

;; rackton/foreign/ptr — Foreign.Ptr / Foreign.Marshal core: an opaque
;; typed pointer, raw allocation, typed peek/poke, pointer arithmetic,
;; and C strings, over Racket's @racket[ffi/unsafe].
;;
;; THIS MODULE IS UNSAFE.  Like Haskell's @tt{Foreign}, it does no bounds
;; checking and requires manual @racket[free-ptr]; a stray offset, a
;; double free, or a use-after-free corrupts memory or crashes the
;; process.  It is not auto-loaded and not part of @tt{batteries} —
;; require it explicitly, and only when you must touch raw memory (e.g.
;; marshalling for a future C-function binding).
;;
;; A @racket[(Ptr a)] is opaque; the @racket[a] is a phantom tag for the
;; pointee type.  Reads and writes come in two forms: the type-specific
;; primitives (@racket[peek-int] / @racket[poke-int] /
;; @racket[peek-double] / …) and the prelude's @tt{Storable} class, whose
;; polymorphic @racket[peek] / @racket[poke] (instances below) layer over
;; them.  NULL is @racket[null-ptr].

(provide (all-defined-out))

;; The opaque pointer type `(Ptr a)` and the `Storable` class are in the
;; prelude (Storable.peek is return-typed, so the class must be); the
;; Storable instances are below.

;; A C string is a pointer to NUL-terminated bytes.
(define-alias (CString) (Ptr Char))

;; --- sizes (bytes) -------------------------------------------------

(foreign size-of-int    Integer #:from rackton/private/ffi-runtime)
(foreign size-of-double Integer #:from rackton/private/ffi-runtime)
(foreign size-of-ptr    Integer #:from rackton/private/ffi-runtime)

;; --- allocation / lifetime -----------------------------------------

;; malloc n raw bytes; release with free-ptr (never garbage-collected).
(foreign malloc-bytes (-> Integer (IO (Ptr a)))
         #:from rackton/private/ffi-runtime)

(foreign free-ptr (-> (Ptr a) (IO Unit))
         #:from rackton/private/ffi-runtime)

;; --- null + arithmetic ---------------------------------------------

(foreign null-ptr (Ptr a)
         #:from rackton/private/ffi-runtime)

(foreign ptr-null? (-> (Ptr a) Boolean)
         #:from rackton/private/ffi-runtime)

;; offset a pointer by a number of bytes.
(foreign ptr-plus (-> (Ptr a) (-> Integer (Ptr a)))
         #:from rackton/private/ffi-runtime)

;; --- typed peek / poke ---------------------------------------------

(foreign peek-int (-> (Ptr Integer) (IO Integer))
         #:from rackton/private/ffi-runtime)
(foreign poke-int (-> (Ptr Integer) (-> Integer (IO Unit)))
         #:from rackton/private/ffi-runtime)

(foreign peek-double (-> (Ptr Float) (IO Float))
         #:from rackton/private/ffi-runtime)
(foreign poke-double (-> (Ptr Float) (-> Float (IO Unit)))
         #:from rackton/private/ffi-runtime)

(foreign peek-byte (-> (Ptr Integer) (IO Integer))
         #:from rackton/private/ffi-runtime)
(foreign poke-byte (-> (Ptr Integer) (-> Integer (IO Unit)))
         #:from rackton/private/ffi-runtime)

;; --- C strings -----------------------------------------------------

;; copy a String into a freshly malloc'd, NUL-terminated buffer.
(foreign string->c-string (-> String (IO CString))
         #:from rackton/private/ffi-runtime)

;; read a NUL-terminated UTF-8 C string back into a String.
(foreign c-string->string (-> CString (IO String))
         #:from rackton/private/ffi-runtime)

;; --- Storable instances --------------------------------------------
;; The polymorphic peek / poke (declared in the prelude) over the
;; scalar C types, in terms of the type-specific operations above.
;; (Bytes use peek-byte / poke-byte directly — a Byte isn't a distinct
;; Rackton type, so it can't have its own Storable Integer instance.)

(instance (Storable Integer)
  (define (peek p)   (peek-int p))
  (define (poke p v) (poke-int p v)))

(instance (Storable Float)
  (define (peek p)   (peek-double p))
  (define (poke p v) (poke-double p v)))

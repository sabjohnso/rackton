#lang racket/base

;; private/ffi-runtime.rkt — runtime for rackton/foreign/ptr.
;;
;; The C-FFI primitives are kept in their own module (rather than in
;; prelude-runtime) so that `ffi/unsafe` — and the FFI machinery it
;; loads — is only pulled in when a program actually requires
;; rackton/foreign/ptr.  Every Rackton-typed wrapper is in foreign/ptr;
;; these are the bare host bindings it imports via `foreign`.
;;
;; A Rackton `Ptr a` is a Racket cpointer (NULL = #f).  IO actions are
;; built with prelude-runtime's `$io` so they share the same struct
;; `run-io` destructures.  Everything here is UNSAFE: raw malloc, manual
;; free, no bounds checking.

(require ffi/unsafe
         (only-in "prelude-runtime.rkt" $io MkUnit)
         (only-in "dict.rkt" define/curried))

(provide malloc-bytes free-ptr null-ptr ptr-null? ptr-plus
         size-of-int size-of-double size-of-ptr
         peek-int poke-int peek-double poke-double peek-byte poke-byte
         string->c-string c-string->string
         rackton-ffi-bind)

;; --- inline foreign-c support --------------------------------------

;; Map a Rackton C-type keyword to a Racket ctype.
(define (tag->ctype t)
  (cond
    [(eq? t 'double)  _double]
    [(eq? t 'int)     _int]
    [(eq? t 'string)  _string]
    [(eq? t 'pointer) _pointer]
    [(eq? t 'void)    _void]
    [(eq? t 'byte)    _byte]
    [else (error 'foreign-c "unknown C type: ~a" t)]))

;; Bind a C function: build the function ctype from the arg/result tags,
;; fetch the symbol from the library (lib is a string, or #f for the
;; running process), and return either the raw n-ary procedure (pure) or
;; an IO-wrapping form.  For io? with no args the binding IS an IO action
;; (a value); with args it is a function returning an IO action.
(define (rackton-ffi-bind lib sym arg-tags res-tag io? arity)
  (define proc
    (get-ffi-obj sym (ffi-lib lib)
                 (_cprocedure (map tag->ctype arg-tags) (tag->ctype res-tag))))
  (cond
    [(not io?)        proc]
    [(= arity 0)      ($io (lambda () (proc)))]
    [else             (lambda args ($io (lambda () (apply proc args))))]))

;; --- sizes (pure) --------------------------------------------------

(define size-of-int    (ctype-sizeof _int))
(define size-of-double (ctype-sizeof _double))
(define size-of-ptr    (ctype-sizeof _pointer))

;; --- allocation / lifetime (IO) ------------------------------------

;; malloc n raw bytes (manually freed with free-ptr).
(define (malloc-bytes n) ($io (lambda () (malloc n 'raw))))

(define (free-ptr p) ($io (lambda () (free p) MkUnit)))

;; --- null + arithmetic (pure) --------------------------------------

(define null-ptr #f)                          ; Racket models NULL as #f
(define (ptr-null? p) (eq? p #f))
(define/curried (ptr-plus p n) (ptr-add p n)) ; byte offset

;; --- typed peek / poke (IO) ----------------------------------------

(define (peek-int p)        ($io (lambda () (ptr-ref p _int))))
(define/curried (poke-int p v)
  ($io (lambda () (ptr-set! p _int v) MkUnit)))

(define (peek-double p)     ($io (lambda () (ptr-ref p _double))))
(define/curried (poke-double p v)
  ($io (lambda () (ptr-set! p _double v) MkUnit)))

(define (peek-byte p)       ($io (lambda () (ptr-ref p _byte))))
(define/curried (poke-byte p v)
  ($io (lambda () (ptr-set! p _byte v) MkUnit)))

;; --- C strings (IO) ------------------------------------------------

;; copy a String into a freshly malloc'd, NUL-terminated buffer.
(define (string->c-string s)
  ($io (lambda ()
         (define bs (string->bytes/utf-8 s))
         (define n (bytes-length bs))
         (define p (malloc (+ n 1) 'raw))
         (memcpy p bs n)
         (ptr-set! p _byte n 0)
         p)))

;; read a NUL-terminated UTF-8 C string back into a String.
(define (c-string->string p)
  ($io (lambda () (cast p _pointer _string/utf-8))))

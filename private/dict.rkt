#lang racket/base

;; Rackton — class-method runtime dispatch.
;;
;; Each class C with methods m1, m2, … is compiled to:
;;   - a hash table `$dispatch:C`, mapping runtime "type tags" → method impls
;;   - one generic function per method, which dispatches on the first
;;     argument's tag and applies the matching impl.
;;
;; Each instance compiles to a sequence of `register-instance-method!` calls
;; that populate the table.  For ADT instance heads (e.g. `(Eq (Maybe a))`)
;; one entry is registered per data constructor of the head's type — so a
;; recursive method call inside the impl dispatches the same way regardless
;; of which constructor it sees.

(provide define-class-method
         register-instance-method!
         dispatch-tag
         rackton-no-instance-error)

(require racket/struct
         (for-syntax racket/base
                     syntax/parse))

;; Map a runtime value to a tag symbol that uniquely identifies its type.
;; Primitive types use their declared type name; structures use the symbol
;; returned by `struct-type-info`, which matches the `$ctor:Foo` names
;; emitted by `define-data-ctor`.
(define (dispatch-tag v)
  (cond
    [(exact-integer? v) 'Integer]
    [(boolean? v)       'Boolean]
    [(string? v)        'String]
    [(struct? v)
     (define-values (st _skip) (struct-info v))
     (define-values (name _i _a _r _mu _im _pa _x) (struct-type-info st))
     name]
    [else
     (error 'dispatch-tag "no tag for value: ~v" v)]))

(define (rackton-no-instance-error method-name v)
  (error method-name "no instance for: ~v" v))

;; A generic-method definition.  Given a class's dispatch table, declares
;; the named method to look up by first-argument tag.
(define-syntax (define-class-method stx)
  (syntax-parse stx
    [(_ method:id table:id)
     #'(define (method x . rest)
         (define impl
           (hash-ref table (dispatch-tag x)
                     (lambda ()
                       (rackton-no-instance-error 'method x))))
         (apply impl x rest))]))

(define (register-instance-method! table tag impl)
  (hash-set! table tag impl))

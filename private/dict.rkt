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
         define/curried
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
    [(inexact-real? v)  'Float]
    [(boolean? v)       'Boolean]
    [(string? v)        'String]
    [(char? v)          'Char]
    [(bytes? v)         'Bytes]
    [(struct? v)
     (define-values (st _skip) (struct-info v))
     (define-values (name _i _a _r _mu _im _pa _x) (struct-type-info st))
     name]
    [else
     (error 'dispatch-tag "no tag for value: ~v" v)]))

(define (rackton-no-instance-error method-name v)
  (error method-name "no instance for: ~v" v))

;; A generic-method definition.  Given a class's dispatch table, the
;; argument index whose runtime tag selects the instance, and the
;; method's total arity, declares the named method as a function that
;; accepts either the full arity directly OR fewer arguments — in the
;; partial case it returns a closure that collects the rest.  Once all
;; `arity` args are present, the method dispatches on the value at
;; `pos` and applies the impl.  Backwards-compatible defaults: pos=0
;; and arity=(add1 pos) (single-arg dispatch with one expected arg).
(define-syntax (define-class-method stx)
  (syntax-parse stx
    [(_ method:id table:id
        (~optional pos:nat   #:defaults ([pos   #'0]))
        (~optional arity:nat #:defaults ([arity #'1])))
     #'(define method
         (curry-class-method 'method table 'pos 'arity '()))]))

;; Build a method-callable that collects up to `arity` args, then
;; dispatches on position `pos` and applies the instance impl.  Called
;; recursively with each partial application to remember the args so
;; far.
(define (curry-class-method name table pos arity acc)
  (cond
    [(>= (length acc) arity)
     (dispatch-and-apply name table pos acc)]
    [else
     ;; Accept any number of further args at the call site.  If we
     ;; reach `arity` we dispatch; otherwise we go around again with
     ;; the extended accumulator.
     (lambda new-args
       (cond
         [(null? new-args)
          ;; (f) — treat as identity (return the partial callable)
          (curry-class-method name table pos arity acc)]
         [else
          (curry-class-method name table pos arity
                              (append acc new-args))]))]))

(define (dispatch-and-apply name table pos args)
  (define tagger (list-ref args pos))
  (define impl
    (hash-ref table (dispatch-tag tagger)
              (lambda ()
                (rackton-no-instance-error name tagger))))
  (apply impl args))

(define (register-instance-method! table tag impl)
  (hash-set! table tag impl))

;; Define a function that accepts EITHER its full arity at once OR any
;; shorter prefix, returning a closure that collects the rest.  This is
;; the hand-written counterpart to the curried `e:lam` compilation in
;; private/codegen.rkt — used for Racket-side prelude functions that
;; previously took multiple args directly via `(define (f x y) body)`.
;; The full-arity clause is listed first so common-case calls hit it
;; without paying the cost of closure allocation.
(define-syntax (define/curried stx)
  (syntax-parse stx
    [(_ (name:id arg:id ...+) body ...+)
     (define args (syntax->list #'(arg ...)))
     (define n (length args))
     (cond
       [(<= n 1)
        #'(define (name arg ...) body ...)]
       [else
        (with-syntax ([(clause ...)
                       (build-curry-clauses args #'(begin body ...))])
          #'(define name (case-lambda clause ...)))])]))

(define-for-syntax (build-curry-clauses args body)
  ;; Emit one clause per prefix length 1..n.  Full-arity clause runs
  ;; `body`; shorter prefixes return a nested curried function over the
  ;; remaining args.
  (define n (length args))
  (for/list ([k (in-range 1 (add1 n))])
    (define prefix (take-list args k))
    (define rest   (drop-list args k))
    (cond
      [(null? rest)
       (with-syntax ([(p ...) prefix]
                     [bdy body])
         #'[(p ...) bdy])]
      [else
       (with-syntax ([(p ...) prefix]
                     [inner (build-nested-curried rest body)])
         #'[(p ...) inner])])))

(define-for-syntax (build-nested-curried rest body)
  ;; The "rest" of args (those still missing) build a fresh
  ;; case-lambda so the partial-result can itself be further
  ;; partially-applied.
  (define m (length rest))
  (cond
    [(= m 1)
     (with-syntax ([(p) rest]
                   [bdy body])
       #'(lambda (p) bdy))]
    [else
     (with-syntax ([(clause ...) (build-curry-clauses rest body)])
       #'(case-lambda clause ...))]))

(define-for-syntax (take-list xs n)
  (cond [(or (zero? n) (null? xs)) '()]
        [else (cons (car xs) (take-list (cdr xs) (sub1 n)))]))

(define-for-syntax (drop-list xs n)
  (cond [(or (zero? n) (null? xs)) xs]
        [else (drop-list (cdr xs) (sub1 n))]))

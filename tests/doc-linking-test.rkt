#lang racket/base

;; tests/doc-linking-test.rkt — every name documented as a surface
;; form, provide spec, type, class, or return-typed method in the
;; reference must be bound in the `rackton` module so that Scribble's
;; `for-label` cross-references resolve.  Without this, an
;; @racket[name] markup in the guide renders as plain text instead of
;; a hyperlink to the reference entry.
;;
;; This is the dual of doc-coverage-test.rkt: that test asserts every
;; export is documented; this one asserts every documented language
;; identifier is exported.

(require racket/string
         rackunit)

(dynamic-require 'rackton #f)

(define rackton-bindings
  (let-values ([(vals stxs) (module->exports 'rackton)])
    (define h (make-hasheq))
    (for ([bucket (in-list (list vals stxs))])
      (for ([phase-row (in-list bucket)])
        (for ([entry (in-list (cdr phase-row))])
          (hash-set! h (car entry) #t))))
    h))

(define (bound? sym) (hash-ref rackton-bindings sym #f))

;; Hand-maintained categorisations of names that must link.  Keep
;; these in sync with the reference's @defform / @defidform / @defthing
;; entries; the doc-coverage test enforces the opposite direction.

(define surface-forms
  '(define : data newtype struct
     protocol instance define-alias define-effect
     lambda λ let let& let% let+ letrec match-let where
     if cond match do list ann update escape racket handle
     require provide foreign
     All))

(define provide-specs
  '(all-defined-out all-from-out data-out struct-out protocol-out rename-out except-out))

(define type-ctors
  '(Integer Float Rational Complex Boolean String Char Bytes Unit
    Maybe List Pair Result
    IO Ref MVar Chan ThreadId Future TVar STM
    Identity
    State Env StateT EnvT WriterT ExceptT
    Map Set))

(define classes
  '(Eq Ord Num Fractional Integral Real Floating RealFrac RealFloat
    Show
    Functor Applicative Monad Foldable Traversable Bifunctor
    Semigroup Monoid
    MonadState MonadEnv MonadWriter MonadError Concurrent))

(define return-typed-methods
  '(pure mempty get-st ask-en await-c yield-c pi))

(define (check-all-bound category names)
  (define unbound (filter (lambda (n) (not (bound? n))) names))
  (unless (null? unbound)
    (fail-check
     (format "~a ~a not bound in rackton (so @racket[name] won't link):~n  ~a"
             (length unbound)
             category
             (string-join (map symbol->string unbound) "\n  ")))))

(test-case "every surface form is bound in rackton"
  (check-all-bound "surface form(s)" surface-forms))

(test-case "every provide spec is bound in rackton"
  (check-all-bound "provide spec(s)" provide-specs))

(test-case "every type constructor is bound in rackton"
  (check-all-bound "type constructor(s)" type-ctors))

(test-case "every class is bound in rackton"
  (check-all-bound "class(es)" classes))

(test-case "every return-typed method is bound in rackton"
  (check-all-bound "return-typed method(s)" return-typed-methods))

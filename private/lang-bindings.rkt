#lang racket/base

;; private/lang-bindings.rkt — identifier stubs for every name in the
;; Rackton language that has no other Racket-level binding.
;;
;; Why these exist: Rackton's surface forms (protocol, data,
;; …), type constructors (Maybe, IO, …), classes (Eq, Functor, …), and
;; return-typed methods (pure, mempty, …) are not Racket macros or
;; values — they are recognised by the surface parser in
;; private/surface.rkt and resolved by inference / codegen.  But the
;; documentation refers to them with @racket[name], and Scribble only
;; turns @racket[name] into a hyperlink when `name` is bound in a
;; module listed in (for-label …).  Without a binding the markup
;; renders as plain text, even though the reference has a matching
;; @defform / @defidform entry.
;;
;; Each stub raises a precise syntax error if it slips out of a
;; (rackton …) form into raw Racket — which is a clearer message than
;; the "unbound identifier" we used to produce.  Inside a (rackton …)
;; the surface parser uses #:datum-literals so the stub binding is
;; never expanded; control reaches inference and codegen unaltered.

(require (for-syntax racket/base))

;; ----- helper: stub macros that error if used as expression --------

(define-syntax (define-rackton-form stx)
  (syntax-case stx ()
    [(_ name kind-string)
     (with-syntax ([msg (format "Rackton ~a; must appear inside a (rackton …) form or a #lang rackton module"
                                (syntax->datum #'kind-string))])
       #'(begin
           (provide name)
           (define-syntax (name use-stx)
             (raise-syntax-error 'name msg use-stx))))]))

;; ----- surface forms -----------------------------------------------

;; Toplevel declarations.
(define-rackton-form : "type ascription")
(define-rackton-form data "data type declaration")
(define-rackton-form newtype "newtype declaration")
(define-rackton-form protocol "type class (protocol) declaration")
(define-rackton-form instance "instance declaration")
(define-rackton-form define-alias "type alias declaration")
(define-rackton-form define-effect "algebraic effect declaration")

;; Expressions and binding forms not provided by racket/base.
;; (`let*` — the sequential binding form — IS a racket/base name, so it
;; needs no stub here; it binds via the racket/base re-export.)
(define-rackton-form foreign "host (FFI) import")
(define-rackton-form foreign-c "inline C-function (FFI) import")
(define-rackton-form let& "sequential monadic binding")
(define-rackton-form let% "parallel monadic binding")
(define-rackton-form let+ "applicative binding")
(define-rackton-form ann "type ascription")
(define-rackton-form update "functional record update")
(define-rackton-form escape "internal escape form")
(define-rackton-form racket "host-language escape")
(define-rackton-form handle "effect handler")
(define-rackton-form delay "deferred (call-by-need) computation")

;; Type-level keywords (appear inside type signatures).
(define-rackton-form All "universal quantifier")

;; ----- provide specs -----------------------------------------------

(define-rackton-form data-out "provide spec exporting a data type with its constructors")
(define-rackton-form protocol-out "provide spec exporting a protocol with its methods")

;; ----- type constructors -------------------------------------------

(define-syntax (define-rackton-type stx)
  (syntax-case stx ()
    [(_ name)
     #'(begin
         (provide name)
         (define-syntax (name use-stx)
           (raise-syntax-error 'name
                               "Rackton type constructor; not a value — use only in type signatures inside (rackton …)"
                               use-stx)))]))

(define-syntax-rule (define-rackton-types name ...)
  (begin (define-rackton-type name) ...))

;; Types whose constructor now SHARES the type name (Coalton/Haskell
;; style — `(data (Pair a b) (Pair a b))`) are intentionally absent here:
;; the constructor's own binding is the for-label target, so a stub would
;; be a duplicate binding.  Only types with NO same-named constructor
;; (erased types, or distinct ctor names like Maybe/List) get a stub.
(define-rackton-types
  ;; Primitive types
  Integer Float Rational Complex Boolean String Char Bytes
  ;; Sum / product types (distinct ctor names)
  Maybe List Result
  ;; IO, refs, concurrency
  IO Ref MVar Chan ThreadId Future TVar STM
  ;; Containers
  Map Set
  ;; Raw memory (rackton/foreign/ptr)
  Ptr)

;; ----- classes -----------------------------------------------------

(define-syntax (define-rackton-class stx)
  (syntax-case stx ()
    [(_ name)
     #'(begin
         (provide name)
         (define-syntax (name use-stx)
           (raise-syntax-error 'name
                               "Rackton class name; not a value — use only in class declarations, instance heads, and constraint contexts inside (rackton …)"
                               use-stx)))]))

(define-syntax-rule (define-rackton-classes name ...)
  (begin (define-rackton-class name) ...))

(define-rackton-classes
  ;; Equality and ordering
  Eq Ord
  ;; Numeric hierarchy
  Num Fractional Integral Real Floating RealFrac RealFloat
  ;; Display
  Show
  ;; Functor hierarchy
  Functor Applicative Monad
  ;; Folding and traversal
  Foldable Traversable Bifunctor
  ;; Semigroup / monoid
  Semigroup Monoid
  ;; MTL-style monadic classes
  MonadState MonadEnv MonadWriter MonadError Concurrent
  ;; transformer / IO lifting
  MonadTrans MonadIO
  ;; raw memory
  Storable)

;; ----- return-typed class methods ----------------------------------

;; These methods have no runtime binding: the elaborator resolves each
;; call site to a per-instance impl name like |$pure:Maybe| before
;; codegen.  Stubs exist solely so docs can link to them.

(define-syntax (define-rackton-method stx)
  (syntax-case stx ()
    [(_ name)
     #'(begin
         (provide name)
         (define-syntax (name use-stx)
           (raise-syntax-error 'name
                               "Rackton class method; resolved at type-check time — use only inside (rackton …) where its return type can be inferred"
                               use-stx)))]))

(define-syntax-rule (define-rackton-methods name ...)
  (begin (define-rackton-method name) ...))

(define-rackton-methods
  ;; Applicative / Monad
  pure
  ;; Monoid
  mempty
  ;; MonadState
  get-st
  ;; MonadEnv
  ask-en
  ;; Concurrent
  await-c yield-c
  ;; MonadTrans / MonadIO
  lift lift-io
  ;; Storable (peek is return-typed; poke is an ordinary dispatched method)
  peek
  ;; Floating
  pi)

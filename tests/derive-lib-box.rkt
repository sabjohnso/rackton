#lang rackton

;; A library that defines a Monad via `:derive-supers`.  Only the
;; type and its constructor are exported; the synthesized Functor and
;; Applicative instances must escape (like any instance) so a client can
;; call the derived `fmap`/`fapply`/`pure` on `DBox` values.

(data (DBox a) (MkDBox a))

(instance (Monad DBox) :derive-supers
  (define (pure x)      (MkDBox x))
  (define (flatmap f b) (match b [(MkDBox x) (f x)])))

(provide (data-out DBox))

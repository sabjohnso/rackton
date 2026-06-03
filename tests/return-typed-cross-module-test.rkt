#lang racket/base

;; Enabler A regression: call return-typed methods (`pure`, `mempty`) on
;; instances defined in ANOTHER module.  Before Enabler A this raised a
;; codegen-time `$pure:Box: unbound identifier`.

(require rackunit
         "../main.rkt")

(rackton
  (require "return-typed-cross-module-lib.rkt")

  ;; return-typed `pure` resolves from the expected type (Box Integer)
  (: made (Box Integer))
  (define made (pure 5))

  ;; return-typed `mempty` resolves from the expected type (Wrap Integer)
  (: empty-wrap (Wrap Integer))
  (define empty-wrap mempty)

  (: combined (Wrap Integer))
  (define combined (mappend (MkWrap (Cons 1 (Cons 2 Nil))) empty-wrap)))

(test-case "cross-module return-typed pure"
  (check-equal? (unbox-it made) 5))

(test-case "cross-module return-typed mempty + positional mappend"
  (check-equal? (unwrap empty-wrap) Nil)
  (check-equal? (unwrap combined) (Cons 1 (Cons 2 Nil))))

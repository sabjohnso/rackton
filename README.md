# Rackton

A Racket adaptation of the [Coalton](https://github.com/coalton-lang/coalton)
statically-typed functional language.  Embeds a Hindley–Milner core — type
inference, let-polymorphism, algebraic data types, pattern matching — inside
Racket.

## Status

**Phase 1 + 2 + 3** — typed lambda calculus, ADTs, pattern matching, two
surfaces (`(rackton ...)` macro and `#lang rackton`), type classes with
superclass constraints / qualifying contexts / default methods / runtime
dispatch, host-language `racket` escape, a built-in prelude
(`Eq`/`Ord`/`Num`/`Show` + `Maybe`/`List`/`Result`/`Pair`/`Unit`),
fatal exhaustiveness checking, and pretty-printed error messages.

## Quick start

Inside a regular Racket module:

```racket
#lang racket/base
(require rackton)

(rackton
  (define-data (Maybe a) None (Some a))

  (: from-maybe (-> a (-> (Maybe a) a)))
  (define (from-maybe d m)
    (match m
      [(None)   d]
      [(Some x) x])))
```

…or as an entire module:

```racket
#lang rackton

(define (fact n)
  (if (= n 0) 1 (* n (fact (- n 1)))))
```

## Building & testing

```bash
raco pkg install --link --auto       # install once
raco test -p rackton                  # run the full test suite
raco scribble --html scribblings/rackton.scrbl    # build docs
```

## Layout

```
private/      type AST, unifier, env, surface parser,
              inference, ADT runtime, pattern compiler,
              codegen, elaborator
lang/         #lang rackton reader & lang module
tests/        end-to-end / typecheck-error / #lang rackton sample
scribblings/  reference docs
```

## License

Dual-licensed under Apache-2.0 OR MIT.

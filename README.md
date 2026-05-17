# Rackton

A Racket adaptation of the [Coalton](https://github.com/coalton-lang/coalton)
statically-typed functional language.  Embeds a Hindley–Milner core — type
inference, let-polymorphism, algebraic data types, pattern matching — inside
Racket.

## Status

**Phase 1 + 2 + 3 + 4 + 5** — typed lambda calculus, ADTs, pattern
matching, two surfaces (`(rackton ...)` macro and `#lang rackton`),
type classes (superclass constraints, qualifying contexts, default
methods, single dispatch on the first argument whose type mentions a
class parameter), host-language `racket` escape, a built-in prelude
(`Eq`/`Ord`/`Num`/`Show`/`Functor`/`Monad` + `Maybe`/`List`/`Result`/
`Pair`/`Unit` + `id`/`const`/`compose`/`flip` + `not`/`and`/`or` +
`length`/`foldr`/`filter`), explicit-kind higher-kinded classes,
fatal exhaustiveness checking, pretty-printed error messages,
`#:deriving Eq Show` on ADTs, do-notation for any Monad, and
multi-file imports that carry bindings, data types, classes, and
instances across `#lang rackton` files via a sidecar submodule.

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

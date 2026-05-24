# Rackton

A Racket adaptation of the [Coalton](https://github.com/coalton-lang/coalton)
statically-typed functional language.  Embeds a Hindley–Milner core — type
inference, let-polymorphism, algebraic data types, pattern matching — inside
Racket.

## Status

**Phase 1 + 2 + 3 + 4 + 5 + 6 + 7 + 8 + 9 + 10 + 11 + 12 + 13 + 14 +
15 + 16 + 17 + 18 + 19 + 20 + 21 + 22 + 23 + 24 + 25 + 26 + 27 + 28 + 29 + 30 + 31 + 32 + 33 + 34 + 35 + 36 + 37 + 38 + 39 + 40 + 41 + 42 + 43 + 44 + 45 + 46 + 47 + 48 + 49 + 50 + 51 + 52 + 53** — typed lambda
calculus, ADTs, records (`define-struct`), pattern matching, `letrec`,
type aliases (`define-alias`), immutable `(Map k v)` and `(Set a)`
containers, `Float` with real arithmetic and `Fractional` class,
structured error recovery (`try` / `raise-io`), two surfaces
(`(rackton ...)` macro and `#lang rackton`), single- and multi-parameter
type classes (superclass constraints, qualifying contexts, default
methods, explicit-kind higher-kinded classes, first-arg runtime
dispatch), host-language `racket` escape, a built-in prelude
(`Eq`/`Ord`/`Num`/`Show`/`Functor`/`Applicative`/`Monad`/`Bifunctor`/`Foldable` + `Maybe`/`List`/`Result`/
`Pair`/`Unit`/`IO`/`Ref` + `id`/`const`/`compose`/`flip` +
`not`/`and`/`or` + `length`/`foldr`/`filter`/`reverse`/`append`/
`zip`/`take`/`drop`/`find`/`sort`/`concat-map`/`group-by` +
`fst`/`snd`/`swap` + string ops + numeric helpers +
IO/file/ref/Map/Set primitives + `panic`), fatal
exhaustiveness checking, pretty-printed error messages with
"did-you-mean?" suggestions for unbound identifiers, polymorphic
recursion via declared schemes, `#:deriving Eq Ord Show Functor` on
ADTs, do-notation for any Monad, multiple `(rackton ...)` blocks per
Racket module, and multi-file imports that carry bindings, data types,
records, classes, and instances across `#lang rackton` files via a
sidecar submodule.

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
examples/     calc.rkt — a small expression interpreter
              demonstrating ADTs, Map, Result, IO, mutual
              recursion
scribblings/  reference docs
```

Run the calc REPL:

```bash
racket examples/calc.rkt
```

Or the todo CLI:

```bash
racket examples/todo.rkt add buy milk
racket examples/todo.rkt list
```

## License

Dual-licensed under Apache-2.0 OR MIT.

# Rackton

A Racket adaptation of the [Coalton](https://github.com/coalton-lang/coalton)
statically-typed functional language.  Embeds a Hindley–Milner core — type
inference, let-polymorphism, algebraic data types, pattern matching — inside
Racket.

## Status

Rackton is usable as a small typed functional language embedded in
Racket.  Highlights:

- **Type system** — Hindley–Milner with let-polymorphism, polymorphic
  recursion via declared schemes, full GADTs with skolem refinement,
  rank-N polymorphism (`All`), existential types, type aliases, and
  associated types (type families).
- **Data** — ADTs, records (`struct`) with polymorphic field
  update, sealed abstract types, pattern matching with guards and
  `match-let`, fatal exhaustiveness checking, `#:deriving` for `Eq`,
  `Ord`, `Show`, `Functor`, `Foldable`, `Traversable`, `Bifunctor`,
  `Semigroup`, `Monoid`, `Lens`, and `Prism`.
- **Type classes** — single- and multi-parameter classes, superclass
  constraints, qualifying contexts, default methods, functional
  dependencies, overlapping instances, explicit-kind HKTs, first-arg
  runtime dispatch with compile-time monomorphization plus inlining
  of small impls, module-level coherence.
- **Prelude** — `Eq`/`Ord`/`Num`/`Fractional`/`Integral`/`Show`,
  `Functor`/`Applicative`/`Monad`/`Bifunctor`/`Foldable`/`Traversable`,
  `Semigroup`/`Monoid`, MTL-style `MonadState`/`MonadEnv`/`MonadWriter`/
  `MonadError`, the corresponding transformers (`StateT`, `EnvT`,
  `WriterT`, `ExceptT`, `Identity`), `Maybe`/`List`/`Result`/`Pair`/
  `Unit`/`IO`/`Ref`, lens/prism/traversal combinators, a full numeric
  tower (`Integer`/`Float`/`Rational`/`Complex`/`Char`/`Bytes`),
  string/list/IO/file/Map/Set primitives, and `panic`.
- **Effects & concurrency** — `do`-notation for any `Monad`, structured
  error recovery (`try` / `raise-io`), algebraic effects + handlers,
  threads/MVars/channels, STM (`TVar`/`atomically`/`retry`), and a
  polymorphic `Concurrent` class.
- **Foreign function interface** — `foreign` imports a host (Racket)
  binding under a Rackton type; `foreign-c` binds an external C
  function via `get-ffi-obj`; `rackton/foreign/ptr` provides an unsafe
  `Ptr` core (malloc / peek / poke / free) with the `Storable` class
  for polymorphic `peek`/`poke`. The declared type is an unchecked
  trust boundary — these are deliberately unsafe.
- **Surfaces** — embedded `(rackton ...)` macro (multiple blocks per
  Racket module) and whole-module `#lang rackton`, with multi-file
  imports that carry bindings, data types, records, classes, and
  instances across `#lang rackton` files via a sidecar submodule.
- **Tooling** — interactive REPL (`racket -l rackton/repl`) with
  multi-line input, history, and tab completion; pretty-printed type
  errors with expected/got blame and did-you-mean suggestions; a
  host-language `racket` escape for trapdoors back into Racket.

## Quick start

Inside a regular Racket module:

```racket
#lang racket/base
(require rackton)

(rackton
  (data (Maybe a) None (Some a))

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
raco setup --check-pkg-deps rackton   # compile + dep check + build docs
```

## Documentation

The documentation is split into three documents, each addressing a
different audience:

- **Guide** (`scribblings/guide/`) — narrative introduction, organised
  from quickstart to advanced features.  Start here if you're new to
  Rackton.
- **Reference** (`scribblings/reference/`) — exhaustive API
  reference.  Every prelude binding, every surface form, every type
  class.  100% coverage is enforced by
  `tests/doc-coverage-test.rkt`.
- **Developer** (`scribblings/developer/`) — architecture, the
  compilation pipeline, theoretical foundations, bibliography.  For
  maintainers.

Read them online at the Racket package docs server, or build locally
with `raco setup rackton`.

## Layout

```
private/      type AST, unifier, env, surface parser,
              inference, ADT runtime, pattern compiler,
              codegen, elaborator
lang/         #lang rackton reader & lang module
tests/        per-feature tests + cross-cutting end-to-end
              + doc-coverage-test.rkt
examples/     calc.rkt (typed interpreter), todo.rkt (CLI)
scribblings/  rackton.scrbl (landing) + guide/ + reference/
              + developer/
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

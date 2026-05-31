#lang rackton

;; rackton/batteries — umbrella module re-exporting the whole standard
;; library, for users who prefer one import over managing per-module
;; requires.  As the library grows (Phase 2 carves + Phases 3-8), each
;; new stdlib module is added here.  Writable thanks to (all-from-out …)
;; (Enabler C).

(require rackton/data/maybe
         rackton/data/either
         rackton/data/char
         rackton/data/bool
         rackton/data/function
         rackton/data/ord
         rackton/data/functor
         rackton/data/foldable
         rackton/data/traversable
         rackton/data/complex
         rackton/data/ratio
         rackton/data/list/nonempty
         rackton/data/monoid
         rackton/data/semigroup
         rackton/data/bits
         rackton/data/lens
         rackton/data/list
         rackton/data/tuple
         rackton/data/map
         rackton/data/set
         rackton/control/applicative
         rackton/control/monad
         rackton/control/stm
         rackton/control/concurrent
         rackton/control/monad/state
         rackton/control/monad/reader
         rackton/control/monad/writer
         rackton/control/monad/except
         rackton/control/monad/trans
         rackton/numeric/integer
         rackton/numeric/real
         rackton/numeric/natural
         rackton/numeric/show
         rackton/numeric/conversions
         rackton/system
         rackton/text/string
         rackton/text/printf
         rackton/text/read)

(provide (all-from-out rackton/data/maybe)
         (all-from-out rackton/data/either)
         (all-from-out rackton/data/char)
         (all-from-out rackton/data/bool)
         (all-from-out rackton/data/function)
         (all-from-out rackton/data/ord)
         (all-from-out rackton/data/functor)
         (all-from-out rackton/data/foldable)
         (all-from-out rackton/data/traversable)
         (all-from-out rackton/data/complex)
         (all-from-out rackton/data/ratio)
         (all-from-out rackton/data/list/nonempty)
         (all-from-out rackton/data/monoid)
         (all-from-out rackton/data/semigroup)
         (all-from-out rackton/data/bits)
         (all-from-out rackton/data/lens)
         (all-from-out rackton/data/list)
         (all-from-out rackton/data/tuple)
         (all-from-out rackton/data/map)
         (all-from-out rackton/data/set)
         (all-from-out rackton/control/applicative)
         (all-from-out rackton/control/monad)
         (all-from-out rackton/control/stm)
         (all-from-out rackton/control/concurrent)
         (all-from-out rackton/control/monad/state)
         (all-from-out rackton/control/monad/reader)
         (all-from-out rackton/control/monad/writer)
         (all-from-out rackton/control/monad/except)
         (all-from-out rackton/control/monad/trans)
         (all-from-out rackton/numeric/integer)
         (all-from-out rackton/numeric/real)
         (all-from-out rackton/numeric/natural)
         (all-from-out rackton/numeric/show)
         (all-from-out rackton/numeric/conversions)
         (all-from-out rackton/text/string)
         (all-from-out rackton/text/printf)
         (all-from-out rackton/text/read)
         (all-from-out rackton/system))

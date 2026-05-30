#lang rackton

;; rackton/system/random — System.Random.  Pseudo-random number
;; generation in IO.  The runtime primitives live in
;; private/prelude-runtime and are reached via `foreign`.

(provide (all-defined-out))

;; random-integer lo hi: a uniform random Integer in the half-open
;; range [lo, hi) (hi exclusive; hi must be greater than lo).
(foreign random-integer (-> Integer (-> Integer (IO Integer)))
         #:from rackton/private/prelude-runtime)

;; random-float: a uniform random Float in [0, 1).
(foreign random-float (IO Float)
         #:from rackton/private/prelude-runtime)

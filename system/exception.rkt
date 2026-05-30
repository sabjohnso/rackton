#lang rackton

;; rackton/system/exception — exceptions in IO (Haskell's
;; Control.Exception / System.IO.Error).  `try` reifies a raised error
;; as a Result; `raise-io` throws one.  The runtime primitives live in
;; private/prelude-runtime and are reached via `foreign`.

(provide (all-defined-out))

;; try: run an action, catching any raised error as (Err message).
(foreign try (-> (IO a) (IO (Result String a)))
         #:from rackton/private/prelude-runtime)

;; raise-io: throw an error with the given message.
(foreign raise-io (-> String (IO a))
         #:from rackton/private/prelude-runtime)

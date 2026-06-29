#lang rackton

;; rackton/system/exception — exceptions in IO (Haskell's
;; Control.Exception / System.IO.Error).  `try` reifies a raised error
;; as a (stdlib) Result; `raise-io` throws one.  The runtime primitive
;; (private/prelude-runtime) reifies a raised error as the prelude
;; Either (Left message / Right value); `try` maps that into Result so
;; callers get Ok/Err naming.

(require rackton/data/result)
(provide try raise-io)

;; Low-level runtime primitive: success / failure as the prelude Either.
(foreign raw-try (-> (IO a) (IO (Either String a)))
         :from rackton/private/prelude-runtime :as try)

;; try: run an action, catching any raised error as (Err message).
(: try (-> (IO a) (IO (Result String a))))
(define (try io) (fmap either->result (raw-try io)))

;; raise-io: throw an error with the given message.
(foreign raise-io (-> String (IO a))
         :from rackton/private/prelude-runtime)

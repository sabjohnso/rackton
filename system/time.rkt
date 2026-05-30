#lang rackton

;; rackton/system/time — wall-clock access.  The runtime primitive
;; lives in private/prelude-runtime and is reached via `foreign`.

(provide (all-defined-out))

;; current-time-seconds: seconds since the Unix epoch.
(foreign current-time-seconds (IO Integer)
         #:from rackton/private/prelude-runtime)

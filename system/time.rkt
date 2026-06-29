#lang rackton

;; rackton/system/time — wall-clock access.  The runtime primitive
;; lives in private/prelude-runtime and is reached via `foreign`.

(provide (all-defined-out))

;; current-time-seconds: seconds since the Unix epoch.
(foreign current-time-seconds (IO Integer)
         :from rackton/private/prelude-runtime)

;; get-current-time-millis: wall-clock milliseconds since the Unix
;; epoch (finer-grained than current-time-seconds).
(foreign get-current-time-millis (IO Integer)
         :from rackton/private/prelude-runtime)

;; get-cpu-time-millis: CPU milliseconds consumed by this process
;; (Haskell's getCPUTime, in ms rather than picoseconds).
(foreign get-cpu-time-millis (IO Integer)
         :from rackton/private/prelude-runtime)

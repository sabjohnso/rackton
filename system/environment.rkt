#lang rackton

;; rackton/system/environment — System.Environment.  Process
;; environment and command-line access.  The runtime primitives live in
;; private/prelude-runtime and are reached via `foreign`.

(provide (all-defined-out))

;; lookupEnv: the value of an environment variable, or None if unset.
(foreign getenv (-> String (IO (Maybe String)))
         #:from rackton/private/prelude-runtime)

;; getArgs: the command-line arguments.
(foreign argv (IO (List String))
         #:from rackton/private/prelude-runtime)

;; getProgName: the running program's name (without directory).
(foreign get-prog-name (IO String)
         #:from rackton/private/prelude-runtime)

;; setEnv: set an environment variable.
(foreign set-env (-> String (-> String (IO Unit)))
         #:from rackton/private/prelude-runtime)

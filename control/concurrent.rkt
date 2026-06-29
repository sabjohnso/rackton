#lang rackton

;; rackton/control/concurrent — Control.Concurrent.  Threads, MVars, and
;; channels, moved out of the auto-prelude (Phase 2 slim).  Thin wrappers
;; over Racket's threads + semaphores + async channels; the runtime stays
;; in private/prelude-runtime and is reached via `foreign`.  (The
;; `Concurrent` class and its IO/Identity instances + `Future` stay in
;; the prelude.)

(provide (all-defined-out))

(data ThreadId)
(data (MVar a))
(data (Chan a))

(foreign fork-io        (-> (IO a) (IO ThreadId))
         :from rackton/private/prelude-runtime)
(foreign wait-thread    (-> ThreadId (IO Unit))
         :from rackton/private/prelude-runtime)
(foreign new-mvar       (-> a (IO (MVar a)))
         :from rackton/private/prelude-runtime)
(foreign new-empty-mvar (IO (MVar a))
         :from rackton/private/prelude-runtime)
(foreign take-mvar      (-> (MVar a) (IO a))
         :from rackton/private/prelude-runtime)
(foreign put-mvar       (-> (MVar a) (-> a (IO Unit)))
         :from rackton/private/prelude-runtime)
(foreign read-mvar      (-> (MVar a) (IO a))
         :from rackton/private/prelude-runtime)
(foreign modify-mvar    (-> (MVar a) (-> (-> a a) (IO Unit)))
         :from rackton/private/prelude-runtime)
(foreign new-chan       (IO (Chan a))
         :from rackton/private/prelude-runtime)
(foreign send-chan      (-> (Chan a) (-> a (IO Unit)))
         :from rackton/private/prelude-runtime)
(foreign recv-chan      (-> (Chan a) (IO a))
         :from rackton/private/prelude-runtime)

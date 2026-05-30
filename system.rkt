#lang rackton

;; rackton/system — System.* : mutable references, file I/O, exceptions,
;; and OS surface (random, time, environment, directories).  Moved out of
;; the auto-prelude (Phase 2 slim); all IO-based, no instances.  Runtime
;; stays in private/prelude-runtime, reached via `foreign`.  Core IO
;; (print/println/read-line/pure-io/run-io) stays in the prelude.

(provide (all-defined-out))

(data (Ref a))

;; --- mutable references ---
(foreign make-ref  (-> a (IO (Ref a)))
         #:from rackton/private/prelude-runtime)
(foreign read-ref  (-> (Ref a) (IO a))
         #:from rackton/private/prelude-runtime)
(foreign write-ref (-> (Ref a) (-> a (IO Unit)))
         #:from rackton/private/prelude-runtime)

;; --- file I/O ---
(foreign read-file      (-> String (IO String))
         #:from rackton/private/prelude-runtime)
(foreign write-file     (-> String (-> String (IO Unit)))
         #:from rackton/private/prelude-runtime)
(foreign file-exists?   (-> String (IO Boolean))
         #:from rackton/private/prelude-runtime)
(foreign delete-file    (-> String (IO Unit))
         #:from rackton/private/prelude-runtime)
(foreign make-directory (-> String (IO Unit))
         #:from rackton/private/prelude-runtime)
(foreign list-directory (-> String (IO (List String)))
         #:from rackton/private/prelude-runtime)

;; --- exceptions in IO ---
(foreign try      (-> (IO a) (IO (Result String a)))
         #:from rackton/private/prelude-runtime)
(foreign raise-io (-> String (IO a))
         #:from rackton/private/prelude-runtime)

;; --- OS surface ---
(foreign random-integer       (-> Integer (-> Integer (IO Integer)))
         #:from rackton/private/prelude-runtime)
(foreign random-float         (IO Float)
         #:from rackton/private/prelude-runtime)
(foreign current-time-seconds (IO Integer)
         #:from rackton/private/prelude-runtime)
(foreign getenv               (-> String (IO (Maybe String)))
         #:from rackton/private/prelude-runtime)
(foreign argv                 (IO (List String))
         #:from rackton/private/prelude-runtime)

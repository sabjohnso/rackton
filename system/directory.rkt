#lang rackton

;; rackton/system/directory — System.Directory.  Filesystem entries:
;; existence checks, deletion, directory creation and listing.  The
;; runtime primitives live in private/prelude-runtime and are reached
;; via `foreign`.

(provide (all-defined-out))

;; doesFileExist.
(foreign file-exists? (-> String (IO Boolean))
         #:from rackton/private/prelude-runtime)

;; removeFile.
(foreign delete-file (-> String (IO Unit))
         #:from rackton/private/prelude-runtime)

;; createDirectory.
(foreign make-directory (-> String (IO Unit))
         #:from rackton/private/prelude-runtime)

;; listDirectory.
(foreign list-directory (-> String (IO (List String)))
         #:from rackton/private/prelude-runtime)

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

;; doesDirectoryExist.
(foreign does-directory-exist? (-> String (IO Boolean))
         #:from rackton/private/prelude-runtime)

;; getCurrentDirectory.
(foreign get-current-directory (IO String)
         #:from rackton/private/prelude-runtime)

;; renameFile: move/rename, replacing an existing destination.
(foreign rename-file (-> String (-> String (IO Unit)))
         #:from rackton/private/prelude-runtime)

;; copyFile: copy contents, replacing an existing destination.
(foreign copy-file (-> String (-> String (IO Unit)))
         #:from rackton/private/prelude-runtime #:as copy-file-io)

;; createDirectoryIfMissing: create the directory and any parents,
;; with no error if it already exists.
(foreign create-directory-if-missing (-> String (IO Unit))
         #:from rackton/private/prelude-runtime)

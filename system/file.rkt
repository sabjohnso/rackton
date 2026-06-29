#lang rackton

;; rackton/system/file — whole-file I/O (Haskell's readFile / writeFile).
;; The runtime primitives live in private/prelude-runtime and are
;; reached via `foreign`.

(provide (all-defined-out))

;; readFile: the entire contents of a file as a String.
(foreign read-file (-> String (IO String))
         :from rackton/private/prelude-runtime)

;; writeFile: replace a file's contents with the given String.
(foreign write-file (-> String (-> String (IO Unit)))
         :from rackton/private/prelude-runtime)

;; appendFile: append to a file, creating it if it doesn't exist.
(foreign append-file (-> String (-> String (IO Unit)))
         :from rackton/private/prelude-runtime)

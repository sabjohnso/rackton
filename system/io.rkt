#lang rackton

;; rackton/system/io — System.IO.  Handle-based file and stream I/O.
;; The prelude already provides the standard-stream conveniences as
;; `print` (putStr), `println` (putStrLn), and `read-line` (getLine);
;; this module adds explicit handles.
;;
;; A `Handle` is opaque (a host port).  An operation that doesn't match
;; the handle's direction — e.g. `h-put-str` on a read handle — errors
;; at runtime, exactly as Haskell's does.  The runtime primitives live
;; in private/prelude-runtime and are reached via `foreign`.

(require rackton/system/exception)
(provide (all-defined-out))

;; Opaque handle type.
(data Handle)

;; The mode a file is opened in (Haskell's IOMode, minus ReadWriteMode).
(data IOMode
  ReadMode
  WriteMode
  AppendMode)

;; --- standard handles ----------------------------------------------

(foreign stdin  Handle #:from rackton/private/prelude-runtime)
(foreign stdout Handle #:from rackton/private/prelude-runtime)
(foreign stderr Handle #:from rackton/private/prelude-runtime)

;; --- opening / closing ---------------------------------------------

;; The host primitive takes an integer mode code; open-file maps the
;; IOMode to it so callers work with the typed constructors.
(foreign open-file-with-mode (-> String (-> Integer (IO Handle)))
         #:from rackton/private/prelude-runtime)

(: open-file (-> String (-> IOMode (IO Handle))))
(define (open-file path mode)
  (open-file-with-mode path
    (match mode
      [(ReadMode)   0]
      [(WriteMode)  1]
      [(AppendMode) 2])))

(foreign h-close (-> Handle (IO Unit))
         #:from rackton/private/prelude-runtime)

;; --- writing -------------------------------------------------------

(foreign h-put-str (-> Handle (-> String (IO Unit)))
         #:from rackton/private/prelude-runtime)

(foreign h-put-str-ln (-> Handle (-> String (IO Unit)))
         #:from rackton/private/prelude-runtime)

(foreign h-flush (-> Handle (IO Unit))
         #:from rackton/private/prelude-runtime)

;; --- reading -------------------------------------------------------

;; hGetContents: the rest of the handle's input as one String.
(foreign h-get-contents (-> Handle (IO String))
         #:from rackton/private/prelude-runtime)

;; hGetLine: the next line as (Some line), or None at end-of-file.
;; (Haskell's hGetLine throws at EOF; returning Maybe is safer and
;; matches the prelude's getenv convention.)
(foreign h-get-line (-> Handle (IO (Maybe String)))
         #:from rackton/private/prelude-runtime)

;; getContents: the rest of standard input as one String.
(: get-contents (IO String))
(define get-contents (h-get-contents stdin))

;; withFile: open a handle, run the action, and close the handle even
;; if the action raises (Haskell's withFile bracket).  Implemented over
;; `try` so the close always runs; a captured error is re-raised after.
(: with-file (-> String (-> IOMode (-> (-> Handle (IO r)) (IO r)))))
(define (with-file path mode action)
  (do [h <- (open-file path mode)]
      [r <- (try (action h))]
      [_ <- (h-close h)]
      (match r
        [(Ok v)  (pure v)]
        [(Err e) (raise-io e)])))

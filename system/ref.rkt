#lang rackton

;; rackton/system/ref — mutable references in IO (Haskell's
;; Data.IORef).  The Ref type is abstract; the runtime primitives live
;; in private/prelude-runtime and are reached via `foreign`.

(provide (all-defined-out))

(data (Ref a))

;; newIORef: allocate a reference holding the given value.
(foreign make-ref (-> a (IO (Ref a)))
         :from rackton/private/prelude-runtime)

;; readIORef: read the current value.
(foreign read-ref (-> (Ref a) (IO a))
         :from rackton/private/prelude-runtime)

;; writeIORef: replace the stored value.
(foreign write-ref (-> (Ref a) (-> a (IO Unit)))
         :from rackton/private/prelude-runtime)

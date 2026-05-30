#lang rackton

;; rackton/system — umbrella over the System.* family.  The flat module
;; was split (2026-05-30) into one module per Haskell System.* concern;
;; this umbrella re-exports all of them so `(require rackton/system)`
;; still brings the whole system interface — mutable references, file
;; and directory I/O, exceptions, random, time, and environment — in one
;; import, exactly as before the split.
;;
;; Prefer the specific module imports in library code (they make
;; dependencies explicit); the umbrella is handy for scripts.

(require rackton/system/ref
         rackton/system/file
         rackton/system/directory
         rackton/system/exception
         rackton/system/random
         rackton/system/time
         rackton/system/environment)

(provide (all-from-out rackton/system/ref)
         (all-from-out rackton/system/file)
         (all-from-out rackton/system/directory)
         (all-from-out rackton/system/exception)
         (all-from-out rackton/system/random)
         (all-from-out rackton/system/time)
         (all-from-out rackton/system/environment))

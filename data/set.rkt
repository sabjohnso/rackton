#lang rackton

;; rackton/data/set — Data.Set.  Immutable sets, moved out of the
;; auto-prelude (Phase 2 slim).  Runtime in private/containers-runtime
;; (Racket immutable hashes) reached via `foreign`; elements compare by
;; structural equality.

(provide (all-defined-out))

(data (Set a))

(foreign empty-set (Set a)
         #:from rackton/private/containers-runtime)
(foreign set-insert (-> a (-> (Set a) (Set a)))
         #:from rackton/private/containers-runtime)
(foreign set-member? (-> a (-> (Set a) Boolean))
         #:from rackton/private/containers-runtime)
(foreign set-delete (-> a (-> (Set a) (Set a)))
         #:from rackton/private/containers-runtime)
(foreign set-size (-> (Set a) Integer)
         #:from rackton/private/containers-runtime)
(foreign set-to-list (-> (Set a) (List a))
         #:from rackton/private/containers-runtime)

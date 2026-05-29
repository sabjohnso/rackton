#lang rackton

;; rackton/control/stm — Control.Concurrent.STM.  Software transactional
;; memory, moved out of the auto-prelude (Phase 2 slim).  The runtime
;; (transaction log, commit lock, version checks) stays in
;; private/prelude-runtime and is reached via `foreign`; STM is an opaque
;; type whose values carry the `$stm` runtime dispatch tag.

(provide (all-defined-out))

(data (TVar a))
(data (STM a) #:runtime-tag $stm)

;; --- STM primitives (host runtime via foreign) ---
(foreign new-tvar   (-> a (STM (TVar a)))
         #:from rackton/private/prelude-runtime)
(foreign read-tvar  (-> (TVar a) (STM a))
         #:from rackton/private/prelude-runtime)
(foreign write-tvar (-> (TVar a) (-> a (STM Unit)))
         #:from rackton/private/prelude-runtime)
(foreign retry      (STM a)
         #:from rackton/private/prelude-runtime)
(foreign or-else    (-> (STM a) (-> (STM a) (STM a)))
         #:from rackton/private/prelude-runtime)
(foreign atomically (-> (STM a) (IO a))
         #:from rackton/private/prelude-runtime)

;; --- instance-method impls (host runtime via foreign) ---
(foreign stm-fmap (-> (-> a b) (-> (STM a) (STM b)))
         #:from rackton/private/prelude-runtime)
(foreign stm-pure (-> a (STM a))
         #:from rackton/private/prelude-runtime)
(foreign stm-ap   (-> (STM (-> a b)) (-> (STM a) (STM b)))
         #:from rackton/private/prelude-runtime)
(foreign stm-bind (-> (-> a (STM b)) (-> (STM a) (STM b)))
         #:from rackton/private/prelude-runtime)

(instance (Functor STM)
  (define (fmap f s) (stm-fmap f s)))

(instance (Applicative STM)
  (define (pure x) (stm-pure x))
  (define (fapply sf sa) (stm-ap sf sa)))

(instance (Monad STM)
  (define (flatmap f s) (stm-bind f s)))

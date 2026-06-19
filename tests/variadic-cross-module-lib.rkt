#lang rackton

;; Fixture for variadic-test.rkt: a library that exports a variadic
;; function.  An importer must recover its arity from the sidecar so
;; call sites in the importing module still gather their trailing
;; arguments into the rest-list.

(provide sum-all)

(: sum-all (-> Integer ... Integer))
(define (sum-all . xs) (foldr + 0 xs))

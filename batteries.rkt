#lang rackton

;; rackton/batteries — umbrella module re-exporting the whole standard
;; library, for users who prefer one import over managing per-module
;; requires.  As the library grows (Phase 2 carves + Phases 3-8), each
;; new stdlib module is added here.  Writable thanks to (all-from-out …)
;; (Enabler C).

(require rackton/data/maybe
         rackton/data/monoid)

(provide (all-from-out rackton/data/maybe)
         (all-from-out rackton/data/monoid))

#lang rackton

;; The umbrella lives at the collection root (rackton/cmdline →
;; cmdline.rkt) so the Rackton require resolver finds it; this submodule
;; entry just re-exports it for anyone importing rackton/cmdline/main.

(require rackton/cmdline)
(provide (all-from-out rackton/cmdline))

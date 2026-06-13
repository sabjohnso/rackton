#lang racket/base

;; Rackton — DAP debug server entry point.
;;
;;   racket -l rackton/dap
;;
;; Speaks the Debug Adapter Protocol over stdio.  Needs the
;; gui-debugger collection at runtime (`raco pkg install drracket`).
;; Emacs (dape):
;;
;;   (add-to-list 'dape-configs
;;                `(rackton
;;                  command "racket" command-args ("-l" "rackton/dap")
;;                  :type "rackton"
;;                  :program dape-buffer-default))
;;
;; The kernel (private/dap.rkt) runs the debuggee in a thread and
;; reports breakpoints, stack frames, and locals by Rackton source
;; positions; this entry point only binds the loop to stdio.

(require "private/dap.rkt")

(module+ main
  (file-stream-buffer-mode (current-output-port) 'none)
  (run-dap-loop (current-input-port) (current-output-port)))

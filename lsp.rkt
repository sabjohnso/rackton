#lang racket/base

;; Rackton — LSP server entry point.
;;
;;   racket -l rackton/lsp
;;
;; Speaks the Language Server Protocol over stdio.  Emacs (eglot):
;;
;;   (add-to-list 'eglot-server-programs
;;                '(rackton-mode . ("racket" "-l" "rackton/lsp")))
;;
;; or with scheme-mode/racket-mode buffers visiting #lang rackton
;; files, point eglot at the same command.  The kernel
;; (private/lsp.rkt) is a pure state machine; this loop only frames
;; bytes and logs handler failures to stderr.

(require "private/lsp.rkt")

(module+ main
  (define in (current-input-port))
  (define out (current-output-port))
  (file-stream-buffer-mode out 'none)
  (let loop ([st (make-lsp-state)])
    (define msg (read-lsp-message in))
    (cond
      [(eof-object? msg) (void)]
      [else
       (define-values (st* outgoing)
         (with-handlers ([exn:fail?
                          (lambda (e)
                            (eprintf "rackton/lsp: ~a\n" (exn-message e))
                            (values st '()))])
           (handle-message st msg)))
       (for ([o (in-list outgoing)])
         (write-lsp-message out o))
       (unless (lsp-state-done? st*)
         (loop st*))])))

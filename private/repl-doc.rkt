#lang racket/base

;; private/repl-doc.rkt — the REPL's ,doc command.
;;
;; ,doc opens the Rackton documentation in a browser. Resolution reuses
;; Racket's own cross-reference database (the same mechanism `raco docs`
;; uses): the rendered manual's index is found by looking up the "top"
;; section tag of the main scribbling. If the docs are not built, the
;; lookup returns #f and the command reports how to build them instead
;; of opening anything.
;;
;; The one side effect (opening a browser) and the path resolution are
;; each injected through a parameter, so the command's decision is
;; testable without launching a browser or building the docs. See
;; repl-doc-test.rkt.

(require setup/xref
         scribble/xref
         scribble/tag
         net/sendurl)

(provide rackton-doc-path
         current-doc-resolver
         current-url-opener
         doc-command-result)

;; The module path of the top-level Rackton manual.
(define rackton-doc-module '(lib "rackton/scribblings/rackton.scrbl"))

;; The filesystem path (as a string) of the Rackton documentation
;; index, or #f when the docs are not built / not registered in the
;; cross-reference database.
(define (rackton-doc-path)
  (define xref (load-collections-xref))
  (define tag (make-section-tag "top" #:doc rackton-doc-module))
  (define-values (path _anchor) (xref-tag->path+anchor xref tag))
  (and path (path->string path)))

;; Injection points. The resolver returns a path string or #f; the
;; opener is handed a path string to display. Both default to the real
;; implementations and are overridden in tests.
(define current-doc-resolver (make-parameter rackton-doc-path))
(define current-url-opener   (make-parameter send-url/file))

;; Resolve the documentation path; open it and report it when found,
;; otherwise report how to build the docs. Returns the message string.
(define (doc-command-result)
  (define path ((current-doc-resolver)))
  (cond
    [path
     ((current-url-opener) path)
     (format "opening documentation: ~a\n" path)]
    [else
     (string-append
      "documentation not found — build it with:\n"
      "  raco setup rackton\n"
      "then try ,doc again\n")]))

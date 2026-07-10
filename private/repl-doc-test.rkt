#lang racket/base

;; Tests for private/repl-doc.rkt — the REPL's ,doc command core.
;;
;; The command has one side effect (opening a browser) and one
;; decision (which path to open, and what to report). Both the path
;; resolver and the opener are injected through parameters, so these
;; tests exercise the found and not-found branches without launching a
;; browser or requiring the docs to be built.
;;
;; RED: private/repl-doc.rkt does not exist yet.

(module+ test
  (require rackunit
           "repl-doc.rkt")

  (test-case "doc found: opens the resolved path and reports it"
    (define opened (box 'unset))
    (parameterize ([current-doc-resolver (lambda () "/tmp/rackton/index.html")]
                   [current-url-opener   (lambda (p) (set-box! opened p))])
      (define msg (doc-command-result))
      (check-equal? (unbox opened) "/tmp/rackton/index.html"
                    "the opener receives the resolved path")
      (check-regexp-match #rx"/tmp/rackton/index\\.html" msg
                          "the message names the path opened")))

  (test-case "doc not found: does not open, advises building the docs"
    (define opened (box 'unset))
    (parameterize ([current-doc-resolver (lambda () #f)]
                   [current-url-opener   (lambda (p) (set-box! opened p))])
      (define msg (doc-command-result))
      (check-equal? (unbox opened) 'unset "the opener is not called")
      (check-regexp-match #rx"raco setup rackton" msg
                          "the message tells the user how to build the docs")))

  ;; Integration: the real resolver consults Racket's cross-reference
  ;; database, so it can only resolve when the docs are built. When it
  ;; does resolve, the result must be the Rackton manual's index — a
  ;; wrong module path or tag would instead return #f or a foreign
  ;; manual. When the docs are not built (e.g. a fresh CI checkout), the
  ;; resolver correctly returns #f and there is nothing to assert.
  (test-case "rackton-doc-path resolves to the rackton manual index when built"
    (define path (rackton-doc-path))
    (when path
      (check-regexp-match #rx"rackton[/\\\\]index\\.html$" path
                          "the resolved path is the rackton manual's index"))))

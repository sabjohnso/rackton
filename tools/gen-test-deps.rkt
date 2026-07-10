#lang racket/base

;; gen-test-deps.rkt — the thin shell over gen-test-deps-lib.
;;
;; Default mode prints the GNU Make fragment mapping each test to its
;; transitive in-repo sources (see the library header for the format).
;; `--list-sources` prints every `.rkt` source path, one per line, so
;; the Makefile can obtain its compile inventory from the same directory
;; walk that discovers tests — the exclusion policy then lives in one
;; place. Paths are resolved against this file's location and emitted
;; relative to the repository root (the Makefile runs from that root).

(require racket/runtime-path
         "gen-test-deps-lib.rkt")

(define-runtime-path tools-dir ".")
(define repo-root (simplify-path (build-path tools-dir 'up) #f))

(define (emit-fragment)
  (display (render-fragment (compute-mapping repo-root))))

(define (emit-sources)
  (for ([p (in-list (sort (list-source-files repo-root) path<?))])
    (displayln (path->repo-relative repo-root p))))

(module+ main
  (case (mode-for-args (vector->list (current-command-line-arguments)))
    [(list-sources) (emit-sources)]
    [else (emit-fragment)]))

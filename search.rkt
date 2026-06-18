#lang racket/base

;; Rackton — signature search over every installed module.
;;
;;   racket -l rackton/search -- "(-> (List a) Integer)"
;;   racket -l rackton/search -- --returns "(List Integer)"
;;   racket -l rackton/search -- --accepts "Integer"
;;   racket -l rackton/search -- --name "stream"
;;   racket -l rackton/search -- --stdlib "(-> (List a) Integer)"
;;   racket -l rackton/search -- --reindex --name "stream"
;;
;; The default scope is the whole workspace: the curated standard
;; library unioned with every installed `#lang rackton` module.
;; `--stdlib` narrows to the library alone; `--reindex` rebuilds the
;; installed-module index cache.  The default mode matches whole
;; signatures by unification — same arity, in order (listed first) or
;; with arguments permuted (listed after).  Results show each match's
;; defining module and line, from the rackton-defs sidecar table.  The
;; same queries are available inside a session as the ,search /
;; ,returns / ,accepts REPL commands, where they also see the
;; session's own definitions.

(provide search-collections)

(require racket/cmdline
         racket/list
         "private/repl-search.rkt"
         "private/analyze.rkt"
         (only-in "private/installed-scan.rkt"
                  rackton-installed-entries index-cache-path)
         (only-in "private/types.rkt" scheme->datum))

;; Search installed modules; returns (list hit provenance…) pairs — the
;; matched (name . scheme) plus the index entries behind the name
;; (module + srcloc).  `scope` is 'workspace (stdlib + installed
;; modules) or 'stdlib (the library alone).  Entries are kept raw — not
;; deduplicated across modules — so provenance can list every module a
;; name comes from.
(define (search-collections query #:kind [kind 'signature] #:scope [scope 'workspace])
  (define entries
    (case scope
      [(stdlib) (rackton-collection-entries)]
      [else (append (rackton-collection-entries) (rackton-installed-entries))]))
  ;; A re-exported binding indexes once per module; search candidates
  ;; once per distinct (name, type) — provenance keeps every module.
  (define pairs
    (remove-duplicates
     (for/list ([e (in-list entries)] #:when (index-entry-scheme e))
       (cons (index-entry-name e) (index-entry-scheme e)))
     #:key (lambda (p) (cons (car p) (scheme->datum (cdr p))))))
  (define hits (search-entries pairs query #:kind kind #:env #f))
  (cond
    [(symbol? hits) hits]                ; 'bare-query
    [else
     (define by-name
       (for/fold ([h (hasheq)]) ([e (in-list entries)])
         (hash-update h (index-entry-name e)
                      (lambda (l) (cons e l)) '())))
     (for/list ([hit (in-list hits)])
       (cons hit (hash-ref by-name (car hit) '())))]))

(module+ main
  (define kind (make-parameter 'signature))
  (define scope (make-parameter 'workspace))
  (define reindex? (make-parameter #f))
  (define query-string
    (command-line
     #:program "rackton/search"
     #:once-each
     ["--stdlib" "search only the standard library, not installed modules"
                 (scope 'stdlib)]
     ["--reindex" "rebuild the installed-module index cache from scratch"
                  (reindex? #t)]
     #:once-any
     ["--returns" "match the result type" (kind 'returns)]
     ["--accepts" "match an argument position" (kind 'accepts)]
     ["--name" "search names instead of types" (kind 'name)]
     #:args (query) query))
  ;; --reindex drops the cache so every module is re-read; otherwise the
  ;; mtime-keyed cache refreshes only what changed.  A cold cache means
  ;; the first workspace run loads each module once — warn so the wait
  ;; is not a surprise.
  (when (reindex?)
    (with-handlers ([exn:fail? void]) (delete-file (index-cache-path))))
  (when (and (eq? (scope) 'workspace) (not (file-exists? (index-cache-path))))
    (eprintf "indexing installed modules (first run may be slow)…\n"))
  (define query
    (if (eq? (kind) 'name)
        query-string
        (read (open-input-string query-string))))
  (define results
    (search-collections query
                        #:scope (scope)
                        #:kind (if (eq? (kind) 'name)
                                   'signature   ; string ⇒ name search
                                   (kind))))
  (cond
    [(eq? results 'bare-query)
     (displayln "the query is a bare type variable — everything matches")]
    [(null? results)
     (printf "no matches for ~a\n" query-string)]
    [else
     (for ([r (in-list results)])
       (define hit (car r))
       (printf "~s :: ~a\n" (car hit) (scheme->datum (cdr hit)))
       (for ([e (in-list (cdr r))])
         (define l (index-entry-srcloc e))
         (printf "    ~a~a\n"
                 (index-entry-module e)
                 (if (and l (srcloc-line l)) (format ":~a" (srcloc-line l)) ""))))]))

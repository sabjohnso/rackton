#lang racket/base

;; Tests for installed-scan.rkt — discovery of installed `#lang rackton`
;; modules and the mtime-keyed index cache that keeps repeated searches
;; from re-running each module.
;;
;; The discovery filter (`rackton-module-file?`) reads only a file's
;; #lang line, so it never instantiates the module.  Indexing does load
;; a module (through its compiled `rackton-schemes` sidecar), so the
;; cache exists to bound that to once per (path, mtime).  Fixtures are
;; `#lang rackton` files written to a temp directory, indexed exactly as
;; the real scan does.

(module+ test
  (require rackunit
           racket/file
           racket/list
           racket/path
           "installed-scan.rkt"
           "analyze.rkt"
           (only-in "types.rkt" scheme->datum))

  (define root (make-temporary-file "rackton-scan-~a" 'directory))

  (define (fixture rel . lines)
    (define path (build-path root rel))
    (make-directory* (let-values ([(d _n _?) (split-path path)]) d))
    (call-with-output-file path #:exists 'truncate
      (lambda (out) (for ([l (in-list lines)]) (displayln l out))))
    path)

  (define (canon p) (simplify-path (path->complete-path p)))
  (define (in? p ps) (and (member (canon p) (map canon ps)) #t))

  ;; ----- #lang rackton detection (no instantiation) ------------------

  (define rk (fixture "lib.rkt"
                      "#lang rackton"
                      "(provide thing)"
                      "(: thing Integer)"
                      "(define thing 42)"))
  (define plain (fixture "plain.rkt"
                         "#lang racket/base"
                         "(provide x)"
                         "(define x 1)"))

  (check-true  (rackton-module-file? rk)    "a #lang rackton file is detected")
  (check-false (rackton-module-file? plain) "a #lang racket/base file is not")

  ;; ----- directory walk, conventional dirs excluded ------------------

  (define nested   (fixture "sub/nested.rkt" "#lang rackton"
                            "(provide a)" "(: a Integer)" "(define a 1)"))
  (define in-tests (fixture "tests/probe.rkt" "#lang rackton"
                            "(provide b)" "(: b Integer)" "(define b 2)"))

  (define found (find-rackton-files-in root))
  (check-true  (in? rk found)       "top-level rackton module found")
  (check-true  (in? nested found)   "nested rackton module found")
  (check-false (in? plain found)    "non-rackton file skipped")
  (check-false (in? in-tests found) "files under tests/ excluded")

  ;; ----- indexing a discovered module --------------------------------

  (define cache-path (build-path root "cache.rktd"))

  (define entries (index-entries-for-files (list rk) #:cache-path cache-path))
  (define (named name es) (for/first ([e (in-list es)]
                                      #:when (eq? (index-entry-name e) name)) e))
  (let ([thing (named 'thing entries)])
    (check-true (and thing #t) "the discovered module's export indexes")
    (check-equal? (scheme->datum (index-entry-scheme thing)) 'Integer))

  ;; ----- the cache is persisted and mtime-keyed ----------------------

  (define c (read-index-cache cache-path))
  (define mt (file-or-directory-modify-seconds rk))
  (check-true  (and (index-cache-ref c rk mt) #t)
               "a fresh lookup hits at the file's recorded mtime")
  (check-false (index-cache-ref c rk (+ mt 1000))
               "a changed mtime misses")

  ;; ----- a fresh cache entry is trusted without reloading ------------

  ;; Seed the cache with fabricated tables at the current mtime: while
  ;; the mtime matches, indexing must trust the cache and not re-read
  ;; the module — so the fabricated `ghost` shows and the real `thing`
  ;; does not.
  (define seed-path (build-path root "seed.rktd"))
  (define seeded
    (index-cache-set (make-index-cache) rk mt
                     (sidecar-tables #:bindings (list (cons 'ghost 'Integer)))))
  (write-index-cache! seed-path seeded)
  (let ([es (index-entries-for-files (list rk) #:cache-path seed-path)])
    (check-true  (and (named 'ghost es) #t)
                 "a fresh cache entry is trusted without loading the module")
    (check-false (named 'thing es)
                 "the module was not re-read while its mtime was unchanged"))

  ;; Advancing the file's mtime invalidates the entry → a real reload,
  ;; which restores the true export and drops the stale ghost.
  (file-or-directory-modify-seconds rk (+ mt 1000))
  (let ([es (index-entries-for-files (list rk) #:cache-path seed-path)])
    (check-true  (and (named 'thing es) #t)
                 "a changed mtime forces a reload from the sidecar")
    (check-false (named 'ghost es)
                 "the stale cache entry is discarded on reload"))

  ;; ----- workspace entries: stdlib unioned with installed modules ----

  (let ([ws (rackton-workspace-entries
             #:cache-path (build-path root "ws.rktd")
             #:installed-files (list rk))])
    (check-true (for/or ([e (in-list ws)]) (eq? (index-entry-name e) 'Stream))
                "the curated stdlib is still present")
    (check-true (for/or ([e (in-list ws)]) (eq? (index-entry-name e) 'thing))
                "an installed user module's export is present too"))

  (delete-directory/files root))

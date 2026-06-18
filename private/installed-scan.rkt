#lang racket/base

;; Rackton — discovery and indexing of installed `#lang rackton`
;; modules, so type search ranges over every installed module and not
;; just the curated standard library.
;;
;; Three concerns, kept separate:
;;
;;   discovery  — `rackton-module-file?` classifies a file by its
;;       #lang line alone (no instantiation), and `find-rackton-files-in`
;;       walks a directory for such files, skipping the conventional
;;       non-library subdirectories.  `installed-rackton-files` applies
;;       the walk to every installed package directory (minus Rackton's
;;       own collection, which the curated stdlib list already covers).
;;
;;   caching    — indexing a module loads it (through its compiled
;;       `rackton-schemes` sidecar), which runs its top level.  The
;;       mtime-keyed cache in the preference directory bounds that to
;;       once per (path, mtime): an unchanged module is replayed from
;;       its stored sidecar tables without being re-read.
;;
;;   combining  — `rackton-workspace-entries` unions the curated stdlib
;;       index with the installed-module index, deduplicated by name and
;;       type, ready for `search-entries`.
;;
;; The decode of sidecar tables into `index-entry`s lives in
;; analyze.rkt; this module adds the discovery, the cache, and the
;; union on top of it.

(provide rackton-module-file?
         default-excluded-dirs
         find-rackton-files-in
         installed-rackton-files
         sidecar-tables
         make-index-cache
         index-cache-ref
         index-cache-set
         index-cache-path
         read-index-cache
         write-index-cache!
         index-entries-for-files
         rackton-installed-entries
         rackton-workspace-entries)

(require racket/match
         racket/list
         (only-in racket/string string-trim)
         (only-in racket/file make-directory*)
         (only-in racket/path path-only)
         (only-in pkg/lib get-all-pkg-scopes installed-pkg-names pkg-directory)
         (only-in "types.rkt" scheme->datum)
         (only-in "analyze.rkt"
                  index-entry-name index-entry-scheme
                  module-sidecar-tables entries-from-tables
                  rackton-collection-entries))

;; ----- discovery ------------------------------------------------------

;; #t when `path` is a `#lang rackton` module, judged from its first
;; line only — Racket requires `#lang` at the very start of a module,
;; so this never reads or runs the body.  A missing or unreadable file
;; is simply not a Rackton module.
(define (rackton-module-file? path)
  (with-handlers ([exn:fail? (lambda (_) #f)])
    (and (file-exists? path)
         (let ([line (call-with-input-file path read-line)])
           (and (string? line)
                (regexp-match? #px"^#lang\\s+rackton\\s*$" (string-trim line))
                #t)))))

;; Subdirectories a library scan skips: test suites and examples (whose
;; modules run on load), generated trees, and a package's internals.
(define default-excluded-dirs
  '("tests" "test" "examples" "scribblings" "benchmarks"
    "compiled" "doc" "private" "info"))

;; Every `#lang rackton` file under `root`, recursively, skipping the
;; excluded directory names.  A non-existent root yields nothing.
(define (find-rackton-files-in root #:exclude-dirs [excluded default-excluded-dirs])
  (cond
    [(not (directory-exists? root)) '()]
    [else
     (for*/fold ([acc '()] #:result (reverse acc))
                ([entry (in-list (directory-list root))]
                 [p (in-value (build-path root entry))])
       (cond
         [(directory-exists? p)
          (if (member (path->string entry) excluded)
              acc
              (append (reverse (find-rackton-files-in p #:exclude-dirs excluded))
                      acc))]
         [(and (regexp-match? #rx"\\.rkt$" (path->string entry))
               (rackton-module-file? p))
          (cons p acc)]
         [else acc]))]))

;; Canonical key for a path: absolute, with `.`/`..` resolved
;; syntactically, as a string — so cache keys and exclusion compare
;; stably regardless of how a path was spelled.
(define (path-key p)
  (path->string (simplify-path (path->complete-path p))))

;; The directories of every installed package, across both scopes.
;; Guarded end to end: any failure in the package database yields no
;; user modules rather than breaking search.
(define (installed-package-dirs)
  (with-handlers ([exn:fail? (lambda (_) '())])
    (define cache (make-hash))
    (for*/list ([scope (in-list (get-all-pkg-scopes))]
                [name (in-list (installed-pkg-names #:scope scope))]
                [dir (in-value (pkg-directory name #:cache cache))]
                #:when dir)
      dir)))

;; Rackton's own collection directory, or #f — excluded from the scan
;; because the curated stdlib list already indexes it (and a blind
;; scan would run its test suite).
(define (rackton-collection-dir)
  (with-handlers ([exn:fail? (lambda (_) #f)])
    (collection-path "rackton")))

;; Every installed `#lang rackton` module file: walk each installed
;; package directory (minus Rackton's own), deduplicated by canonical
;; path.
(define (installed-rackton-files)
  (define rk (let ([d (rackton-collection-dir)]) (and d (path-key d))))
  (define roots
    (remove-duplicates
     (for/list ([d (in-list (installed-package-dirs))]
                #:unless (and rk (equal? (path-key d) rk)))
       d)
     #:key path-key))
  (remove-duplicates (append-map find-rackton-files-in roots) #:key path-key))

;; ----- the index cache ------------------------------------------------

;; A cache maps a path's canonical key to (cons mtime tables), where
;; tables is the sidecar-tables hash analyze.rkt decodes.

(define (make-index-cache) (hash))

;; Build a sidecar-tables hash (the value `module-sidecar-tables`
;; returns) with explicit fields, each defaulting to empty.  Used to
;; replay cached entries and, in tests, to stand in for a module.
(define (sidecar-tables #:bindings   [bindings '()]
                        #:data-ctors [data-ctors '()]
                        #:tcons      [tcons '()]
                        #:classes    [classes '()]
                        #:defs       [defs '()])
  (hasheq 'bindings bindings 'data-ctors data-ctors
          'tcons tcons 'classes classes 'defs defs))

;; The cached tables for `path` when the recorded mtime equals `mtime`,
;; else #f — a stale or absent entry misses.
(define (index-cache-ref cache path mtime)
  (define e (hash-ref cache (path-key path) #f))
  (and e (equal? (car e) mtime) (cdr e)))

(define (index-cache-set cache path mtime tables)
  (hash-set cache (path-key path) (cons mtime tables)))

;; The cache file in the user's preference directory (the convention
;; the REPL history follows).
(define (index-cache-path)
  (build-path (find-system-path 'pref-dir) "rackton-module-index.rktd"))

;; Read the cache, forgivingly: a missing, unreadable, or malformed
;; file is an empty cache, never an error.  On disk it is a list of
;; (path mtime bindings data-ctors tcons classes defs) rows of plain
;; s-expressions — no hash literals, so it round-trips through
;; read/write exactly.
(define (read-index-cache path)
  (with-handlers ([exn:fail? (lambda (_) (make-index-cache))])
    (define data (call-with-input-file path read))
    (cond
      [(list? data)
       (for/fold ([c (make-index-cache)]) ([row (in-list data)])
         (match row
           [(list (? string? p) mtime bindings data-ctors tcons classes defs)
            (hash-set c p (cons mtime (sidecar-tables #:bindings bindings
                                                      #:data-ctors data-ctors
                                                      #:tcons tcons
                                                      #:classes classes
                                                      #:defs defs)))]
           [_ c]))]
      [else (make-index-cache)])))

(define (write-index-cache! path cache)
  (with-handlers ([exn:fail? (lambda (_) (void))])   ; best effort
    (define rows
      (for/list ([(p e) (in-hash cache)])
        (define t (cdr e))
        (list p (car e)
              (hash-ref t 'bindings '()) (hash-ref t 'data-ctors '())
              (hash-ref t 'tcons '()) (hash-ref t 'classes '())
              (hash-ref t 'defs '()))))
    (make-directory* (path-only path))
    (call-with-output-file path #:exists 'truncate
      (lambda (out) (write rows out) (newline out)))))

;; ----- indexing through the cache -------------------------------------

;; Index `files`, reusing cached tables for any file whose mtime is
;; unchanged and loading the rest through their sidecars.  The updated
;; cache is written back.  A file that vanishes mid-scan is skipped.
(define (index-entries-for-files files #:cache-path [cp (index-cache-path)])
  (define cache0 (read-index-cache cp))
  (define-values (entries cache1)
    (for/fold ([acc '()] [cache cache0]) ([f (in-list files)])
      (define mtime
        (with-handlers ([exn:fail? (lambda (_) #f)])
          (file-or-directory-modify-seconds f)))
      (cond
        [(not mtime) (values acc cache)]
        [else
         (define cached (index-cache-ref cache f mtime))
         (define tables (or cached (module-sidecar-tables f)))
         (define cache* (if cached cache (index-cache-set cache f mtime tables)))
         (values (append acc (entries-from-tables tables f)) cache*)])))
  (write-index-cache! cp cache1)
  entries)

;; The installed-module index: every installed `#lang rackton` module,
;; through the cache.
(define (rackton-installed-entries #:cache-path [cp (index-cache-path)])
  (index-entries-for-files (installed-rackton-files) #:cache-path cp))

;; ----- the workspace index --------------------------------------------

;; The full searchable index: the curated standard library unioned with
;; every installed user module, deduplicated by name and type so a
;; binding re-exported through several modules is one candidate.
;; `installed-files` is the module set to index (the live installed scan
;; by default; overridable for testing).
(define (rackton-workspace-entries #:cache-path [cp (index-cache-path)]
                                   #:installed-files [files (installed-rackton-files)])
  (remove-duplicates
   (append (rackton-collection-entries)
           (index-entries-for-files files #:cache-path cp))
   #:key (lambda (e) (cons (index-entry-name e)
                           (let ([s (index-entry-scheme e)])
                             (and s (scheme->datum s)))))))

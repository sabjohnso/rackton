#lang racket/base

;; Rackton — completion candidates for the module paths a `require` spec
;; may name.
;;
;; Enumeration is directed by the prefix, not indexed ahead of time: the
;; prefix already names the directory to read, so each call lists at most
;; the directories its own leading segments resolve to.  Nothing is
;; cached, because nothing is scanned that the user has not already typed
;; their way into.  (Contrast installed-scan.rkt, which must index every
;; installed module because type search ranges over all of them.)
;;
;; Two candidate universes, one for each spec shape the grammar admits:
;;
;;   collection paths — `rackton/data/list`, resolved through Racket's
;;       collection search (collects roots, links, packages), so anything
;;       requirable is offered, Rackton module or not.
;;
;;   relative paths — `"helpers.rkt"`, resolved against the directory the
;;       spec is anchored at (the requiring file's, or the REPL's).
;;
;; Candidate shape, common to both: a subdirectory keeps a trailing slash
;; so a second completion descends into it.  A collection-path module
;; drops its `.rkt` extension, because that is how it is written in a
;; require; a relative-path module keeps it, for the same reason.
;;
;; Every filesystem failure — an unreadable directory, a prefix naming no
;; collection — degrades to no candidates.  Completion is an assist; it
;; must never raise into the editor or the REPL.
;;
;; Public API:
;;   collection-path-completions : string -> (listof string)
;;   relative-path-completions   : string path-string -> (listof string)

(provide collection-path-completions
         relative-path-completions)

(require racket/list
         racket/string
         (only-in setup/dirs get-collects-search-dirs)
         (only-in setup/link links))

;; ----- the shared shape -------------------------------------------------

;; Complete `prefix` against whatever `entries-of` says its leading
;; segments contain.  The two universes differ only in how a segment list
;; resolves and in what counts as a segment at all; the rest — split the
;; prefix, filter by its final segment, re-attach what was already typed —
;; is common.
;;
;; Every candidate extends `prefix` literally, which is what the editor
;; relies on when it inserts the candidates' common prefix.  A prefix
;; whose leading segments cannot name anything (an empty segment, from a
;; leading or doubled slash) therefore completes to nothing rather than to
;; something that silently drops the offending characters.
(define (complete-under prefix entries-of segment-ok?)
  (define-values (segments leaf) (split-prefix prefix))
  (cond
    [(not (andmap segment-ok? segments)) '()]
    [else
     (define kept (string-append* (for/list ([s (in-list segments)])
                                    (string-append s "/"))))
     (sorted-candidates
      (for/list ([name (in-list (entries-of segments))]
                 #:when (string-prefix? name leaf))
        (string-append kept name)))]))

;; ----- collection paths -------------------------------------------------

;; The module paths that extend `prefix`, a partially typed collection
;; path such as "rackton/da".  Its leading segments name the directories
;; to read; its final segment filters their contents.
(define (collection-path-completions prefix)
  (complete-under prefix collection-entries collection-segment?))

;; A collection path is a sequence of collection names; `.` and `..` are
;; filesystem notation, not collection names.
(define (collection-segment? s)
  (and (non-empty-string? s) (not (member s '("." "..")))))

(define (collection-entries segments)
  (if (null? segments)
      (top-level-collection-entries)
      (append* (map directory-entries (collection-dirs segments)))))

;; Every directory a collection path names.  A collection may be spliced
;; across roots, so all of them are consulted, not just the first:
;; `collection-path` resolves one (honouring links and packages), and each
;; collects root is additionally tried directly.
(define (collection-dirs segments)
  (define resolved
    (guarded #f (lambda () (apply collection-path segments))))
  (remove-duplicates
   (filter values
           (cons resolved
                 (for/list ([root (in-list (collects-roots))])
                   (define d (apply build-path root segments))
                   (and (directory-exists? d) d))))
   #:key path-key))

;; The names of every top-level collection: the subdirectories of each
;; collects root, plus the collections registered in the links database.
(define (top-level-collection-entries)
  (append (append* (map directory-entries (collects-roots)))
          (for/list ([name (in-list (guarded '() links))])
            (string-append (format "~a" name) "/"))))

(define (collects-roots)
  (guarded '() get-collects-search-dirs))

;; ----- relative paths ---------------------------------------------------

;; The file paths that extend `prefix` relative to `base-dir`.  Unlike a
;; collection path, the `.rkt` extension is part of what is written, so
;; entries are offered under their own file names.
(define (relative-path-completions prefix base-dir)
  (complete-under prefix
                  (lambda (segments) (relative-entries base-dir segments))
                  ;; `.` and `..` are ordinary here — a string spec may
                  ;; well reach a sibling directory.
                  non-empty-string?))

(define (relative-entries base-dir segments)
  (define dir
    (guarded #f (lambda ()
                  (define d (if (null? segments)
                                (if (path? base-dir) base-dir (string->path base-dir))
                                (apply build-path base-dir segments)))
                  (and (directory-exists? d) d))))
  (if dir (directory-entries dir #:keep-extension? #t) '()))

;; ----- directory reading ------------------------------------------------

;; The requirable entries of one directory, as candidate segments: each
;; subdirectory with a trailing slash, each Racket module by name.
;; Skipped are build output, `info.rkt` (metadata, never required by
;; user code), and dotfiles.
(define (directory-entries dir #:keep-extension? [keep-extension? #f])
  (guarded
   '()
   (lambda ()
     (for*/list ([p (in-list (directory-list dir))]
                 [name (in-value (path->string p))]
                 #:unless (string-prefix? name ".")
                 [full (in-value (build-path dir p))]
                 [entry (in-value
                         (cond
                           [(directory-exists? full)
                            (and (not (equal? name "compiled"))
                                 (string-append name "/"))]
                           [(and (string-suffix? name ".rkt")
                                 (not (equal? name "info.rkt")))
                            (if keep-extension? name (drop-extension name))]
                           [else #f]))]
                 #:when entry)
       entry))))

(define (drop-extension name)
  (substring name 0 (- (string-length name) (string-length ".rkt"))))

;; ----- shared helpers ---------------------------------------------------

;; Split a partially typed path into the complete segments before the
;; last slash and the partial segment after it.  A prefix ending in a
;; slash has an empty final segment, which matches every entry.  Empty
;; leading segments are kept, not dropped: the caller rejects them, so
;; that re-joining the segments always reproduces the text typed.
(define (split-prefix prefix)
  (define parts (string-split prefix "/" #:trim? #f))
  (cond
    [(null? parts) (values '() "")]
    [else (values (drop-right parts 1) (last parts))]))

(define (sorted-candidates cs)
  (sort (remove-duplicates cs) string<?))

(define (path-key p) (path->string (simplify-path (path->complete-path p))))

;; Run `thunk`, answering `fallback` on any failure: a missing collection,
;; an unreadable directory, a malformed links file.
(define (guarded fallback thunk)
  (with-handlers ([exn:fail? (lambda (_) fallback)]) (thunk)))

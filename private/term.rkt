#lang racket/base

;; Terminal-width detection for REPL error formatting.
;;
;; Tenets: this module is the *only* place that touches the terminal, so
;; the impure query (env var + `stty`) is isolated behind a small,
;; injectable interface.  The arithmetic (`compute-display-columns`,
;; `columns->type-budget`) is pure and unit-tested without a terminal;
;; the impure `detect-display-columns` just feeds it real signals.
;;
;; Public API:
;;   compute-display-columns  : env-col terminal? stty-line → cols | #f  (pure)
;;   columns->type-budget     : cols → budget                            (pure)
;;   detect-display-columns   : → cols | #f                              (impure)
;;   refresh-type-columns!    : [detect] → void   (sets current-type-columns)
;;
;; Width adaptation is REPL-only.  `current-type-columns` (types.rkt)
;; defaults to 66 and is changed only when inference runs interactively:
;;   - the Rackton REPL calls `refresh-type-columns!` each prompt (phase 0);
;;   - the `rackton` macro expander (phase 1, in elaborate.rkt) reads
;;     `detect-display-columns` and parameterizes the width around
;;     inference, but only at `'top-level` context.
;; A `'module` context (batch compile, a `#lang rackton` file) keeps the
;; default, so compiled error text stays reproducible.

(require racket/match
         racket/string
         racket/system
         racket/port
         "types.rkt")

(provide current-type-columns  ; re-export (from types.rkt)
         compute-display-columns
         columns->type-budget
         detect-display-columns
         refresh-type-columns!)

;; ----- pure arithmetic ----------------------------------------------

;; The display budget after the diagnostic's label column.  13 = the
;; width of "  expected: " (12) plus a one-space margin; the floor keeps
;; a narrow terminal from collapsing a wrapped type to one token a line.
(define min-type-columns 40)
(define label-width 13)

(define (columns->type-budget cols)
  (max min-type-columns (- cols label-width)))

;; Resolve a column count from the available signals, preferring an
;; explicit `COLUMNS` over an `stty size` probe.  `env-col` is the raw
;; `COLUMNS` string (or #f); `stty-line` is the raw "rows cols" line (or
;; #f) and is consulted only on a real terminal.  #f means "no reliable
;; width — keep the default".
(define (compute-display-columns env-col terminal? stty-line)
  (define from-env (and env-col (string->number (string-trim env-col))))
  (cond
    [(and (exact-integer? from-env) (positive? from-env)) from-env]
    [(and terminal? stty-line) (parse-stty-cols stty-line)]
    [else #f]))

;; "24 80" → 80; anything that isn't two positive integers → #f.
(define (parse-stty-cols line)
  (match (map string->number (string-split line))
    [(list (? exact-positive-integer?) (? exact-positive-integer? cols)) cols]
    [_ #f]))

;; ----- impure query --------------------------------------------------

(define (detect-display-columns)
  ;; A real terminal on either port is enough to trust `stty size`;
  ;; `COLUMNS` is honored even without one (so a piped or comint REPL
  ;; that exports it still adapts).  When neither holds, this returns #f
  ;; and the caller keeps the default — set `current-type-columns`
  ;; directly to override in an environment we cannot probe.
  (define terminal? (or (terminal-port? (current-input-port))
                        (terminal-port? (current-output-port))))
  (compute-display-columns (getenv "COLUMNS")
                           terminal?
                           (and terminal? (query-stty-size))))

;; The raw `stty size` output ("rows cols"), or #f if it can't be run
;; (no controlling terminal, no `stty`, Windows, …).
(define (query-stty-size)
  (with-handlers ([exn:fail? (lambda (_) #f)])
    (define out (string-trim (with-output-to-string (lambda () (system "stty size")))))
    (and (positive? (string-length out)) out)))

;; ----- wiring --------------------------------------------------------

;; Set `current-type-columns` from a freshly detected width; leave it
;; untouched when no width is available (e.g. piped, non-terminal).  Used
;; by the Rackton REPL loop (phase 0).
(define (refresh-type-columns! [detect detect-display-columns])
  (define cols (detect))
  (when cols (current-type-columns (columns->type-budget cols))))

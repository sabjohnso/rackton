#lang racket/base

;; Rackton — the LSP server kernel.
;;
;; Pure message handlers over a server-state value, the REPL-kernel
;; pattern again: `handle-message` maps state × jsexpr → state ×
;; outgoing jsexprs, so every capability is tested as a function —
;; the stdio loop (lsp.rkt at the package root) only frames bytes.
;;
;; Capabilities (v1): diagnostics on open/change (whole-module
;; re-analysis via analyze-text — buffer contents, not disk), hover
;; (schemes), completion (env names + keywords; falls back to the
;; prelude when the buffer is mid-edit and unanalyzable), definition
;; (local from the analysis; imported through required modules'
;; sidecar defs tables), and document symbols.
;;
;; Position conventions: LSP is 0-based lines and UTF-16 columns;
;; srclocs are 1-based lines and 0-based character columns.  Columns
;; are treated as characters — identical for the Basic Multilingual
;; Plane, which covers Rackton source in practice (λ included).

(provide make-lsp-state
         lsp-state-done?
         handle-message
         read-lsp-message
         write-lsp-message
         path->uri
         uri->path)

(require racket/match
         racket/list
         racket/string
         json
         "analyze.rkt"
         "env.rkt"
         "types.rkt"
         "prelude.rkt"
         (only-in "repl-term.rkt" rackton-keyword-names))

;; ----- framing -----------------------------------------------------------

(define (read-lsp-message in)
  (define len
    (let loop ([len #f])
      (define line (read-line in 'any))
      (cond
        [(eof-object? line) #f]
        [(string=? line "") len]
        [(regexp-match #rx"^Content-Length: *([0-9]+)" line)
         => (lambda (m) (loop (string->number (cadr m))))]
        [else (loop len)])))
  (cond
    [(not len) eof]
    [else
     (define payload (read-bytes len in))
     (if (or (eof-object? payload) (< (bytes-length payload) len))
         eof
         (bytes->jsexpr payload))]))

(define (write-lsp-message out msg)
  (define payload (jsexpr->bytes msg))
  (fprintf out "Content-Length: ~a\r\n\r\n" (bytes-length payload))
  (write-bytes payload out)
  (flush-output out))

;; ----- file uris ------------------------------------------------------------

(define (path->uri p)
  (string-append "file://" (percent-encode (path->string p))))

(define (uri->path uri)
  (string->path
   (percent-decode
    (if (string-prefix? uri "file://") (substring uri 7) uri))))

(define (unreserved? b)
  (or (<= (char->integer #\a) b (char->integer #\z))
      (<= (char->integer #\A) b (char->integer #\Z))
      (<= (char->integer #\0) b (char->integer #\9))
      (memv b (map char->integer '(#\- #\. #\_ #\~ #\/)))))

(define (percent-encode s)
  (apply string-append
         (for/list ([b (in-bytes (string->bytes/utf-8 s))])
           (if (unreserved? b)
               (string (integer->char b))
               (format "%~a" (string-upcase (number->string b 16)))))))

(define (percent-decode s)
  (define out (open-output-bytes))
  (let loop ([i 0])
    (when (< i (string-length s))
      (cond
        [(and (char=? (string-ref s i) #\%) (< (+ i 2) (string-length s)))
         (write-byte (string->number (substring s (add1 i) (+ i 3)) 16) out)
         (loop (+ i 3))]
        [else
         (write-bytes (string->bytes/utf-8 (string (string-ref s i))) out)
         (loop (add1 i))])))
  (bytes->string/utf-8 (get-output-bytes out)))

;; ----- server state ------------------------------------------------------------

;; docs: uri → analysis (re-made on every open/change).
(struct lsp-state (docs done?) #:transparent)

(define (make-lsp-state) (lsp-state (hash) #f))

;; ----- jsexpr helpers ------------------------------------------------------------

(define (response id result)
  (hasheq 'jsonrpc "2.0" 'id id 'result result))

(define (error-response id code message)
  (hasheq 'jsonrpc "2.0" 'id id
          'error (hasheq 'code code 'message message)))

(define (notification method params)
  (hasheq 'jsonrpc "2.0" 'method method 'params params))

(define (ref js . keys)
  (for/fold ([v js]) ([k (in-list keys)])
    (and (hash? v) (hash-ref v k #f))))

;; ----- positions ----------------------------------------------------------------

(define (srcloc->range l)
  (define line (max 0 (sub1 (or (srcloc-line l) 1))))
  (define col (or (srcloc-column l) 0))
  (hasheq 'start (hasheq 'line line 'character col)
          'end (hasheq 'line line 'character (+ col (or (srcloc-span l) 1)))))

(define (srcloc->location l)
  (hasheq 'uri (path->uri (if (path? (srcloc-source l))
                              (srcloc-source l)
                              (string->path (format "~a" (srcloc-source l)))))
          'range (srcloc->range l)))

;; ----- dispatch -------------------------------------------------------------------

(define (handle-message st msg)
  (define method (hash-ref msg 'method #f))
  (define id (hash-ref msg 'id #f))
  (define params (hash-ref msg 'params (hasheq)))
  (define (respond thunk)
    ;; A request handler that fails still answers — internal error.
    (values st
            (list (with-handlers ([exn:fail?
                                   (lambda (e)
                                     (error-response id -32603
                                                     (exn-message e)))])
                    (response id (thunk))))))
  (match method
    ["initialize"
     (values st (list (response id (hasheq 'capabilities capabilities))))]
    ["initialized" (values st '())]
    ["shutdown" (values st (list (response id 'null)))]
    ["exit" (values (struct-copy lsp-state st [done? #t]) '())]
    ["textDocument/didOpen"
     (sync-doc st (ref params 'textDocument 'uri)
               (ref params 'textDocument 'text))]
    ["textDocument/didChange"
     (define changes (hash-ref params 'contentChanges '()))
     (if (pair? changes)
         (sync-doc st (ref params 'textDocument 'uri)
                   (hash-ref (last changes) 'text))
         (values st '()))]
    ["textDocument/didSave" (values st '())]
    ["textDocument/didClose"
     (define uri (ref params 'textDocument 'uri))
     (values (struct-copy lsp-state st
                          [docs (hash-remove (lsp-state-docs st) uri)])
             (list (notification "textDocument/publishDiagnostics"
                                 (hasheq 'uri uri 'diagnostics '()))))]
    ["textDocument/hover" (respond (lambda () (hover st params)))]
    ["textDocument/definition" (respond (lambda () (definition st params)))]
    ["textDocument/documentSymbol"
     (respond (lambda () (document-symbols st params)))]
    ["textDocument/completion" (respond (lambda () (completion st params)))]
    [_ (if id
           (values st (list (error-response id -32601
                                            (format "unknown method: ~a" method))))
           (values st '()))]))

(define capabilities
  (hasheq 'textDocumentSync (hasheq 'openClose #t 'change 1)   ; full sync
          'hoverProvider #t
          'definitionProvider #t
          'documentSymbolProvider #t
          'completionProvider (hasheq 'triggerCharacters '())))

;; ----- documents and diagnostics -------------------------------------------------

(define (sync-doc st uri text)
  (define a (analyze-text text (uri->path uri)))
  (values (struct-copy lsp-state st
                       [docs (hash-set (lsp-state-docs st) uri a)])
          (list (notification
                 "textDocument/publishDiagnostics"
                 (hasheq 'uri uri
                         'diagnostics (map diag->lsp
                                           (analysis-diagnostics a)))))))

(define (diag->lsp d)
  (hasheq 'range (srcloc->range (diag-srcloc d))
          'severity (case (diag-severity d) [(error) 1] [(warning) 2] [else 3])
          'source "rackton"
          'message (diag-message d)))

;; ----- requests --------------------------------------------------------------------

(define (doc-of st params)
  (hash-ref (lsp-state-docs st) (ref params 'textDocument 'uri) #f))

;; LSP 0-based line / char → name-at's 1-based line / 0-based col.
(define (name-at-params a params)
  (name-at a
           (add1 (ref params 'position 'line))
           (ref params 'position 'character)))

(define (hover st params)
  (define a (doc-of st params))
  (cond
    [(not a) 'null]
    [else
     (define-values (sym _site) (name-at-params a params))
     (define text (and sym (hover-text a sym)))
     (if text
         (hasheq 'contents (hasheq 'kind "plaintext" 'value text))
         'null)]))

(define (hover-text a sym)
  (define env (analysis-env a))
  (define sch (analysis-scheme-of a sym))
  (cond
    [sch (format "~s :: ~a" sym (scheme->datum sch))]
    [(and env (env-ref-class env sym)) (format "~s — class" sym)]
    [(and env (env-ref-tcon env sym))
     (format "~s — type constructor (arity ~a)"
             sym (tcon-info-arity (env-ref-tcon env sym)))]
    [else #f]))

(define (definition st params)
  (define a (doc-of st params))
  (cond
    [(not a) '()]
    [else
     (define-values (sym site) (name-at-params a params))
     (cond
       [(not sym) '()]
       [site (list (srcloc->location (defsite-srcloc site)))]
       [else
        ;; Imported: the first required module whose sidecar defs
        ;; table locates the name.
        (or (for*/first ([rp (in-list (analysis-requires a))]
                         [e (in-list (module-index-entries rp))]
                         #:when (and (eq? (index-entry-name e) sym)
                                     (index-entry-srcloc e)
                                     (srcloc-line (index-entry-srcloc e))))
              (list (srcloc->location
                     (struct-copy srcloc (index-entry-srcloc e)
                                  [source (index-entry-module e)]))))
            '())])]))

(define (document-symbols st params)
  (define a (doc-of st params))
  (cond
    [(not a) '()]
    [else
     (for/list ([(name site) (in-hash (analysis-defs a))])
       (hasheq 'name (symbol->string name)
               'kind (case (defsite-kind site)
                       [(class) 5] [(method) 6] [(constructor) 9]
                       [(type) 23] [else 12])
               'location (srcloc->location (defsite-srcloc site))))]))

;; Completion works mid-edit: when the buffer doesn't currently
;; analyze, candidates come from the last parse's definition sites,
;; the prelude, and the keywords — never nothing.
(define (completion st params)
  (define a (doc-of st params))
  (cond
    [(not a) '()]
    [else
     (define prefix (prefix-at a params))
     (define env (or (analysis-env a) prelude-env))
     (define candidates
       (sort
        (remove-duplicates
         (filter (lambda (s) (string-prefix? s prefix))
                 (append rackton-keyword-names
                         (map symbol->string (hash-keys (analysis-defs a)))
                         (names-of env))))
        string<?))
     (for/list ([c (in-list candidates)])
       (hasheq 'label c 'kind (candidate-kind a env (string->symbol c))))]))

(define (names-of env)
  (for/list ([n (in-list (append (hash-keys (env-vars env))
                                 (hash-keys (env-data-ctors env))
                                 (hash-keys (env-classes env))
                                 (hash-keys (env-tcons env))))]
             #:unless (char=? (string-ref (symbol->string n) 0) #\$))
    (symbol->string n)))

(define (candidate-kind a env sym)
  (define site (analysis-def-of a sym))
  (cond
    [(memq sym (map string->symbol rackton-keyword-names)) 14]  ; Keyword
    [(and site (eq? (defsite-kind site) 'constructor)) 4]
    [(env-ref-data env sym) 4]                                  ; Constructor
    [(env-ref-class env sym) 7]                                 ; Class
    [(env-ref-tcon env sym) 22]                                 ; Struct
    [else 12]))                                                 ; Value

;; The word being typed: characters before the cursor up to a
;; delimiter.
(define (prefix-at a params)
  (define text (analysis-text a))
  (define line (ref params 'position 'line))
  (define char (ref params 'position 'character))
  (define l (list-ref-safe (string-split text "\n" #:trim? #f) line))
  (cond
    [(not l) ""]
    [else
     (define upto (substring l 0 (min char (string-length l))))
     (define start
       (let loop ([i (string-length upto)])
         (if (and (> i 0) (word-char? (string-ref upto (sub1 i))))
             (loop (sub1 i))
             i)))
     (substring upto start)]))

(define (list-ref-safe lst i)
  (and (>= i 0) (< i (length lst)) (list-ref lst i)))

(define (word-char? c)
  (not (or (char-whitespace? c)
           (memv c '(#\( #\) #\[ #\] #\{ #\} #\" #\; #\' #\` #\,)))))

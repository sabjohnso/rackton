#lang racket/base

;; Tests for lsp.rkt — the LSP server's pure parts: Content-Length
;; framing, file URIs, and the message handlers, driven as plain
;; functions on jsexpr values (state × message → state × outgoing).

(module+ test
  (require rackunit
           racket/file
           racket/list
           racket/string
           json
           "lsp.rkt")

  ;; ----- framing -------------------------------------------------------

  (let ([msg (hasheq 'jsonrpc "2.0" 'id 1 'method "initialize"
                     'params (hasheq 'rootUri 'null))])
    (define out (open-output-bytes))
    (write-lsp-message out msg)
    (define in (open-input-bytes (get-output-bytes out)))
    (check-equal? (read-lsp-message in) msg "framing round-trips")
    (check-pred eof-object? (read-lsp-message in)))

  ;; ----- uris ------------------------------------------------------------

  (check-equal? (uri->path (path->uri (string->path "/tmp/a b/mod.rkt")))
                (string->path "/tmp/a b/mod.rkt")
                "uri round-trip survives spaces")
  (check-true (string-prefix? (path->uri (string->path "/tmp/x.rkt"))
                              "file:///"))

  ;; ----- driving the handlers ----------------------------------------------

  (define (req id method [params (hasheq)])
    (hasheq 'jsonrpc "2.0" 'id id 'method method 'params params))
  (define (notif method [params (hasheq)])
    (hasheq 'jsonrpc "2.0" 'method method 'params params))

  (define (drive msgs)
    ;; Returns (values final-state all-outgoing).
    (for/fold ([st (make-lsp-state)] [outs '()]
               #:result (values st (reverse outs)))
              ([m (in-list msgs)])
      (define-values (st* os) (handle-message st m))
      (values st* (append (reverse os) outs))))

  (define (response-for outs id)
    (for/first ([o (in-list outs)] #:when (equal? (hash-ref o 'id #f) id)) o))
  (define (notifications-for outs method)
    (for/list ([o (in-list outs)]
               #:when (equal? (hash-ref o 'method #f) method))
      o))

  (define dir (make-temporary-file "rackton-lsp-~a" 'directory))
  (define mod-path (build-path dir "mod.rkt"))
  (define mod-uri (path->uri mod-path))

  (define clean-text
    (string-join '("#lang rackton"
                   "(: inc (-> Integer Integer))"
                   "(define (inc x) (+ x 1))"
                   "(data Box (MkBox Integer))"
                   "(: y Integer)"
                   "(define y (inc 41))")
                 "\n"))
  (define broken-text
    "#lang rackton\n(: bad Integer)\n(define bad (+ 1 \"x\"))\n")

  (define (open-msg text) (notif "textDocument/didOpen"
                                 (hasheq 'textDocument
                                         (hasheq 'uri mod-uri 'text text))))
  (define (change-msg text)
    (notif "textDocument/didChange"
           (hasheq 'textDocument (hasheq 'uri mod-uri)
                   'contentChanges (list (hasheq 'text text)))))
  (define (pos line char) (hasheq 'line line 'character char))
  (define (at-msg id method line char)
    (req id method (hasheq 'textDocument (hasheq 'uri mod-uri)
                           'position (pos line char))))

  ;; initialize
  (let-values ([(_st outs) (drive (list (req 1 "initialize"
                                             (hasheq 'rootUri 'null))))])
    (define r (response-for outs 1))
    (define caps (hash-ref (hash-ref r 'result) 'capabilities))
    (check-equal? (hash-ref (hash-ref caps 'textDocumentSync) 'change) 1)
    (check-true (hash-ref caps 'hoverProvider))
    (check-true (hash-ref caps 'definitionProvider))
    (check-true (hash-ref caps 'documentSymbolProvider)))

  ;; diagnostics on open; cleared by a fixing change
  (let-values ([(_st outs) (drive (list (open-msg broken-text)
                                        (change-msg clean-text)))])
    (define pubs (notifications-for outs "textDocument/publishDiagnostics"))
    (check-equal? (length pubs) 2)
    (define first-diags (hash-ref (hash-ref (first pubs) 'params) 'diagnostics))
    (check-equal? (length first-diags) 1)
    (check-true (>= (hash-ref (hash-ref (hash-ref (first first-diags) 'range)
                                        'start)
                              'line)
                    1)
                "0-based line points into the file")
    (check-equal? (hash-ref (hash-ref (second pubs) 'params) 'diagnostics)
                  '()))

  ;; hover: `inc` use on line 6 (0-based 5), column 11
  (let-values ([(_st outs) (drive (list (open-msg clean-text)
                                        (at-msg 2 "textDocument/hover" 5 11)))])
    (define r (response-for outs 2))
    (define contents (hash-ref (hash-ref r 'result) 'contents))
    (check-regexp-match #rx"inc :: \\(-> Integer Integer\\)"
                        (hash-ref contents 'value)))

  ;; hover over whitespace: null result
  (let-values ([(_st outs) (drive (list (open-msg clean-text)
                                        (at-msg 3 "textDocument/hover" 5 9)))])
    (check-equal? (hash-ref (response-for outs 3) 'result) 'null))

  ;; definition: the same `inc` use resolves to its define on line 3 (0-based 2)
  (let-values ([(_st outs) (drive (list (open-msg clean-text)
                                        (at-msg 4 "textDocument/definition" 5 11)))])
    (define locs (hash-ref (response-for outs 4) 'result))
    (check-equal? (length locs) 1)
    (check-equal? (hash-ref (first locs) 'uri) mod-uri)
    (check-equal? (hash-ref (hash-ref (hash-ref (first locs) 'range) 'start)
                            'line)
                  2))

  ;; documentSymbol lists the definitions
  (let-values ([(_st outs) (drive (list (open-msg clean-text)
                                        (req 5 "textDocument/documentSymbol"
                                             (hasheq 'textDocument
                                                     (hasheq 'uri mod-uri)))))])
    (define syms (hash-ref (response-for outs 5) 'result))
    (check-true (for/or ([s (in-list syms)])
                  (equal? (hash-ref s 'name) "inc")))
    (check-true (for/or ([s (in-list syms)])
                  (equal? (hash-ref s 'name) "Box"))))

  ;; completion: prefix `in` on a fresh line offers inc; `defi` offers define
  (let ([text (string-append clean-text "\nin")])
    (let-values ([(_st outs) (drive (list (open-msg text)
                                          (at-msg 6 "textDocument/completion" 6 2)))])
      (define items (hash-ref (response-for outs 6) 'result))
      (check-true (for/or ([i (in-list items)])
                    (equal? (hash-ref i 'label) "inc")))))
  (let ([text (string-append clean-text "\ndefi")])
    (let-values ([(_st outs) (drive (list (open-msg text)
                                          (at-msg 7 "textDocument/completion" 6 4)))])
      (define items (hash-ref (response-for outs 7) 'result))
      (check-true (for/or ([i (in-list items)])
                    (equal? (hash-ref i 'label) "define")))))

  ;; completion inside a require offers module paths, not env names, and
  ;; says which text it replaces — the client's own idea of a word would
  ;; stop at the slashes.
  (let ([text (string-append clean-text "\n(require rackton/data/may")])
    (let-values ([(_st outs) (drive (list (open-msg text)
                                          (at-msg 20 "textDocument/completion" 6 25)))])
      (define items (hash-ref (response-for outs 20) 'result))
      (define item
        (for/first ([i (in-list items)]
                    #:when (equal? (hash-ref i 'label) "rackton/data/maybe"))
          i))
      (check-not-false item "the module path is offered")
      (check-false (for/or ([i (in-list items)])
                     (equal? (hash-ref i 'label) "define"))
                   "environment names are not offered in a module position")
      (define range (hash-ref (hash-ref item 'textEdit) 'range))
      (check-equal? (hash-ref (hash-ref range 'start) 'character) 9
                    "the edit replaces the whole path, from just after `require`")
      (check-equal? (hash-ref (hash-ref range 'end) 'character) 25)
      (check-equal? (hash-ref (hash-ref item 'textEdit) 'newText)
                    "rackton/data/maybe")))

  ;; a require sub-form wraps a module reference; the wrapped position
  ;; completes the same way
  (let ([text (string-append clean-text "\n(require (only-in rackton/data/may")])
    (let-values ([(_st outs) (drive (list (open-msg text)
                                          (at-msg 21 "textDocument/completion" 6 34)))])
      (define items (hash-ref (response-for outs 21) 'result))
      (check-true (for/or ([i (in-list items)])
                    (equal? (hash-ref i 'label) "rackton/data/maybe")))))

  ;; outside a require the same text is an ordinary name again
  (let ([text (string-append clean-text "\n(defi")])
    (let-values ([(_st outs) (drive (list (open-msg text)
                                          (at-msg 22 "textDocument/completion" 6 5)))])
      (define items (hash-ref (response-for outs 22) 'result))
      (check-true (for/or ([i (in-list items)])
                    (equal? (hash-ref i 'label) "define")))))

  ;; cross-module definition through the sidecar defs table
  (define lib-path (build-path dir "lib.rkt"))
  (call-with-output-file lib-path #:exists 'truncate
    (lambda (out)
      (displayln "#lang rackton" out)
      (displayln "(provide thing)" out)
      (displayln "(: thing Integer)" out)
      (displayln "(define thing 42)" out)))
  (let ([text "#lang rackton\n(require \"lib.rkt\")\n(: m Integer)\n(define m thing)\n"])
    (let-values ([(_st outs) (drive (list (open-msg text)
                                          (at-msg 8 "textDocument/definition" 3 10)))])
      (define locs (hash-ref (response-for outs 8) 'result))
      (check-equal? (length locs) 1)
      (check-equal? (hash-ref (first locs) 'uri) (path->uri lib-path))
      (check-equal? (hash-ref (hash-ref (hash-ref (first locs) 'range) 'start)
                              'line)
                    3
                    "the sidecar's line 4, 0-based")))

  ;; a string spec completes to file paths, anchored at the document's own
  ;; directory — where `require` would resolve it — not the server's
  ;; working directory
  (let ([text "#lang rackton\n(require \"li"])
    (let-values ([(_st outs) (drive (list (open-msg text)
                                          (at-msg 24 "textDocument/completion" 1 25)))])
      (define items (hash-ref (response-for outs 24) 'result))
      (check-not-false
       (for/or ([i (in-list items)]) (equal? (hash-ref i 'label) "lib.rkt"))
       "the sibling module beside this document is offered")))

  ;; unknown request → MethodNotFound; shutdown/exit
  (let-values ([(st outs) (drive (list (req 9 "workspace/zap")
                                       (req 10 "shutdown")
                                       (notif "exit")))])
    (check-equal? (hash-ref (hash-ref (response-for outs 9) 'error) 'code)
                  -32601)
    (check-equal? (hash-ref (response-for outs 10) 'result) 'null)
    (check-true (lsp-state-done? st)))

  (delete-directory/files dir))

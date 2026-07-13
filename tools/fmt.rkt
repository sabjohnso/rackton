#lang rackton

;; tools/fmt.rkt — the Rackton source formatter command-line interface,
;; built with rackton/cmdline over rackton/tools/fmt-lib.
;;
;; Usage:
;;   racket tools/fmt.rkt [OPTION]... [FILE]...
;;
;;   -w, --width N   target line width (default 80)
;;       --write     format files in place
;;       --check     exit non-zero if any file would change (format
;;                   nothing)
;;       --help / --version
;;
;; With no FILE it formats standard input to standard output; with
;; files it prints each formatted file to standard output unless
;; --write or --check is given.  (Reflow mode only for now.)

(require rackton/cmdline
         rackton/tools/fmt-lib
         rackton/system)

;; ----- the work ------------------------------------------------------

;; reflow (re-emit to the width, the default) or reindent (keep the
;; author's line breaks, fix indentation).
(: format-src (-> Boolean Integer String String))
(define (format-src reindent? width src)
  (if reindent? (reindent-source src) (reflow-source width src)))

;; format each file; return whether any file's output differs from its
;; input.  --write replaces the file; --check measures only; otherwise
;; the formatted text is printed.
(: process-files (-> Boolean Integer Boolean Boolean (List String) (IO Boolean)))
(define (process-files reindent? width check write files)
  (match files
    [(Nil) (pure #f)]
    [(Cons f rest)
     (let& ([src (read-file f)])
       (let ([out (format-src reindent? width src)])
         (let& ([_ (cond
                     [write (write-file f out)]
                     [check (pure Unit)]
                     [else  (print out)])]
                [rest-changed (process-files reindent? width check write rest)])
           (pure (if (and check (not (== out src))) #t rest-changed)))))]))

(: run-fmt (-> Boolean Integer Boolean Boolean (List String) (IO Unit)))
(define (run-fmt reindent? width check write files)
  (match files
    [(Nil) (let& ([src get-contents]) (print (format-src reindent? width src)))]
    [_     (let& ([changed (process-files reindent? width check write files)])
             (if (and check changed) (exit-with-code 1) (pure Unit)))]))

;; ----- the command-line interface -----------------------------------

(: fmt-term (Term (IO Unit)))
(define fmt-term
  (let+ ([reindent? (value (flag (info-doc "Keep the author's line breaks; fix only indentation"
                                           (arg-info (list "reindent")))))]
         [width (value (opt conv-int 80
                            (info-docv "N"
                                       (info-doc "Target line width for reflow (default 80)"
                                                 (arg-info (list "w" "width"))))))]
         [check (value (flag (info-doc "Exit non-zero if any file would change"
                                       (arg-info (list "check")))))]
         [write (value (flag (info-doc "Format files in place"
                                       (arg-info (list "write")))))]
         [files (value (pos-all conv-string Nil
                                (info-docv "FILE"
                                           (info-doc "Files to format; standard input if none"
                                                     (arg-info Nil)))))])
    (run-fmt reindent? width check write files)))

(: fmt-cmd (Cmd (IO Unit)))
(define fmt-cmd
  (cmd-v (cmd-version "0.1"
                      (cmd-doc "format Rackton source by reflowing it to a width"
                               (cmd-info "rackton-fmt")))
         fmt-term))

(: main (IO Unit))
(define main (let& ([code (eval fmt-cmd)]) (exit-with-code code)))

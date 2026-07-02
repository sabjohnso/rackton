#lang racket/base

;; Milestone 1 of InteractiveDevelopment.org §3.7: the `,reload` command.
;;
;; `,reload "m.rkt"` must, for an edited-on-disk module:
;;   - recompile + re-instantiate its runtime bindings in the session
;;     namespace (dynamic-rerequire), and
;;   - re-fold its `rackton-schemes` sidecar so the REPL's type view is
;;     fresh too,
;; so a name whose value/type changed on disk reflects the change after
;; `,reload`.  A value built BEFORE the reload must still match the
;; reinstantiated type (the Tier-V representation guarantee).
;;
;; These tests drive the pure kernel (`rackton-repl-step`) over a real
;; temp module on disk, editing the file between steps.  They are RED
;; until the `,reload` command exists: today the command is unrecognised,
;; so the module is not reloaded and the post-reload observations still
;; show the stale value/type.

(require rackunit
         racket/file
         "../private/repl.rkt")

;; foo : Integer  →  foo : String
(define foo-v1 "#lang rackton\n(provide foo)\n(define foo 0)\n")
(define foo-v2 "#lang rackton\n(provide foo)\n(define foo \"hi\")\n")

;; adds a data type whose values must survive reinstantiation
(define wrap-v1
  (string-append "#lang rackton\n"
                 "(provide foo (data-out Wrap))\n"
                 "(data (Wrap a) (Wrapped a))\n"
                 "(define foo 0)\n"))
(define wrap-v2
  (string-append "#lang rackton\n"
                 "(provide foo (data-out Wrap))\n"
                 "(data (Wrap a) (Wrapped a))\n"
                 "(define foo \"hi\")\n"))

(define (write-file! path s)
  (call-with-output-file path #:exists 'replace (lambda (o) (display s o))))

;; Bump the source mtime strictly past the freshly-written .zo so the
;; compilation manager recompiles on the next require/rerequire.
(define (touch-future! path)
  (file-or-directory-modify-seconds path (+ (current-seconds) 5)))

(test-case "REPL: ,reload refreshes a changed module's value and type"
  (define dir (make-temporary-file "rktreload~a" 'directory))
  (define mod (build-path dir "m.rkt"))
  (dynamic-wind
   void
   (lambda ()
     (write-file! mod foo-v1)
     (parameterize ([current-directory dir])
       (define s0 (rackton-repl-init))
       (define-values (s1 _o1) (rackton-repl-step s0 '(require "m.rkt")))
       (define-values (s2 o2)  (rackton-repl-step s1 'foo))
       ;; sanity — before the edit, foo is 0 :: Integer
       (check-regexp-match #rx"0 :: " o2)
       (check-regexp-match #rx"Integer" o2)
       ;; edit the module on disk: foo becomes a String
       (write-file! mod foo-v2)
       (touch-future! mod)
       (define-values (s3 _o3) (rackton-repl-step s2 '(unquote reload "m.rkt")))
       (define-values (_s4 o4) (rackton-repl-step s3 'foo))
       ;; the reload must make the new value AND the new type visible
       (check-regexp-match #rx"hi" o4)
       (check-regexp-match #rx"String" o4)
       (check-false (regexp-match? #rx"Integer" o4) o4)))
   (lambda () (delete-directory/files dir))))

(test-case "REPL: a value built before ,reload still matches after it"
  (define dir (make-temporary-file "rktreload~a" 'directory))
  (define mod (build-path dir "m.rkt"))
  (dynamic-wind
   void
   (lambda ()
     (write-file! mod wrap-v1)
     (parameterize ([current-directory dir])
       (define s0 (rackton-repl-init))
       (define-values (s1 _o1) (rackton-repl-step s0 '(require "m.rkt")))
       ;; build a value from the module's type, held in a session binding
       (define-values (s2 _o2) (rackton-repl-step s1 '(define c (Wrapped 0))))
       ;; edit + reload (foo changes; Wrap stays)
       (write-file! mod wrap-v2)
       (touch-future! mod)
       (define-values (s3 _o3) (rackton-repl-step s2 '(unquote reload "m.rkt")))
       ;; the reload happened: foo is now a String
       (define-values (s4 o4) (rackton-repl-step s3 'foo))
       (check-regexp-match #rx"String" o4)
       ;; and the pre-reload value still matches the reinstantiated ctor
       (define-values (_s5 o5)
         (rackton-repl-step s4 '(match c ((Wrapped x) x))))
       (check-regexp-match #rx"^0 :: " o5)))
   (lambda () (delete-directory/files dir))))

(test-case "REPL: ,reload with no argument reloads every required module"
  (define dir (make-temporary-file "rktreload~a" 'directory))
  (define a (build-path dir "a.rkt"))
  (define b (build-path dir "b.rkt"))
  (dynamic-wind
   void
   (lambda ()
     (write-file! a "#lang rackton\n(provide fa)\n(define fa 0)\n")
     (write-file! b "#lang rackton\n(provide fb)\n(define fb 0)\n")
     (parameterize ([current-directory dir])
       (define s0 (rackton-repl-init))
       (define-values (s1 _1) (rackton-repl-step s0 '(require "a.rkt")))
       (define-values (s2 _2) (rackton-repl-step s1 '(require "b.rkt")))
       ;; edit both modules on disk
       (write-file! a "#lang rackton\n(provide fa)\n(define fa \"A\")\n")
       (write-file! b "#lang rackton\n(provide fb)\n(define fb \"B\")\n")
       (touch-future! a)
       (touch-future! b)
       ;; a bare ,reload must refresh EVERY module required this session
       (define-values (s3 _3) (rackton-repl-step s2 '(unquote reload)))
       (define-values (s4 oa) (rackton-repl-step s3 'fa))
       (define-values (_s5 ob) (rackton-repl-step s4 'fb))
       (check-regexp-match #rx"A" oa)
       (check-regexp-match #rx"String" oa)
       (check-regexp-match #rx"B" ob)
       (check-regexp-match #rx"String" ob)))
   (lambda () (delete-directory/files dir))))

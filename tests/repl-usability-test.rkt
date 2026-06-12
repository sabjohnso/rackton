#lang racket/base

;; REPL usability: the ,source command (and the function-define echo).
;;
;; ,source NAME prints the input form that bound NAME — a session
;; definition shows the form as typed, a prelude name shows its
;; definition in the prelude source, a class shows its definition plus
;; the instances the session has seen.  These tests drive the kernel
;; directly, like repl-info-test.rkt.

(require rackunit
         "../private/repl.rkt")

(define (drive-session inputs)
  ;; Returns (values final-state outputs), where outputs is the list
  ;; of per-step output strings.
  (for/fold ([state (rackton-repl-init)] [out '()] #:result (values state (reverse out)))
            ([form (in-list inputs)])
    (define-values (state* o) (rackton-repl-step state form))
    (values state* (cons o out))))

(define (last-output inputs)
  (define-values (_ outs) (drive-session inputs))
  (car (reverse outs)))

;; ----- ,source on session definitions -----------------------------

(test-case ",source on a session define shows the define form"
  (define out (last-output '((define inc (lambda (x) (+ x 1)))
                             (unquote source inc))))
  (check-regexp-match #rx"\\(define inc" out))

(test-case ",src is an alias for ,source"
  (define out (last-output '((define inc (lambda (x) (+ x 1)))
                             (unquote src inc))))
  (check-regexp-match #rx"\\(define inc" out))

(test-case ",source on a redefined name shows only the latest define"
  (define out (last-output '((define n 1)
                             (define n 2)
                             (unquote source n))))
  (check-regexp-match #rx"\\(define n 2\\)" out)
  (check-false (regexp-match #rx"\\(define n 1\\)" out)))

(test-case ",source on a data type and on its constructor both show the data form"
  (define session '((data (Pair a b) (MkPair a b))
                    (unquote source Pair)))
  (check-regexp-match #rx"\\(data \\(Pair a b\\)" (last-output session))
  (check-regexp-match #rx"\\(data \\(Pair a b\\)"
                      (last-output '((data (Pair a b) (MkPair a b))
                                     (unquote source MkPair)))))

(test-case ",source on a standalone signature shows the declaration"
  (define out (last-output '((: g (-> Integer Integer))
                             (unquote source g))))
  (check-regexp-match #rx"\\(: g" out))

(test-case "a define after a signature supersedes it in ,source"
  (define out (last-output '((: g (-> Integer Integer))
                             (define (g x) x)
                             (unquote source g))))
  (check-regexp-match #rx"\\(define \\(g x\\)" out)
  (check-false (regexp-match #rx"\\(: g" out)))

;; ----- ,source on classes and instances ----------------------------

(test-case ",source on a class shows the protocol and its session instances"
  (define out (last-output '((protocol (Frob a) (: frob (-> a a)))
                             (instance (Frob Integer) (define (frob x) x))
                             (unquote source Frob))))
  (check-regexp-match #rx"\\(protocol \\(Frob a\\)" out)
  (check-regexp-match #rx"\\(instance \\(Frob Integer\\)" out))

(test-case ",source on a method shows the protocol"
  (define out (last-output '((protocol (Frob a) (: frob (-> a a)))
                             (unquote source frob))))
  (check-regexp-match #rx"\\(protocol \\(Frob a\\)" out))

(test-case "a re-evaluated instance is not duplicated in ,source"
  (define out (last-output '((protocol (Frob a) (: frob (-> a a)))
                             (instance (Frob Integer) (define (frob x) x))
                             (instance (Frob Integer) (define (frob x) x))
                             (unquote source Frob))))
  (define matches (regexp-match* #rx"\\(instance \\(Frob Integer\\)" out))
  (check-equal? (length matches) 1))

;; ----- ,source on prelude and unbound names ------------------------

(test-case ",source on a prelude type shows its prelude definition"
  (define out (last-output '((unquote source Maybe))))
  (check-regexp-match #rx"\\(data \\(Maybe a\\)" out))

(test-case ",source on a prelude class shows the protocol form"
  ;; The form may pretty-print across lines, so allow whitespace
  ;; (including a newline) between `protocol` and the head.
  (define out (last-output '((unquote source Eq))))
  (check-regexp-match #px"\\(protocol\\s+\\(Eq a\\)" out))

(test-case ",source on an unbound name says so"
  (define out (last-output '((unquote source zzzzz))))
  (check-regexp-match #rx"unbound" out))

;; ----- ,source on session macros -----------------------------------

(test-case ",source on a session macro shows its definition"
  (define out (last-output '((define-syntax-rule (twice e) (begin e e))
                             (unquote source twice))))
  (check-regexp-match #rx"define-syntax-rule \\(twice e\\)" out))

;; ----- ,accepts TYPE ------------------------------------------------

(test-case ",accepts lists a session function whose argument has the queried type"
  (define out (last-output '((: f (-> Integer Integer))
                             (define (f x) x)
                             (unquote accepts Integer))))
  (check-regexp-match #rx"f :: " out))

(test-case ",a is an alias for ,accepts"
  (define out (last-output '((: f (-> Integer Integer))
                             (define (f x) x)
                             (unquote a Integer))))
  (check-regexp-match #rx"f :: " out))

(test-case ",accepts on a parameterized type matches prelude functions"
  (define out (last-output '((unquote accepts (List Integer)))))
  (check-regexp-match #rx"length :: " out)
  (check-regexp-match #rx"reverse :: " out))

(test-case ",accepts keeps constrained matches whose instance exists, drops impossible ones"
  (define out (last-output '((unquote accepts (List Integer)))))
  ;; fmap's (f a) argument matches with f = List, and (Functor List)
  ;; exists — keep.  censor's (m a) argument also unifies, but no
  ;; (MonadWriter w List) instance can ever apply — drop.
  (check-regexp-match #rx"fmap :: " out)
  (check-false (regexp-match #rx"censor :: " out)))

(test-case ",accepts does not list functions whose argument is a bare type variable"
  (define out (last-output '((define (idf x) x)
                             (unquote accepts Integer))))
  (check-false (regexp-match #rx"idf :: " out)))

(test-case ",accepts lists constrained-variable functions whose instance exists"
  ;; + :: (Num a) => (-> a (-> a a)) and (Num Integer) exists.
  (define out (last-output '((unquote accepts Integer))))
  (check-regexp-match #px"(?m:^\\+ :: )" out))

(test-case ",accepts drops constrained-variable functions with no instance for the query"
  (define out (last-output '((protocol (Zap a) (: zap (-> a a)))
                             (unquote accepts Integer))))
  (check-false (regexp-match #rx"zap :: " out)))

(test-case "an instance makes a constrained-variable function searchable"
  (define out (last-output '((protocol (Zap a) (: zap (-> a a)))
                             (instance (Zap Integer) (define (zap x) x))
                             (unquote accepts Integer))))
  (check-regexp-match #rx"zap :: " out))

(test-case ",accepts with a type-variable argument in the query unifies it"
  (define out (last-output '((: g (-> (Maybe Integer) Integer))
                             (define (g m) 0)
                             (unquote accepts (Maybe a)))))
  (check-regexp-match #rx"g :: " out))

(test-case ",accepts lists data constructors taking the type"
  (define out (last-output '((data Box (MkBox Integer))
                             (unquote accepts Integer))))
  (check-regexp-match #rx"MkBox :: " out))

(test-case ",accepts on a bare type variable explains instead of listing everything"
  (define out (last-output '((unquote accepts a))))
  (check-regexp-match #rx"every function" out))

(test-case ",accepts with no matches says so"
  (define out (last-output '((data Blank MkBlank)
                             (unquote accepts Blank))))
  (check-regexp-match #rx"no functions" out))

(test-case ",accepts on a malformed type reports an error"
  (define out (last-output '((unquote accepts 5))))
  (check-regexp-match #rx"error" out))

;; ----- keyword completion --------------------------------------------

(test-case "surface keywords are completion candidates"
  (define state (rackton-repl-init))
  (check-not-false (member "protocol" (rackton-repl-completions state "prot")))
  (check-not-false (member "define-alias" (rackton-repl-completions state "define")))
  (check-not-false (member "match" (rackton-repl-completions state "mat"))))

;; ----- ,keys ---------------------------------------------------------

(test-case ",keys lists the editor's structural and history bindings"
  (define out (last-output '((unquote keys))))
  (check-regexp-match #rx"C-M-f" out)        ; forward s-expression
  (check-regexp-match #rx"M-\\(" out)        ; wrap next s-expression
  (check-regexp-match #rx"C-right" out)      ; slurp forward
  (check-regexp-match #rx"M-p" out))         ; history prefix search

(test-case ",help mentions ,keys, ,source, and ,accepts"
  (define out (last-output '((unquote help))))
  (check-regexp-match #rx",keys" out)
  (check-regexp-match #rx",source" out)
  (check-regexp-match #rx",accepts" out))

;; ----- ,colors --------------------------------------------------------

;; ,colors reads and writes the editor palette; tests isolate the
;; persistence behind a temporary preferences file.

(require racket/file
         (only-in "../private/repl-term.rkt"
                  current-color-pref-file
                  current-color-scheme
                  current-color-overrides))

(define (with-color-sandbox proc)
  (define pref-file (make-temporary-file "rackton-colors-kernel-~a"))
  ;; An empty file is not valid preference format; start absent.
  (delete-file pref-file)
  (dynamic-wind
   void
   (lambda ()
     (parameterize ([current-color-pref-file pref-file]
                    [current-color-scheme 'standard]
                    [current-color-overrides '()])
       (proc)))
   (lambda () (when (file-exists? pref-file) (delete-file pref-file)))))

(test-case ",colors lists the scheme and every category"
  (with-color-sandbox
   (lambda ()
     (define out (last-output '((unquote colors))))
     (check-regexp-match #rx"scheme: standard" out)
     (check-regexp-match #rx"type *cyan" out)
     (check-regexp-match #rx"constructor *magenta" out))))

(test-case ",colors SCHEME switches schemes"
  (with-color-sandbox
   (lambda ()
     (define out (last-output '((unquote colors plain)
                                (unquote colors))))
     (check-regexp-match #rx"scheme: plain" out))))

(test-case ",colors CATEGORY COLOR overrides one category"
  (with-color-sandbox
   (lambda ()
     (define out (last-output '((unquote colors type light-cyan)
                                (unquote colors))))
     (check-regexp-match #rx"type *light-cyan" out)
     (check-regexp-match #rx"constructor *magenta" out))))

(test-case "the editor's name classifier tells types from constructors via the env"
  (define-values (st _outs)
    (drive-session '((data Box (MkBox Integer)))))
  (check-equal? (rackton-name-kind st 'Box) 'type)
  (check-equal? (rackton-name-kind st 'MkBox) 'constructor)
  (check-equal? (rackton-name-kind st 'Maybe) 'type)
  (check-equal? (rackton-name-kind st 'Some) 'constructor)
  (check-equal? (rackton-name-kind st 'Monad) 'type
                "classes count as types")
  (check-false (rackton-name-kind st 'Wibble)
               "an unknown capitalized name is unclassified"))

(test-case ",colors rejects unknown schemes, categories, and colors"
  (with-color-sandbox
   (lambda ()
     (check-regexp-match #rx"unknown" (last-output '((unquote colors mauve))))
     (check-regexp-match #rx"unknown"
                         (last-output '((unquote colors sparkles red))))
     (check-regexp-match #rx"unknown"
                         (last-output '((unquote colors type mauve)))))))

;; ----- function-define echo ----------------------------------------

(test-case "a function define echoes its name and inferred type"
  (define out (last-output '((define (add1* x) (+ x 1)))))
  (check-regexp-match #rx"add1\\* :: \\(-> Integer Integer\\)" out))

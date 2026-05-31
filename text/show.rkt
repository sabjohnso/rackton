#lang rackton

;; rackton/text/show — Text.Show's ShowS machinery.  A ShowS is a
;; @racket[(-> String String)]: a function that prepends some text to a
;; continuation string.  Building output by composing ShowS values
;; (rather than repeatedly @racket[<>]-ing Strings) keeps concatenation
;; linear, since each piece is prepended to the tail exactly once.
;;
;; The Show class itself and @racket[show] are in the prelude; these are
;; the freestanding combinators.  (Haskell's showList is a Show method,
;; so it stays with the class, not here.)

(provide (all-defined-out))

(define-alias (ShowS) (-> String String))

;; prepend a literal String.
(: show-string (-> String ShowS))
(define (show-string str) (lambda (s) (string-append str s)))

;; prepend a single Char.
(: show-char (-> Char ShowS))
(define (show-char c) (lambda (s) (string-append (char->string c) s)))

;; prepend the shown form of any Showable value (Haskell's `shows`).
(: shows ((Show a) => (-> a ShowS)))
(define (shows x) (lambda (s) (string-append (show x) s)))

;; wrap a ShowS in parentheses when the flag is true (Haskell's
;; showParen — used for precedence-aware showing).
(: show-paren (-> Boolean (-> ShowS ShowS)))
(define (show-paren b p)
  (if b
      (lambda (s) (string-append "(" (p (string-append ")" s))))
      p))

;; run a ShowS against the empty continuation, yielding the String
;; (Haskell's `($ "")`).
(: run-shows (-> ShowS String))
(define (run-shows f) (f ""))

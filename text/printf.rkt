#lang rackton

;; rackton/text/printf — type-safe string formatting (Text.Printf done
;; the HM way).  Instead of a runtime-parsed "%d %s" format string, a
;; format is BUILT from typed directives and composed; the type of the
;; argument-consuming function is inferred and checked at compile time.
;; This is the "functional unparsing" technique (Danvy; the `formatting`
;; library): a wrong argument type or arity is a compile error, never a
;; runtime format mismatch.
;;
;; A @racket[(Format r a)] is, underneath, a function
;; @racket[(-> (-> String r) a)]: given a continuation that consumes the
;; rendered text and produces @racket[r], it returns @racket[a] — where
;; @racket[a] accumulates one curried parameter per value directive.
;; @racket[fmt-cat] threads the continuations; @racket[sprintf] runs a
;; format with the identity continuation, yielding @racket[a].

(provide (all-defined-out))

(define-alias (Format r a) (-> (-> String r) a))

;; --- directives ----------------------------------------------------

;; literal text — consumes no argument.
(: fmt-lit (-> String (Format r r)))
(define (fmt-lit s) (lambda (k) (k s)))

;; consume an Integer, rendered with `show`.
(: fmt-int (Format r (-> Integer r)))
(define (fmt-int k) (lambda (n) (k (show n))))

;; consume a Float, rendered with `show`.
(: fmt-flt (Format r (-> Float r)))
(define (fmt-flt k) (lambda (x) (k (show x))))

;; consume a String, inserted verbatim (no quoting — contrast fmt-show).
(: fmt-str (Format r (-> String r)))
(define (fmt-str k) (lambda (s) (k s)))

;; consume any Showable value, rendered with `show` (Strings come out
;; quoted, since that is what `show` does).
(: fmt-show ((Show a) => (Format r (-> a r))))
(define (fmt-show k) (lambda (x) (k (show x))))

;; --- composition / running -----------------------------------------

;; Concatenate two formats.  The argument functions chain: the combined
;; format takes the arguments of `f` then those of `g`.
(: fmt-cat (-> (Format b c) (-> (Format a b) (Format a c))))
(define (fmt-cat f g)
  (lambda (k)
    (f (lambda (s1) (g (lambda (s2) (k (string-append s1 s2))))))))

;; Run a format, collecting the rendered pieces into the final String.
(: sprintf (-> (Format String a) a))
(define (sprintf fmt) (fmt (lambda (s) s)))

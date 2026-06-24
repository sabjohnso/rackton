#lang rackton

;; rackton/cmdline/conv — argument converters (cmdliner's Arg.conv).
;;
;; A converter pairs a `parse` (String -> Result error value) with a
;; `print` (value -> String), plus a default value placeholder `docv`
;; used in generated help.  The parse/print pair is the converter's
;; specification: `parse (print x) = Ok x` for every value `x`.
;;
(require rackton/data/result
         rackton/text/read)   ; read-int / read-float

(provide (data-out Conv)
         conv conv-parse conv-print conv-docv
         conv-string conv-int conv-bool conv-float conv-char
         conv-some)

;; ----- the converter type ------------------------------------------

(data (Conv a)
  (Conv (-> String (Result String a))   ; parse: Err message on failure
        (-> a String)                     ; print
        String))                          ; docv (value placeholder)

(: conv (-> (-> String (Result String a)) (-> a String) String (Conv a)))
(define (conv parse print docv) (Conv parse print docv))

(: conv-parse (-> (Conv a) String (Result String a)))
(define (conv-parse c s) (match c [(Conv p _ _) (p s)]))

(: conv-print (-> (Conv a) a String))
(define (conv-print c x) (match c [(Conv _ p _) (p x)]))

(: conv-docv (-> (Conv a) String))
(define (conv-docv c) (match c [(Conv _ _ d) d]))

;; ----- predefined converters ---------------------------------------

;; Lift a Maybe-returning reader into the Result-returning parse,
;; tagging the failure with the rejected token.
(: read->parse (-> String (-> String (Maybe a)) String (Result String a)))
(define (read->parse what reader s)
  (match (reader s)
    [(Some x) (Ok x)]
    [(None)   (Err (string-append "not a "
                                  (string-append what (string-append ": " s))))]))

(: conv-string (Conv String))
(define conv-string (conv (lambda (s) (Ok s)) (lambda (s) s) "STRING"))

(: conv-int (Conv Integer))
(define conv-int
  (conv (lambda (s) (read->parse "integer" read-int s)) show "INT"))

(: conv-float (Conv Float))
(define conv-float
  (conv (lambda (s) (read->parse "float" read-float s)) show "FLOAT"))

(: conv-bool (Conv Boolean))
(define conv-bool
  (conv (lambda (s)
          (cond [(== s "true")  (Ok #t)]
                [(== s "false") (Ok #f)]
                [else (Err (string-append "not a bool: " s))]))
        (lambda (b) (if b "true" "false"))
        "BOOL"))

(: conv-char (Conv Char))
(define conv-char
  (conv (lambda (s)
          (if (== (string-length s) 1)
              (match (string-ref s 0)
                [(Some c) (Ok c)]
                [(None)   (Err (string-append "not a char: " s))])
              (Err (string-append "not a single character: " s))))
        char->string
        "CHAR"))

;; Lift a converter to its optional form: a successful parse yields
;; (Some x).  Used with `opt … None` to feed `required` (cmdliner's
;; `Arg.some`).
(: conv-some (-> (Conv a) (Conv (Maybe a))))
(define (conv-some c)
  (conv (lambda (s) (fmap Some (conv-parse c s)))
        (lambda (m) (match m [(Some x) (conv-print c x)] [(None) ""]))
        (conv-docv c)))

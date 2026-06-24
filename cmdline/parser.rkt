#lang rackton

;; rackton/cmdline/parser — the pure argv -> ParseCtx parser.
;;
;; Given the declared options (each with its names and whether it takes
;; a value) it walks argv and records, per option, the occurrences and
;; their values, plus the positional arguments.  It implements the
;; POSIX/GNU conventions: --k=v and --k v, -ovalue and -o value,
;; combined short flags -abc, the -- terminator, lone - as a positional,
;; and unambiguous long-option prefix abbreviation.
;;
;; It is pure: environment defaults are layered in by eval (Step 6),
;; which seeds the context with env-sourced occurrences before parsing.

(require rackton/cmdline/parsed      ; ParseCtx, EvalError, mk-ctx, empty-ctx
         rackton/data/result)        ; Result, Ok, Err

(provide (struct-out OptSpec)
         parse-argv)

;; A declared option: its names (short = length 1, long = length > 1)
;; and whether it consumes a value.
(struct OptSpec
  [names       : (List String)]
  [takes-value : Boolean])

;; ----- small string / list helpers ---------------------------------

(: str-eq? (-> String String Boolean))
(define (str-eq? a b) (== a b))

(: long-name? (-> String Boolean))
(define (long-name? nm) (> (string-length nm) 1))

(: any-pred (-> (-> a Boolean) (List a) Boolean))
(define (any-pred p xs)
  (match xs [(Nil) #f] [(Cons x r) (if (p x) #t (any-pred p r))]))

;; the option's canonical (first) name, for recording occurrences.
(: canonical (-> OptSpec String))
(define (canonical s) (match (OptSpec-names s) [(Cons n _) n] [(Nil) ""]))

;; one occurrence entry (earliest-first accumulation reverses at the end).
(: occ (-> OptSpec String (Pair String (List String))))
(define (occ s v) (Pair (canonical s) (list v)))

(: flag-occ (-> OptSpec (Pair String (List String))))
(define (flag-occ s) (Pair (canonical s) (list "")))

;; ----- error messages ----------------------------------------------

(: msg2 (-> String String String))
(define (msg2 a b) (string-append a b))

(: unknown-opt (-> String EvalError))
(define (unknown-opt name) (EvErr (msg2 "unknown option " name)))

(: ambiguous-opt (-> String EvalError))
(define (ambiguous-opt name) (EvErr (msg2 "option " (msg2 name " is ambiguous"))))

(: missing-value (-> String EvalError))
(define (missing-value name) (EvErr (msg2 "option " (msg2 name " needs a value"))))

(: flag-no-value (-> String EvalError))
(define (flag-no-value name) (EvErr (msg2 "option " (msg2 name " takes no value"))))

;; ----- option resolution -------------------------------------------

;; option whose single-character name is c (a 1-char string).
(: find-short (-> (List OptSpec) String (Maybe OptSpec)))
(define (find-short specs c)
  (match specs
    [(Nil) None]
    [(Cons s rest)
     (if (any-pred (lambda (nm) (str-eq? nm c)) (OptSpec-names s))
         (Some s)
         (find-short rest c))]))

(: has-long-eq? (-> OptSpec String Boolean))
(define (has-long-eq? s name)
  (any-pred (lambda (nm) (and (long-name? nm) (str-eq? nm name))) (OptSpec-names s)))

(: has-long-prefix? (-> OptSpec String Boolean))
(define (has-long-prefix? s name)
  (any-pred (lambda (nm) (and (long-name? nm) (string-prefix? name nm))) (OptSpec-names s)))

;; resolve a long name: exact match wins; otherwise a UNIQUE prefix;
;; otherwise unknown / ambiguous.
(: find-long (-> (List OptSpec) String (Result EvalError OptSpec)))
(define (find-long specs name)
  (match (filter (lambda (s) (has-long-eq? s name)) specs)
    [(Cons s _) (Ok s)]
    [(Nil)
     (match (filter (lambda (s) (has-long-prefix? s name)) specs)
       [(Nil)          (Err (unknown-opt (msg2 "--" name)))]
       [(Cons s (Nil)) (Ok s)]
       [_              (Err (ambiguous-opt (msg2 "--" name)))])]))

;; ----- token shape -------------------------------------------------

(: long-tok? (-> String Boolean))
(define (long-tok? t) (and (string-prefix? "--" t) (> (string-length t) 2)))

(: short-tok? (-> String Boolean))
(define (short-tok? t)
  (and (string-prefix? "-" t)
       (and (> (string-length t) 1)
            (not (string-prefix? "--" t)))))

;; split "name" / "name=value" (already stripped of the leading --).
(: split-eq (-> String (Pair String (Maybe String))))
(define (split-eq s)
  (let loop ([i 0])
    (cond
      [(>= i (string-length s)) (Pair s None)]
      [(str-eq? (substring s i (+ i 1)) "=")
       (Pair (substring s 0 i) (Some (substring s (+ i 1) (string-length s))))]
      [else (loop (+ i 1))])))

;; ----- short-token processing --------------------------------------
;; Process the characters of a short token after the leading '-'.
;; Returns the remaining argv (a valued short option may consume the
;; next token) and the updated occurrence accumulator.

(: parse-short (-> (List OptSpec) String (List String)
                   (List (Pair String (List String)))
                   (Result EvalError
                           (Pair (List String) (List (Pair String (List String)))))))
(define (parse-short specs tok rest named)
  (let go ([i 1] [named named])
    (if (>= i (string-length tok))
        (Ok (Pair rest named))
        (let ([c (substring tok i (+ i 1))])
          (match (find-short specs c)
            [(None) (Err (unknown-opt (msg2 "-" c)))]
            [(Some s)
             (if (OptSpec-takes-value s)
                 (let ([inline (substring tok (+ i 1) (string-length tok))])
                   (if (> (string-length inline) 0)
                       (Ok (Pair rest (Cons (occ s inline) named)))
                       (match rest
                         [(Cons v rest2) (Ok (Pair rest2 (Cons (occ s v) named)))]
                         [(Nil) (Err (missing-value (msg2 "-" c)))])))
                 (go (+ i 1) (Cons (flag-occ s) named)))])))))

;; ----- the main walk -----------------------------------------------

(: parse-argv (-> (List OptSpec) (List String) (Result EvalError ParseCtx)))
(define (parse-argv specs argv)
  (let loop ([toks argv] [dd #f] [named Nil] [pos Nil])
    (match toks
      [(Nil) (Ok (mk-ctx (reverse named) (reverse pos)))]
      [(Cons t rest)
       (cond
         [dd          (loop rest #t named (Cons t pos))]
         [(str-eq? t "--") (loop rest #t named pos)]
         [(long-tok? t)
          (match (split-eq (substring t 2 (string-length t)))
            [(Pair name inline)
             (match (find-long specs name)
               [(Err e) (Err e)]
               [(Ok s)
                (if (OptSpec-takes-value s)
                    (match inline
                      [(Some v) (loop rest #f (Cons (occ s v) named) pos)]
                      [(None)
                       (match rest
                         [(Cons v rest2) (loop rest2 #f (Cons (occ s v) named) pos)]
                         [(Nil) (Err (missing-value (msg2 "--" name)))])])
                    (match inline
                      [(Some _) (Err (flag-no-value (msg2 "--" name)))]
                      [(None)   (loop rest #f (Cons (flag-occ s) named) pos)]))])])]
         [(short-tok? t)
          (match (parse-short specs t rest named)
            [(Err e) (Err e)]
            [(Ok (Pair rest2 named2)) (loop rest2 #f named2 pos)])]
         [else (loop rest #f named (Cons t pos))])])))

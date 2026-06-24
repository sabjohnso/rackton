#lang rackton

;; rackton/cmdline/manpage — man-page blocks and their renderers
;; (cmdliner's Cmdliner.Manpage).
;;
;; A page is a list of ManBlocks.  `render-plain` produces readable
;; text; `render-groff` produces man-page (troff) source.  Doc strings
;; may carry markup: $(b,bold) $(i,italic) $(tname) $(mname).
;;
;; The Pager and Auto backends (which need $PAGER / TTY detection) and
;; the --help / --version dispatch are IO and live with eval (Step 6).
;;
(require rackton/cmdline/arg)   ; ArgInfo accessors

(provide (data-out Format)
         (struct-out Subst)
         (data-out ManBlock)
         expand-markup
         render-plain render-groff
         name-section synopsis-section options-section exit-section)

(data Format FmtPlain FmtGroff)

;; substitutions for $(tname) (this command) and $(mname) (the tool).
(struct Subst
  [tname : String]
  [mname : String])

(data ManBlock
  (ManS String)            ; section heading
  (ManP String)            ; paragraph
  (ManPre String)          ; preformatted
  (ManI String String)     ; labelled item: label, body
  ManNoblank
  (ManBlocks (List ManBlock)))

;; ----- string helpers ----------------------------------------------

(: str-eq? (-> String String Boolean))
(define (str-eq? a b) (== a b))

(: char-at (-> String Integer String))
(define (char-at s i) (substring s i (+ i 1)))

(: app3 (-> String String String String))
(define (app3 a b c) (string-append a (string-append b c)))

;; ----- $(…) markup -------------------------------------------------

;; index of the next ")" at or after j, or -1.
(: find-close (-> String Integer Integer))
(define (find-close s j)
  (cond
    [(>= j (string-length s)) -1]
    [(str-eq? (char-at s j) ")") j]
    [else (find-close s (+ j 1))]))

;; render bold/italic per format.
(: emph (-> Format Boolean String String))
(define (emph fmt bold text)
  (match fmt
    [(FmtPlain) text]
    [(FmtGroff) (if bold (app3 "\\fB" text "\\fR") (app3 "\\fI" text "\\fR"))]))

(: starts-with? (-> String String Boolean))
(define (starts-with? p s) (string-prefix? p s))

(: render-directive (-> Format Subst String String))
(define (render-directive fmt subst body)
  (cond
    [(str-eq? body "tname") (Subst-tname subst)]
    [(str-eq? body "mname") (Subst-mname subst)]
    [(starts-with? "b," body) (emph fmt #t (substring body 2 (string-length body)))]
    [(starts-with? "i," body) (emph fmt #f (substring body 2 (string-length body)))]
    [else body]))

;; Expand $(b,…) / $(i,…) / $(tname) / $(mname); copy everything else.
(: expand-markup (-> Format Subst String String))
(define (expand-markup fmt subst s)
  (let loop ([i 0] [acc ""])
    (if (>= i (string-length s))
        acc
        (let ([ch (char-at s i)])
          (if (str-eq? ch "$")
              (if (>= (+ i 1) (string-length s))
                  (loop (+ i 1) (string-append acc ch))
                  (if (str-eq? (char-at s (+ i 1)) "(")
                      (let ([close (find-close s (+ i 2))])
                        (if (< close 0)
                            (loop (+ i 1) (string-append acc ch))
                            (loop (+ close 1)
                                  (string-append acc
                                    (render-directive fmt subst
                                                      (substring s (+ i 2) close))))))
                      (loop (+ i 1) (string-append acc ch))))
              (loop (+ i 1) (string-append acc ch)))))))

;; ----- renderers ---------------------------------------------------

(: plain-block (-> ManBlock Subst String))
(define (plain-block b subst)
  (match b
    [(ManS t)      (string-append (expand-markup FmtPlain subst t) "\n")]
    [(ManP t)      (app3 "  " (expand-markup FmtPlain subst t) "\n")]
    [(ManPre t)    (string-append (expand-markup FmtPlain subst t) "\n")]
    [(ManI l body) (string-append (app3 "  " (expand-markup FmtPlain subst l) "\n")
                                  (app3 "      " (expand-markup FmtPlain subst body) "\n"))]
    [(ManNoblank)  ""]
    [(ManBlocks bs) (render-plain bs subst)]))

(: render-plain (-> (List ManBlock) Subst String))
(define (render-plain blocks subst)
  (foldr (lambda (b acc) (string-append (plain-block b subst) acc)) "" blocks))

(: groff-block (-> ManBlock Subst String))
(define (groff-block b subst)
  (match b
    [(ManS t)      (app3 ".SH " (expand-markup FmtGroff subst t) "\n")]
    [(ManP t)      (app3 ".P\n" (expand-markup FmtGroff subst t) "\n")]
    [(ManPre t)    (app3 ".nf\n" (expand-markup FmtGroff subst t) "\n.fi\n")]
    [(ManI l body) (string-append (app3 ".TP\n" (expand-markup FmtGroff subst l) "\n")
                                  (string-append (expand-markup FmtGroff subst body) "\n"))]
    [(ManNoblank)  ".sp 0\n"]
    [(ManBlocks bs) (render-groff bs subst)]))

(: render-groff (-> (List ManBlock) Subst String))
(define (render-groff blocks subst)
  (foldr (lambda (b acc) (string-append (groff-block b subst) acc)) "" blocks))

;; ----- standard sections -------------------------------------------

;; an option name with its - / -- prefix.
(: dash (-> String String))
(define (dash nm) (if (> (string-length nm) 1) (string-append "--" nm) (string-append "-" nm)))

(: format-names (-> (List String) String))
(define (format-names names)
  (match names
    [(Nil)          ""]
    [(Cons n (Nil)) (dash n)]
    [(Cons n rest)  (app3 (dash n) ", " (format-names rest))]))

(: arginfo->item (-> ArgInfo ManBlock))
(define (arginfo->item info)
  (ManI (string-append (format-names (ArgInfo-names info))
                       (match (ArgInfo-docv info)
                         [(Some d) (string-append "=" d)]
                         [(None)   ""]))
        (ArgInfo-doc info)))

(: name-section (-> String String (List ManBlock)))
(define (name-section name doc)
  (list (ManS "NAME") (ManP (app3 name " - " doc))))

(: synopsis-section (-> String String (List ManBlock)))
(define (synopsis-section name line)
  (list (ManS "SYNOPSIS") (ManP (app3 name " " line))))

(: options-section (-> (List ArgInfo) (List ManBlock)))
(define (options-section infos)
  (Cons (ManS "OPTIONS") (fmap arginfo->item infos)))

(: exit-section (-> (List (Pair Integer String)) (List ManBlock)))
(define (exit-section codes)
  (Cons (ManS "EXIT STATUS")
        (fmap (lambda (p) (match p [(Pair code doc) (ManI (show code) doc)])) codes)))

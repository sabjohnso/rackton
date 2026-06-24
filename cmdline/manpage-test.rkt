#lang rackton

;; Step 5 of RacktonCmdline.org: man-page rendering.
;;
;; Golden tests for the $(…) markup expansion and the Plain / Groff
;; renderers, plus the OPTIONS section built from ArgInfo.
;;
;; RED: the renderers, markup expansion, and section builders are
;; stubbed, so every golden check fails.

(require rackton/cmdline/manpage
         rackton/cmdline/arg
         "../unit.rkt")

(: sub Subst)
(define sub (Subst "tool" "tool"))

;; does hay contain needle?
(: contains? (-> String String Boolean))
(define (contains? needle hay)
  (let loop ([i 0])
    (cond
      [(> (+ i (string-length needle)) (string-length hay)) #f]
      [(string-prefix? needle (substring hay i (string-length hay))) #t]
      [else (loop (+ i 1))])))

;; a small page and the OPTIONS infos
(: page (List ManBlock))
(define page (list (ManS "NAME") (ManP "tool - does things")))

(: infos (List ArgInfo))
(define infos
  (list (info-docv "N" (info-doc "the count" (arg-info (list "c" "count"))))
        (info-doc "be loud" (arg-info (list "v" "verbose")))))

(: suite (List Test))
(define suite
  (list
   (it "markup: plain strips emphasis, substitutes names"
       (all-checks
        (list (check-equal? (expand-markup FmtPlain sub "$(b,hi) $(tname)") "hi tool")
              (check-equal? (expand-markup FmtPlain sub "$(i,x)") "x")
              (check-equal? (expand-markup FmtPlain sub "$(mname)!") "tool!")
              (check-equal? (expand-markup FmtPlain sub "plain text") "plain text"))))

   (it "markup: groff emits troff font escapes"
       (all-checks
        (list (check-equal? (expand-markup FmtGroff sub "$(b,hi)") "\\fBhi\\fR")
              (check-equal? (expand-markup FmtGroff sub "$(i,x)")  "\\fIx\\fR"))))

   (it "render-plain: heading then indented paragraph"
       (all-checks
        (list (check-equal? (render-plain page sub) "NAME\n  tool - does things\n"))))

   (it "render-groff: .SH heading then .P paragraph"
       (all-checks
        (list (check-equal? (render-groff page sub) ".SH NAME\n.P\ntool - does things\n"))))

   (it "options-section: names, docv, and docs render"
       (let ([txt (render-plain (options-section infos) sub)])
         (all-checks
          (list (check-true (contains? "OPTIONS" txt))
                (check-true (contains? "-c, --count=N" txt))
                (check-true (contains? "the count" txt))
                (check-true (contains? "-v, --verbose" txt))
                (check-true (contains? "be loud" txt))))))

   (it "name-section: NAME plus 'name - doc'"
       (let ([txt (render-plain (name-section "tool" "does things") sub)])
         (all-checks
          (list (check-true (contains? "NAME" txt))
                (check-true (contains? "tool - does things" txt))))))

   (it "exit-section: codes and their docs"
       (let ([txt (render-plain (exit-section (list (Pair 0 "success") (Pair 124 "cli error"))) sub)])
         (all-checks
          (list (check-true (contains? "EXIT STATUS" txt))
                (check-true (contains? "success" txt))
                (check-true (contains? "124" txt))))))))

(: main Unit)
(define main (run-io (run-suite "rackton/cmdline/manpage" suite)))

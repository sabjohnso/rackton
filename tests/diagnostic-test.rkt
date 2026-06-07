#lang racket/base

;; The diagnostic document algebra (private/diagnostic.rkt) — a small
;; Wadler/Leijen pretty-printer.  Errors are built as structured `doc`s
;; and laid out to a target width by ONE renderer, so width (and later,
;; colour/source context) is a property of rendering, not smeared across
;; every raise site.
;;
;; Combinators:
;;   doc-text doc-nil doc-line doc-softline
;;   doc-cat (variadic)  doc-nest  doc-group
;;   render-doc : doc width → string
;;
;; `doc-line` flattens to a space, `doc-softline` to nothing; both become
;; a newline (+ current indent) when a `doc-group` can't fit flat.

(require rackunit rackcheck
         "../private/diagnostic.rkt")

;; ----- unit: the layout algorithm -----------------------------------

(test-case "text and nil"
  (check-equal? (render-doc (doc-text "abc") 80) "abc")
  (check-equal? (render-doc doc-nil 80) ""))

(test-case "cat concatenates"
  (check-equal? (render-doc (doc-cat (doc-text "a") (doc-text "b") (doc-text "c")) 80)
                "abc"))

(test-case "a group that fits flattens line→space, softline→empty"
  (check-equal? (render-doc (doc-group (doc-cat (doc-text "a") doc-line (doc-text "b"))) 80)
                "a b")
  (check-equal? (render-doc (doc-group (doc-cat (doc-text "a") doc-softline (doc-text "b"))) 80)
                "ab"))

(test-case "a group that doesn't fit breaks at its lines"
  (check-equal? (render-doc (doc-group (doc-cat (doc-text "aaaa") doc-line (doc-text "bbbb"))) 5)
                "aaaa\nbbbb")
  (check-equal? (render-doc (doc-group (doc-cat (doc-text "aaaa") doc-softline (doc-text "bbbb"))) 5)
                "aaaa\nbbbb"))

(test-case "nest indents broken lines by n; flat lines are unaffected"
  ;; fits at width 5 → flat, no indent
  (check-equal? (render-doc (doc-group (doc-nest 2 (doc-cat (doc-text "a") doc-line (doc-text "b")))) 5)
                "a b")
  ;; width 2 forces a break → continuation indented 2
  (check-equal? (render-doc (doc-group (doc-nest 2 (doc-cat (doc-text "a") doc-line (doc-text "b")))) 2)
                "a\n  b"))

(test-case "the fit test looks ahead to the next break, not the whole doc"
  ;; "(a b" then a forced newline: the group fits because only the first
  ;; line needs to fit the width, even though more text follows.
  (define d (doc-cat (doc-group (doc-cat (doc-text "a") doc-line (doc-text "b")))
                     doc-line          ; break (outside any group → always breaks)
                     (doc-text "ccccccccccc")))
  (check-equal? (render-doc d 6) "a b\nccccccccccc"))

(test-case "nested groups break independently (inner fits, outer doesn't)"
  (define inner (doc-group (doc-cat (doc-text "x") doc-line (doc-text "y"))))
  (define outer (doc-group (doc-cat (doc-text "start") doc-line inner doc-line (doc-text "end"))))
  ;; width fits "x y" but not the whole thing → outer breaks, inner stays flat
  (check-equal? (render-doc outer 7) "start\nx y\nend"))

(test-case "labeled-block: one line when it fits, one-per-line when it doesn't"
  (check-equal? (render-doc (labeled-block "avail:" '("a" "b" "c")) 100)
                "avail: a b c")
  (check-equal? (render-doc (labeled-block "avail:" '("aaaa" "bbbb" "cccc")) 8)
                "avail:\n  aaaa\n  bbbb\n  cccc"))

;; ----- reference flattener (for the group/flatten law) --------------

;; The all-flat rendering of a doc: every line becomes its flat string,
;; nesting is irrelevant when nothing breaks.
(define (flat-string d)
  (cond
    [(doc-nil? d) ""]
    [(doc-text? d) (doc-text-string d)]
    [(doc-line? d) (doc-line-flat d)]
    [(doc-cat? d) (apply string-append (map flat-string (doc-cat-parts d)))]
    [(doc-nest? d) (flat-string (doc-nest-body d))]
    [(doc-group? d) (flat-string (doc-group-body d))]))

;; ----- property: algebra laws ---------------------------------------

(define gen-str
  (gen:map (gen:list (gen:one-of '(#\a #\b #\c #\d #\space))) list->string))

(define (gen-doc depth)
  (cond
    [(<= depth 0)
     (gen:choice (gen:const doc-nil)
                 (gen:const doc-line)
                 (gen:const doc-softline)
                 (gen:map gen-str doc-text))]
    [else
     (gen:choice
      (gen:const doc-nil)
      (gen:map gen-str doc-text)
      (gen:const doc-line)
      (gen:const doc-softline)
      (gen:map (gen:tuple (gen-doc (sub1 depth)) (gen-doc (sub1 depth)))
               (lambda (p) (doc-cat (car p) (cadr p))))
      (gen:map (gen:tuple (gen:integer-in 0 4) (gen-doc (sub1 depth)))
               (lambda (p) (doc-nest (car p) (cadr p))))
      (gen:map (gen-doc (sub1 depth)) doc-group))]))

(define gen-width (gen:integer-in 0 40))

(check-property
 (property ([d (gen-doc 3)] [w gen-width])
   ;; left and right identity for doc-cat
   (check-equal? (render-doc (doc-cat doc-nil d) w) (render-doc d w))
   (check-equal? (render-doc (doc-cat d doc-nil) w) (render-doc d w))))

(check-property
 (property ([a (gen-doc 2)] [b (gen-doc 2)] [c (gen-doc 2)] [w gen-width])
   ;; associativity of doc-cat (rendering-equivalence)
   (check-equal? (render-doc (doc-cat (doc-cat a b) c) w)
                 (render-doc (doc-cat a (doc-cat b c)) w))))

(check-property
 (property ([d (gen-doc 3)])
   ;; a group at unbounded width is the all-flat rendering
   (check-equal? (render-doc (doc-group d) 1000000) (flat-string d))))

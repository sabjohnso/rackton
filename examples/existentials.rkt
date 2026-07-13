#lang rackton

;; existentials.rkt — first-class existential types.
;;
;; An existential type `(Exists (a) ((Pretty a) => a))` reads "some
;; value whose type satisfies Pretty, with the type itself hidden."
;; Packing a value into it (the `ann` form) erases the witness type but
;; remembers that it satisfies `Pretty`; `open` later recovers the
;; value together with its `Pretty` dictionary.
;;
;; This is the classic use of existentials: a SINGLE homogeneous list
;; whose elements are values of MANY different types, processed
;; uniformly through a shared protocol.  Without existentials the list
;; could not be typed — `(Cons 7 (Cons #t …))` mixes Integer and
;; Boolean.  With them, every element is one type, `Described`, and the
;; differences are sealed behind the constraint.
;;
;;   racket examples/existentials.rkt

;; ----- a protocol the hidden types must satisfy ----------------------

(protocol (Pretty a)
          (: pretty (-> a String)))

(instance (Pretty Integer)
  (define (pretty n) (string-append "the integer " (show n))))

(instance (Pretty Boolean)
  (define (pretty b) (if b "a true flag" "a false flag")))

(data Point (Point2 Integer Integer))
(instance (Pretty Point)
  (define (pretty p)
    (match p
      [(Point2 x y)
       (string-append "the point ("
                      (string-append (show x)
                                     (string-append ", " (string-append (show y) ")"))))])))

;; ----- the existential element type, named once ----------------------

(define-alias Described (Exists (a) ((Pretty a) => a)))

;; A heterogeneous list: each element packs a value of a DIFFERENT type,
;; yet all four share the one element type `Described`.
(: things (List Described))
(define things
  (Cons (ann 7            Described)
        (Cons (ann #t           Described)
              (Cons (ann (Point2 3 4) Described)
                    (Cons (ann #f           Described) Nil)))))

;; `open` recovers each witness with its Pretty dictionary in scope, so
;; `pretty` resolves and dispatches on the (hidden) runtime value.  The
;; witness type `a` never escapes — only the String result leaves.
(: pretty-one (-> Described String))
(define (pretty-one d) (open d (a x) (pretty x)))

(: pretty-all (-> (List Described) (List String)))
(define (pretty-all xs)
  (match xs
    [(Nil)         Nil]
    [(Cons d rest) (Cons (pretty-one d) (pretty-all rest))]))

;; ----- render -------------------------------------------------------

(: bullet-lines (-> (List String) String))
(define (bullet-lines xs)
  (match xs
    [(Nil)         ""]
    [(Cons s rest) (string-append "  - "
                                  (string-append s
                                                 (string-append "\n" (bullet-lines rest))))]))

(: main (IO Unit))
(define main
  (let& ([_ (println "A heterogeneous list, described uniformly:")]
         [_ (println "")])
    (println (bullet-lines (pretty-all things)))))

#lang rackton

;; pretty-json.rkt — pretty-print JSON-like values with rackton/text/pretty.
;;
;; A value is turned into a `Doc`; the renderer then decides the layout
;; for a given width.  Arrays and objects collapse onto one line when
;; they fit and break one-entry-per-line (indented) when they do not —
;; all from a single `Doc`, just by changing the width.
;;
;; It also shows `fill-grid`: a column-aligned, page-width-adaptive list.
;;
;; Run it with:  racket examples/pretty-json.rkt

(require rackton/text/pretty)

;; ----- a small JSON value --------------------------------------------

(data Json
  JNull
  (JBool Boolean)
  (JNum  Integer)
  (JStr  String)
  (JArr  (List Json))
  (JObj  (List (Pair String Json))))

;; ----- Json -> Doc ---------------------------------------------------

(: quote-str (-> String String))
(define (quote-str s) (mappend "\"" (mappend s "\"")))

(: json->doc (-> Json Doc))
(define (json->doc j)
  (match j
    [(JNull)    (text "null")]
    [(JBool b)  (text (if b "true" "false"))]
    [(JNum n)   (text (show n))]
    [(JStr s)   (text (quote-str s))]
    [(JArr xs)  (encloseSep (text "[") (text "]") (text ",") (fmap json->doc xs))]
    [(JObj kvs) (encloseSep (text "{") (text "}") (text ",") (fmap field->doc kvs))]))

;; one object field:  "key": <value>
(: field->doc (-> (Pair String Json) Doc))
(define (field->doc kv)
  (match kv
    [(Pair k v) (<> (text (mappend (quote-str k) ": ")) (json->doc v))]))

;; ----- a sample document ---------------------------------------------

(: sample Json)
(define sample
  (JObj (list
          (Pair "name"     (JStr "rackton"))
          (Pair "version"  (JNum 1))
          (Pair "stable"   (JBool #t))
          (Pair "keywords" (JArr (list (JStr "typed") (JStr "functional") (JStr "racket"))))
          (Pair "meta"     (JObj (list (Pair "authors" (JArr (list (JStr "sbj"))))
                                       (Pair "license" (JStr "MIT"))))))))

(: idents (List Doc))
(define idents
  (fmap text (list "alpha" "beta" "gamma" "delta" "epsilon"
                   "zeta" "eta" "theta" "iota" "kappa")))

;; ----- driver --------------------------------------------------------

(: banner (-> String Integer (IO Unit)))
(define (banner label w)
  (println (mappend "--- " (mappend label (mappend " @ width " (mappend (show w) " ---"))))))

(: demo (-> String Integer Doc (IO Unit)))
(define (demo label w d)
  (do [_ <- (banner label w)] (println (pretty w d))))

(: main (IO Unit))
(define main (do [_ <- (demo "json" 60 (json->doc sample))]
               [_ <- (demo "json" 28 (json->doc sample))]
               [_ <- (println "")]
               [_ <- (demo "grid" 50 (<> (text "exports: ") (fill-grid idents)))]
               (demo "grid" 30 (<> (text "exports: ") (fill-grid idents)))))

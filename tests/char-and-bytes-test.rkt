#lang rackton

;; Char and Bytes primitives.

(require "../unit.rkt")

;; ----- Char literal + Eq/Ord/Show -----------------------
(: ch-a Char)
(define ch-a #\A)

(: ch-z Char)
(define ch-z #\z)

(: a=a Boolean)
(define a=a (== ch-a #\A))

(: a<z Boolean)
(define a<z (< ch-a ch-z))

(: show-A String)
(define show-A (show ch-a))

;; ----- char ↔ integer ---------------------------------
(: code-of-A Integer)
(define code-of-A (char->integer ch-a))

(: from-65 (Maybe Char))
(define from-65 (integer->char 65))

(: from-bad (Maybe Char))
(define from-bad (integer->char -1))

;; ----- char predicates / case ------------------------
(: up-a Char)
(define up-a (char-upcase #\a))

(: down-Z Char)
(define down-Z (char-downcase #\Z))

(: alpha? Boolean)
(define alpha? (char-alphabetic? #\m))

(: numeric? Boolean)
(define numeric? (char-numeric? #\7))

(: ws? Boolean)
(define ws? (char-whitespace? #\space))

(: not-alpha? Boolean)
(define not-alpha? (char-alphabetic? #\7))

;; ----- string ↔ chars --------------------------------
(: chars (List Char))
(define chars (string->chars "abc"))

(: rebuilt String)
(define rebuilt (chars->string chars))

(: ref-ok (Maybe Char))
(define ref-ok (string-ref "abc" 1))

(: ref-bad (Maybe Char))
(define ref-bad (string-ref "abc" 99))

(: ch-as-string String)
(define ch-as-string (char->string ch-a))

;; ----- Bytes literal + Eq + length + ref ------------
(: bs Bytes)
(define bs #"hello")

(: bs=bs Boolean)
(define bs=bs (== bs #"hello"))

(: bs-len Integer)
(define bs-len (bytes-length bs))

(: bs-ref-ok (Maybe Integer))
(define bs-ref-ok (bytes-ref bs 0))     ;; #\h = 104

(: bs-ref-bad (Maybe Integer))
(define bs-ref-bad (bytes-ref bs 99))

(: bs-append Bytes)
(define bs-append (bytes-append #"foo" #"-bar"))

;; ----- Bytes ↔ list ----------------------------------
(: lst (List Integer))
(define lst (bytes->list #"AB"))

(: built Bytes)
(define built (list->bytes (Cons 65 (Cons 66 Nil))))

;; ----- make-bytes ------------------------------------
(: filled Bytes)
(define filled (make-bytes 3 88))    ;; "XXX" as bytes

;; ----- String ↔ Bytes -------------------------------
(: encoded Bytes)
(define encoded (string->bytes "Aé"))

(: decoded (Maybe String))
(define decoded (bytes->string encoded))

(: decoded-bad (Maybe String))
(define decoded-bad (bytes->string #"\xff\xfe\xfd"))

;; ---------- assertions -----------------------------------

(: suite (List Test))
(define suite
  (list
   (it "Char Eq + Ord"
       (all-checks
        (list (check-true a=a)
              (check-true a<z))))
   (it "Char Show prints round-trippable literal"
       (check-equal? show-A "#\\A"))
   (it "char->integer codepoint"
       (check-equal? code-of-A 65))
   (it "integer->char success and failure"
       (all-checks
        (list (check-equal? from-65  (Some #\A))
              (check-equal? from-bad None))))
   (it "case conversion"
       (all-checks
        (list (check-equal? up-a    #\A)
              (check-equal? down-Z  #\z))))
   (it "char predicates"
       (all-checks
        (list (check-true alpha?)
              (check-true numeric?)
              (check-true ws?)
              (check-false not-alpha?))))
   (it "string ↔ chars round-trip"
       (all-checks
        (list (check-equal? chars (Cons #\a (Cons #\b (Cons #\c Nil))))
              (check-equal? rebuilt "abc"))))
   (it "string-ref Maybe path"
       (all-checks
        (list (check-equal? ref-ok  (Some #\b))
              (check-equal? ref-bad None))))
   (it "char->string"
       (check-equal? ch-as-string "A"))
   (it "Bytes Eq + length"
       (all-checks
        (list (check-true bs=bs)
              (check-equal? bs-len 5))))
   (it "bytes-ref Maybe path"
       (all-checks
        (list (check-equal? bs-ref-ok  (Some 104))
              (check-equal? bs-ref-bad None))))
   (it "bytes-append"
       (check-equal? bs-append #"foo-bar"))
   (it "bytes ↔ list round-trip"
       (all-checks
        (list (check-equal? lst   (Cons 65 (Cons 66 Nil)))
              (check-equal? built #"AB"))))
   (it "make-bytes fills the buffer"
       (check-equal? filled #"XXX"))
   (it "string ↔ bytes UTF-8 round-trip + invalid-byte path"
       (all-checks
        (list (check-equal? decoded     (Some "Aé"))
              (check-equal? decoded-bad None))))))

(: main Unit)
(define main (run-io (run-suite "char-and-bytes" suite)))

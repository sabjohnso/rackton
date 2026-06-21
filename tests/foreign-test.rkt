#lang rackton

;; rackton/foreign: import a host (Racket) binding that is NOT in
;; racket/base, give it a Rackton type, and call it.  This is the
;; sanctioned bridge that lets stdlib modules reach beyond racket/base
;; (Phase 8 — unblocks slimming the runtime-backed prelude content).

(require "../unit.rkt")

;; string-contains? lives in racket/string, not racket/base or the
;; Rackton prelude.  Declared 2-ary (curried type, n-ary call).
(foreign string-contains? (-> String (-> String Boolean)) #:from racket/string)

(: has-ell Boolean)
(define has-ell (string-contains? "hello" "ell"))

(: has-zee Boolean)
(define has-zee (string-contains? "hello" "zzz"))

;; #:as renames: bind the Rackton name `str-replace` to racket/string's
;; string-replace (3-ary; curried type, n-ary call).
(foreign str-replace (-> String (-> String (-> String String)))
         #:from racket/string #:as string-replace)

(: dashed String)
(define dashed (str-replace "a b c" " " "-"))

(: suite (List Test))
(define suite
  (list
   (it "foreign import from racket/string"
       (all-checks
        (list (check-true  has-ell)
              (check-false has-zee))))
   (it "foreign import with #:as rename"
       (check-equal? dashed "a-b-c"))))

(: main Unit)
(define main (run-io (run-suite "foreign" suite)))

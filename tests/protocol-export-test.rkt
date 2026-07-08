#lang rackton

;; A protocol defined in one module (protocol-export-proto) can be
;; instantiated in another.  Before the protocol-out dispatch-table
;; export, the instance below failed with `$dispatch:pretty: unbound
;; identifier`.

(require "protocol-export-proto.rkt"
         "../unit.rkt")

(data (Color a) Red Green (Custom a))

(instance (Pretty (Color a))
  (define (pretty c)
    (match c
      [(Red)      "red"]
      [(Green)    "green"]
      [(Custom _) "custom"])))

(: r String) (define r (pretty (ann Red (Color Integer))))
(: g String) (define g (pretty (ann Green (Color Integer))))
(: c String) (define c (pretty (Custom 7)))

(: suite (List Test))
(define suite
  (list
    (it "cross-module instance of a user protocol dispatches"
        (all-checks
          (list (check-equal? r "red")
                (check-equal? g "green")
                (check-equal? c "custom"))))))

(: test-main (IO Unit))
(define test-main (run-suite "cross-module protocol instance" suite))

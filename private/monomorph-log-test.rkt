#lang racket/base

;; Direct unit tests for the monomorphization bookkeeping interface in
;; "monomorph-log.rkt".  These exercise the record/snapshot helpers in
;; isolation — without running the whole pipeline — so the module can be
;; reasoned about and changed on its own.  (The inlining bookkeeping now lives
;; in codegen's cg-st, tested through the codegen/end-to-end suites.)

(module+ test
  (require rackunit
           "monomorph-log.rkt")

  (test-case "monomorphized sites accumulate newest-first"
    (parameterize ([current-monomorphized-sites (make-monomorph-log)])
      (record-monomorphized-site! '== '$==:Integer)
      (record-monomorphized-site! 'show '$show:Maybe)
      (check-equal? (monomorphized-sites-snapshot)
                    '((show . $show:Maybe) (== . $==:Integer)))))

  (test-case "record is a no-op outside an elaborate (unset param)"
    ;; The parameter defaults to #f; the helper must not raise.
    (check-not-exn (lambda () (record-monomorphized-site! 'm '$m:T))))

  (test-case "the snapshot is empty when the param is unset"
    (check-equal? (monomorphized-sites-snapshot) '())))

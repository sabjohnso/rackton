#lang racket/base

;; Direct unit tests for the monomorphization/inlining bookkeeping
;; interface in "monomorph-log.rkt".  These exercise the record/lookup/
;; snapshot helpers in isolation — without running the whole pipeline —
;; so the bookkeeping module can be reasoned about and changed on its
;; own.

(module+ test
  (require rackunit
           "monomorph-log.rkt")

  ;; ----- record-monomorphized-site! / snapshot ---------------------

  (test-case "monomorphized sites accumulate newest-first"
    (parameterize ([current-monomorphized-sites (make-monomorph-log)])
      (record-monomorphized-site! '== '$==:Integer)
      (record-monomorphized-site! 'show '$show:Maybe)
      (check-equal? (monomorphized-sites-snapshot)
                    '((show . $show:Maybe) (== . $==:Integer)))))

  (test-case "inlined sites accumulate independently of monomorphized sites"
    (parameterize ([current-monomorphized-sites (make-monomorph-log)]
                   [current-inlined-sites       (make-monomorph-log)])
      (record-monomorphized-site! '== '$==:Integer)
      (record-inlined-site! 'tag-of '$tag-of:Integer)
      (check-equal? (monomorphized-sites-snapshot)
                    '((== . $==:Integer)))
      (check-equal? (inlined-sites-snapshot)
                    '((tag-of . $tag-of:Integer)))))

  ;; ----- register-inlinable-body! / lookup-inlinable-body ----------

  (test-case "a registered body is recovered by lookup"
    (parameterize ([current-inlinable-bodies (make-inlinable-registry)])
      (register-inlinable-body! '$f:Integer 'a-body)
      (check-eq? (lookup-inlinable-body '$f:Integer) 'a-body)
      (check-false (lookup-inlinable-body '$g:Integer))))

  ;; ----- guards when the parameters are unset ----------------------

  (test-case "record/register are no-ops outside an elaborate (unset params)"
    ;; The parameters default to #f; the helpers must not raise.
    (check-not-exn (lambda () (record-monomorphized-site! 'm '$m:T)))
    (check-not-exn (lambda () (record-inlined-site! 'm '$m:T)))
    (check-not-exn (lambda () (register-inlinable-body! '$m:T 'body))))

  (test-case "lookup and snapshots are empty/false when params are unset"
    (check-false (lookup-inlinable-body '$m:T))
    (check-equal? (monomorphized-sites-snapshot) '())
    (check-equal? (inlined-sites-snapshot) '())))

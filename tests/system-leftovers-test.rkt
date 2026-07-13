#lang rackton

;; rackton/system — directory mutation (rename/copy/createIfMissing)
;; and clock queries (current time / CPU time).

(require rackton/system
         "../unit.rkt")

;; createDirectoryIfMissing is idempotent
(: mkdir-res (IO Boolean))
(define mkdir-res
  (let& ([_ (create-directory-if-missing "/tmp/rackton-leftover-dir")]
         [_ (create-directory-if-missing "/tmp/rackton-leftover-dir")]
         [b (does-directory-exist? "/tmp/rackton-leftover-dir")])
    (pure b)))

;; renameFile: old gone, new has the content
(: rename-res (IO Boolean))
(define rename-res
  (let& ([_   (write-file "/tmp/rackton-ren-a.txt" "data")]
         [_   (rename-file "/tmp/rackton-ren-a.txt" "/tmp/rackton-ren-b.txt")]
         [old (file-exists? "/tmp/rackton-ren-a.txt")]
         [s   (read-file "/tmp/rackton-ren-b.txt")])
    (pure (if old #f (== s "data")))))

;; copyFile: source remains, copy has the content
(: copy-res (IO Boolean))
(define copy-res
  (let& ([_   (write-file "/tmp/rackton-cp-src.txt" "copyme")]
         [_   (copy-file "/tmp/rackton-cp-src.txt" "/tmp/rackton-cp-dst.txt")]
         [src (file-exists? "/tmp/rackton-cp-src.txt")]
         [d   (read-file "/tmp/rackton-cp-dst.txt")])
    (pure (if src (== d "copyme") #f))))

;; clocks
(: now-pos    (IO Boolean)) (define now-pos    (let& ([t get-current-time-millis]) (pure (> t 1000000000000))))
(: cpu-nonneg (IO Boolean)) (define cpu-nonneg (let& ([t get-cpu-time-millis])     (pure (>= t 0))))

(: r-mkdir Boolean) (define r-mkdir (run-io mkdir-res))
(: r-rename Boolean) (define r-rename (run-io rename-res))
(: r-copy Boolean) (define r-copy (run-io copy-res))
(: r-now Boolean) (define r-now (run-io now-pos))
(: r-cpu Boolean) (define r-cpu (run-io cpu-nonneg))

(: suite (List Test))
(define suite
  (list
    (it "createDirectoryIfMissing" (check-true r-mkdir))
    (it "renameFile"               (check-true r-rename))
    (it "copyFile"                 (check-true r-copy))
    (it "getCurrentTime (ms)"      (check-true r-now))
    (it "getCPUTime (ms)"          (check-true r-cpu))))

(: test-main (IO Unit))
(define test-main (run-suite "rackton/system leftovers" suite))

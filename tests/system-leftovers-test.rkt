#lang racket/base

;; rackton/system — directory mutation (rename/copy/createIfMissing)
;; and clock queries (current time / CPU time).

(require rackunit
         "../main.rkt")

(rackton
  (require rackton/system)

  ;; createDirectoryIfMissing is idempotent
  (: mkdir-res (IO Boolean))
  (define mkdir-res
    (do [_ <- (create-directory-if-missing "/tmp/rackton-leftover-dir")]
        [_ <- (create-directory-if-missing "/tmp/rackton-leftover-dir")]
        [b <- (does-directory-exist? "/tmp/rackton-leftover-dir")]
        (pure b)))

  ;; renameFile: old gone, new has the content
  (: rename-res (IO Boolean))
  (define rename-res
    (do [_   <- (write-file "/tmp/rackton-ren-a.txt" "data")]
        [_   <- (rename-file "/tmp/rackton-ren-a.txt" "/tmp/rackton-ren-b.txt")]
        [old <- (file-exists? "/tmp/rackton-ren-a.txt")]
        [s   <- (read-file "/tmp/rackton-ren-b.txt")]
        (pure (if old #f (== s "data")))))

  ;; copyFile: source remains, copy has the content
  (: copy-res (IO Boolean))
  (define copy-res
    (do [_   <- (write-file "/tmp/rackton-cp-src.txt" "copyme")]
        [_   <- (copy-file "/tmp/rackton-cp-src.txt" "/tmp/rackton-cp-dst.txt")]
        [src <- (file-exists? "/tmp/rackton-cp-src.txt")]
        [d   <- (read-file "/tmp/rackton-cp-dst.txt")]
        (pure (if src (== d "copyme") #f))))

  ;; clocks
  (: now-pos    (IO Boolean)) (define now-pos    (do [t <- get-current-time-millis] (pure (> t 1000000000000))))
  (: cpu-nonneg (IO Boolean)) (define cpu-nonneg (do [t <- get-cpu-time-millis]     (pure (>= t 0)))))

;; ---------- assertions ---------------------------------------

(test-case "createDirectoryIfMissing" (check-true (run-io mkdir-res)))
(test-case "renameFile"               (check-true (run-io rename-res)))
(test-case "copyFile"                 (check-true (run-io copy-res)))
(test-case "getCurrentTime (ms)"      (check-true (run-io now-pos)))
(test-case "getCPUTime (ms)"          (check-true (run-io cpu-nonneg)))

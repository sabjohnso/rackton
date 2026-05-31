#lang racket/base

;; rackton/system — additional System.* operations: appendFile,
;; doesDirectoryExist, getCurrentDirectory, getProgName, setEnv,
;; withFile (exception-safe bracket).

(require rackunit
         "../main.rkt")

(rackton
  (require rackton/system)

  ;; appendFile: write then append
  (: append-res (IO String))
  (define append-res
    (do [_ <- (write-file "/tmp/rackton-add.txt" "a")]
        [_ <- (append-file "/tmp/rackton-add.txt" "b")]
        [s <- (read-file "/tmp/rackton-add.txt")]
        (pure s)))

  ;; doesDirectoryExist
  (: dir-yes (IO Boolean)) (define dir-yes (does-directory-exist? "/tmp"))
  (: dir-no  (IO Boolean)) (define dir-no  (does-directory-exist? "/tmp/rackton-nonexistent-xyzzy"))

  ;; getCurrentDirectory / getProgName are non-empty
  (: cwd-ne  (IO Boolean)) (define cwd-ne  (do [d <- get-current-directory] (pure (not (== d "")))))
  (: prog-ne (IO Boolean)) (define prog-ne (do [p <- get-prog-name]         (pure (not (== p "")))))

  ;; setEnv then getenv round-trips
  (: env-rt (IO Boolean))
  (define env-rt
    (do [_ <- (set-env "RACKTON_TEST_VAR_QQ" "hello")]
        [m <- (getenv "RACKTON_TEST_VAR_QQ")]
        (pure (match m [(Some s) (== s "hello")] [(None) #f]))))

  ;; withFile: write through a handle, then read it back
  (: wf (IO String))
  (define wf
    (do [_ <- (with-file "/tmp/rackton-wf.txt" WriteMode (lambda (h) (h-put-str h "wrote")))]
        [s <- (with-file "/tmp/rackton-wf.txt" ReadMode  (lambda (h) (h-get-contents h)))]
        (pure s))))

;; ---------- assertions ---------------------------------------

(test-case "appendFile"            (check-equal? (run-io append-res) "ab"))
(test-case "doesDirectoryExist"    (check-true (run-io dir-yes)) (check-false (run-io dir-no)))
(test-case "getCurrentDirectory"   (check-true (run-io cwd-ne)))
(test-case "getProgName"           (check-true (run-io prog-ne)))
(test-case "setEnv"                (check-true (run-io env-rt)))
(test-case "withFile round-trip"   (check-equal? (run-io wf) "wrote"))

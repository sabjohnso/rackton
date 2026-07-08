#lang rackton

;; rackton/system/io — System.IO: handles, open-file with IOMode,
;; h-put-str / h-get-contents / h-get-line, std handles.

(require rackton/system/io
         rackton/system/file
         "../unit.rkt")

;; write then read back the whole file
(: roundtrip (IO String))
(define roundtrip
  (let& ([h  (open-file "/tmp/rackton-io-test.txt" WriteMode)]
         [_  (h-put-str h "hello ")]
         [_  (h-put-str-ln h "world")]
         [_  (h-close h)]
         [h2 (open-file "/tmp/rackton-io-test.txt" ReadMode)]
         [s  (h-get-contents h2)]
         [_  (h-close h2)])
    (pure-io s)))

;; append mode adds to existing content
(: appended (IO String))
(define appended
  (let& ([_  (write-file "/tmp/rackton-io-test3.txt" "a")]
         [h  (open-file "/tmp/rackton-io-test3.txt" AppendMode)]
         [_  (h-put-str h "b")]
         [_  (h-close h)]
         [s  (read-file "/tmp/rackton-io-test3.txt")])
    (pure-io s)))

;; h-get-line: first line is (Some "one"), then EOF is None
(: line-eof (IO Boolean))
(define line-eof
  (let& ([h  (open-file "/tmp/rackton-io-test2.txt" WriteMode)]
         [_  (h-put-str-ln h "one")]
         [_  (h-close h)]
         [h2 (open-file "/tmp/rackton-io-test2.txt" ReadMode)]
         [l1 (h-get-line h2)]
         [l2 (h-get-line h2)]
         [_  (h-close h2)])
    (pure-io (match l1
               [(Some s) (match l2 [(None) (== s "one")] [(Some _) #f])]
               [(None)   #f]))))

(: suite (List Test))
(define suite
  (list
    (it "open-file / h-put-str / h-get-contents round-trip"
        (check-equal? (run-io roundtrip) "hello world\n"))
    (it "AppendMode"
        (check-equal? (run-io appended) "ab"))
    (it "h-get-line yields Some then None at EOF"
        (check-true (run-io line-eof)))))

(: test-main (IO Unit))
(define test-main (run-suite "system/io" suite))

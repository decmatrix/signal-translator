(uiop:define-package :signal/tests/lexer
    (:use :cl
          :signal/tests/utils)
  (:nicknames :signal/lexer-tests
              :sig-lexer-tests)
  (:import-from :signal/core/all
                #:upload-result-to-file
                #:lexer)
  (:export #:test-suite))

(in-package :signal/tests/lexer)

(defun test-suite ()
  (format t "Run lexer tests ~%")
  (run-true-tests)
  (run-false-tests))

(defun run-true-tests ()
  (run-test
   "true lexer test 1"
   "example-files/for-lexer/true-files/test1-true.sig"
   "example-files/for-lexer/true-files/test1-true-result.txt")
  (run-test
   "true lexer test 2"
   "example-files/for-lexer/true-files/test2-true.sig"
   "example-files/for-lexer/true-files/test2-true-result.txt")
  (run-test
   "true lexer test 3"
   "example-files/for-lexer/true-files/test3-true.sig"
   "example-files/for-lexer/true-files/test3-true-result.txt")
  (format t "~%~%~%"))

(defun run-false-tests ()
  (run-test
   "false lexer test 1"
   "example-files/for-lexer/false-files/test1-false.sig"
   "example-files/for-lexer/false-files/test1-false-result.txt")
  (run-test
   "false lexer test 2"
   "example-files/for-lexer/false-files/test2-false.sig"
   "example-files/for-lexer/false-files/test2-false-result.txt")
  (run-test
   "false lexer test 3"
   "example-files/for-lexer/false-files/test3-false.sig"
   "example-files/for-lexer/false-files/test3-false-result.txt" )
  (format t "~%~%~%"))


(defun process-file (file output-file)
  (upload-result-to-file (lexer (get-path-to-file file)
                                :with-errors t)
                         :long-out t
                         :path-to-file (get-path-to-file output-file)))

(defun run-test (name file output-file)
  (format t "Run ~S test, file: ~S~%" name file)
  (process-file file output-file)
  (format t "~%~%"))

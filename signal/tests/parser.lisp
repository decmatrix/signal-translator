(uiop:define-package :signal/tests/parser
    (:use :cl
          :signal/tests/utils)
  (:nicknames :signal/parser-tests
              :sig-parser-tests)
  (:import-from :signal/core/all
                #:parser
                #:lexer)
  (:import-from :signal/core/parser/parser-front
                #:upload-result-to-file)
  (:export #:test-suite))

(in-package :signal/tests/parser)

(defun test-suite ()
  (format t "Run parser tests ~%")
  (run-true-tests)
  (run-false-tests))

(defun run-true-tests ()
  (run-test
   "true parser test 1"
   "example-files/for-parser/true-files/test1-true.sig"
   "example-files/for-parser/true-files/test1-true-result.sigparser")
  (run-test
   "true parser test 2"
   "example-files/for-parser/true-files/test2-true.sig"
   "example-files/for-parser/true-files/test2-true-result.sigparser")
  (run-test
   "true parser test 3"
   "example-files/for-parser/true-files/test3-true.sig"
   "example-files/for-parser/true-files/test3-true-result.sigparser")
  (format t "~%~%~%"))

(defun run-false-tests ()
  (run-test
   "false parser test 1"
   "example-files/for-parser/false-files/test1-false.sig"
   "example-files/for-parser/false-files/test1-false-result.sigparser")
  (run-test
   "false parser test 2"
   "example-files/for-parser/false-files/test2-false.sig"
   "example-files/for-parser/false-files/test2-false-result.sigparser")
  (run-test
   "false parser test 3"
   "example-files/for-parser/false-files/test3-false.sig"
   "example-files/for-parser/false-files/test3-false-result.sigparser")
  (run-test
   "false parser test 4"
   "example-files/for-parser/false-files/test4-false.sig"
   "example-files/for-parser/false-files/test4-false-result.sigparser")
  (run-test
   "false parser test 5"
   "example-files/for-parser/false-files/test5-false.sig"
   "example-files/for-parser/false-files/test5-false-result.sigparser")
  (run-test
   "false parser test 6"
   "example-files/for-parser/false-files/test6-false.sig"
   "example-files/for-parser/false-files/test6-false-result.sigparser")
  (run-test
   "false parser test 7"
   "example-files/for-parser/false-files/test7-false.sig"
   "example-files/for-parser/false-files/test7-false-result.sigparser")
  (format t "~%~%~%"))


(defun process-file (file output-file)
  (upload-result-to-file (parser (lexer (get-path-to-file file)
                                        :with-errors t)
                                 :with-errors t)
                         :path-to-file (get-path-to-file output-file)))

(defun run-test (name file output-file)
  (format t "Run ~S test, file: ~S~%" name file)
  (process-file file output-file)
  (format t "~%~%"))


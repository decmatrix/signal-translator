(uiop:define-package :signal/tests/code-gen
    (:use :cl
          :signal/tests/utils)
  (:nicknames :signal/code-gen-tests
              :sig-code-gen-tests)
  (:import-from :signal/core/all
                #:parser
                #:lexer
                #:generate-output-code)
  (:import-from :signal/core/code-gen/code-gen-front
                #:upload-result-to-file)
  (:export #:test-suite))

(in-package :signal/tests/code-gen)

(defun test-suite ()
  (format t "Run code gen tests ~%")
  (run-true-tests)
  (run-false-tests))

(defun run-true-tests ()
  (run-test
   "true code gen test 1"
   "example-files/for-code-gen/true-files/test1-true.sig"
   "example-files/for-code-gen/true-files/test1-true-result.asm")
  (run-test
   "true code gen test 2"
   "example-files/for-code-gen/true-files/test2-true.sig"
   "example-files/for-code-gen/true-files/test2-true-result.asm")
  (run-test
   "true code gen test 3"
   "example-files/for-code-gen/true-files/test3-true.sig"
   "example-files/for-code-gen/true-files/test3-true-result.asm")
  (format t "~%~%~%"))

(defun run-false-tests ()
  (run-test
   "false code gen test 1"
   "example-files/for-code-gen/false-files/test1-false.sig"
   "example-files/for-code-gen/false-files/test1-false-result.asm")
  (run-test
   "false code gen 2"
   "example-files/for-code-gen/false-files/test2-false.sig"
   "example-files/for-code-gen/false-files/test2-false-result.asm")
  (run-test
   "false code gen 3"
   "example-files/for-code-gen/false-files/test3-false.sig"
   "example-files/for-code-gen/false-files/test3-false-result.asm")
  (run-test
   "false code gen test 4"
   "example-files/for-code-gen/false-files/test4-false.sig"
   "example-files/for-code-gen/false-files/test4-false-result.asm")
  (run-test
   "false code gen test 5"
   "example-files/for-code-gen/false-files/test5-false.sig"
   "example-files/for-code-gen/false-files/test5-false-result.asm")
  (run-test
   "false code gen test 6"
   "example-files/for-code-gen/false-files/test6-false.sig"
   "example-files/for-code-gen/false-files/test6-false-result.asm")
  (format t "~%~%~%"))

(defun process-file (file output-file)
  (upload-result-to-file (generate-output-code
                          (parser (lexer (get-path-to-file file)
                                         :with-errors t)
                                  :with-errors t)
                          :with-errors t)
                         :path-to-file (get-path-to-file output-file)))

(defun run-test (name file output-file)
  (format t "Run ~S test, file: ~S~%" name file)
  (process-file file output-file)
  (format t "~%~%"))

(uiop:define-package :signal/tests/lexer
    (:use :cl)
  (:nicknames :signal/lexer-tests
              :sig-lexer-tests)
  (:import-from :signal/core/all
                #:format-out-tokens
                #:lexer)
  (:export #:test-suite))

(in-package :signal/tests/lexer)

(defun test-suite ()
  (format t "Run lexer tests ~%")
  (run-true-tests)
  (run-false-tests))

(defun run-true-tests ()
  (run-test
   "true test 1"
   "~/dev/lisp/signal-translator/signal/example-files/true-files/test1-true.sig")
  (run-test
   "true test 2"
   "~/dev/lisp/signal-translator/signal/example-files/true-files/test2-true.sig")
  (run-test
   "true test 3"
   "~/dev/lisp/signal-translator/signal/example-files/true-files/test3-true.sig")
  (format t "~%~%~%"))

(defun run-false-tests ()
  (run-test
   "false test 1"
   "~/dev/lisp/signal-translator/signal/example-files/false-files/test1-false.sig")
  (run-test
   "false test 2"
   "~/dev/lisp/signal-translator/signal/example-files/false-files/test2-false.sig")
  (run-test
   "false test 3"
   "~/dev/lisp/signal-translator/signal/example-files/false-files/test3-false.sig")
  (format t "~%~%~%"))



(defun process-file (file)
  (format-out-tokens (lexer file :with-errors t)))

(defun run-test (name file)
  (format t "Run ~S test, file: ~S~%" name file)
  (process-file file)
  (format t "~%~%"))

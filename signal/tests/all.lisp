(uiop:define-package :signal/tests/all
    (:use :cl
          :signal/tests/utils
          :signal/tests/lexer
          :signal/tests/parser
          :signal/tests/code-gen)
  (:nicknames :signal/tests
              :sig-tests)
  (:shadow #:test-suite)
  (:export #:test-suite))

(in-package :signal/tests/all)

(defun test-suite ()
  (signal/tests/lexer:test-suite)
  (signal/tests/parser:test-suite)
  (signal/tests/code-gen:test-suite))

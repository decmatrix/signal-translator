(uiop:define-package :signal/tests/all
    (:use :cl
          :signal/tests/lexer
          :signal/tests/parser
          ;:signal/tests/codegen
          )
  (:nicknames :signal/tests
              :sig-tests)
  (:shadow)
  (:export #:test-suite))

(in-package :signal/tests/all)

(defun test-suite ()
  (signal/tests/lexer:test-suite))



(uiop:define-package :signal/tests/all
    (:use :cl
          :lisp-unit
          :signal/tests/lexer
          :signal/tests/parser
          :signal/tests/codegen)
  (:shadow)
  (:export #:test-suite))

(in-package :signal/tests/all)

(defun test-suite ()
  (print 1))



(uiop:define-package :signal/tests/utils
    (:use :cl)
  (:nicknames :sig-tests-utils)
  (:export #:get-path-to-file))

(in-package :signal/tests/utils)

(defun get-path-to-file (path-to-file)
  "Get path to file.
--------------------------------------------
Args:
 `file' - file with porgram, with extension `.sig'."
  (concatenate 'string
               (multiple-value-bind (buff1 buff2 path)
                   (asdf:locate-system :signal)
                 (declare (ignore buff1 buff2))
                 (string-trim "signal.asd" (namestring path)))
               path-to-file))


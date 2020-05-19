(uiop:define-package :signal/core/utils/error-utils
    (:use :cl)
  (:nicknames :signal/error-utils
              :sig-error-utils)
  (:export #:*error*
           #:*error-type*
           #:catch-error
           #:throw-error))

(in-package :signal/core/utils/error-utils)

(defparameter *error* nil
  "Uses in `catch-error' macros, stores the current caught error.")
(defparameter *error-type* nil
  "Uses in `catch-error' function, stores type of error.")

(defmacro catch-error ((error-type) &body body)
  "Overrides a variable `*error*' to catch a syntax error.
----------------------------------
&body `body' - body."
  `(let ((*error*)
         (*error-type* ,error-type))
     ,@body))

(defun throw-error (reason)
   "Will capture a caught error into a `*error*' variable and
the value of the `*error*' variable will be the generated error message.
-------------------------------------
Args:
 &key `reason' - reason of error occurred."
   (setf *error* (format nil "~A: ~A" *error-type* reason)))

(uiop:define-package :signal/core/utils/all
    (:use :cl
          :signal/core/utils/error-utils)
  (:export
   ;; error-utils
   #:*error*
   #:*error-type*
   #:catch-error
   #:hand-error))

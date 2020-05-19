(uiop:define-package :signal/core/code-gen/all
    (:use :cl
          :signal/core/code-gen/code-gen-back
          :signal/core/code-gen/code-gen-front)
  (:export #:generate-output-code
           #:format-output-code
           #:res-code-gen-get-error))

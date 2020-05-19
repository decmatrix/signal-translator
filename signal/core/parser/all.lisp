(uiop:define-package :signal/core/parser/all
    (:use :cl
          :signal/core/parser/parser-back
          :signal/core/parser/parser-front)
  (:export #:parser
           #:format-out-tree
           #:res-parser-get-error))

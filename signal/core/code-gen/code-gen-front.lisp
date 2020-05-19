(uiop:define-package :signal/core/code-gen/code-gen-front
    (:use :cl
          :anaphora)
  (:import-from :signal/core/code-gen/code-gen-back
                #:res-code-gen
                #:res-code-gen-get-output-code
                #:res-code-gen-get-error)
  (:nicknames :signal/code-gen-front
              :sig-code-gen-front)
  (:export #:format-out-output-code
           #:upload-result-to-file))

(in-package :signal/core/code-gen/code-gen-front)

(defun format-out-ouput-code (result-code-gen &key (stream t))
  "Out output code of code generator.
--------------------------------------------------------------
Args:
 `result-code-gen' - instance of `res-code-gen' class, output code + error info,
  &key `stream' -  stream output information."
  (format stream "~A" (res-code-gen-get-output-code result-code-gen))
  (aif (res-code-gen-get-error result-code-gen)
       (format stream "~%~%;;~A" it)
       (format stream "~%~%;;OK. No errors")))

(defmethod upload-result-to-file ((result res-code-gen) &key (path-to-file (concatenate
                                                                            'string
                                                                            (string (gensym "output"))
                                                                            ".asm")))
  (with-open-file (stream path-to-file
                          :direction :output
                          :if-does-not-exist :create
                          :if-exists :overwrite)
    (format-out-ouput-code result
                           :stream stream)))

(uiop:define-package :signal/core/lexer/lexer-front
    (:use :cl
          :anaphora
          :alexandria
          :signal/core/lexer/lexer-back)
  (:nicknames :signal/lexer-front
              :sig-lexer-front)
  (:export #:format-out-tokens))

(in-package :signal/core/lexer/lexer-front)

(defun format-out-tokens (result-lexer &key (stream t) long-out)
  "output information about every token from
list - result of lexer
---------------------------------------------
Args:
  `result-lexer' - instance of `res-lexer' class, list of tokens + errors (result of lexer)
  &key `stream' - stream output information
  &key `long-out' - output full-text information"
  (format stream "TOKENS: ~%")
  (mapc (lambda (token)
          (format stream (if long-out
                             "Line: ~S, column: ~S, type: ~S, val: ~S~%~%"
                             "~S ~S ~S ~S~%")
                  (token-y-pos token)
                  (token-x-pos token)
                  (token-type token)
                  (token-val token)))
        (res-lexer-get-tokens result-lexer))
  (aif (res-lexer-get-errors result-lexer)
       (mapc (lambda (elm)
               (format stream "~%~S" elm))
             it)
       (format stream "~%OK. No errors"))
  (format stream "~%~%~%"))

(defmethod upload-result-to-file ((result res-lexer) &key (path-to-file (concatenate
                                                                         'string
                                                                         (string (gensym "signal-lexer-res-gen"))
                                                                         ".txt")) ;; TODO: change to siglexer
                                                       long-out)
  (with-open-file (stream path-to-file
                          :direction :output
                          :if-does-not-exist :create
                          :if-exists :overwrite)
    (format-out-tokens result
                       :stream stream
                       :long-out long-out)))

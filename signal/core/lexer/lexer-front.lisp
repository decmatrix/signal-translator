(uiop:define-package :signal/core/lexer/lexer-front
    (:use :cl
          :anaphora
          :alexandria
          :signal/core/lexer/lexer-back)
  (:nicknames :signal/lexer-front
              :sig-lexer-front)
  (:export #:format-out-tokens))

(in-package :signal/core/lexer/lexer-front)

(defun format-out-tokens (tokens &key (stream t) long-out)
  "output information about every token from
list - result of lexer
---------------------------------------------
Args:
  `tokens' - list of tokens (result of lexer)
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
        (second tokens))
  (aif (third tokens)
       (mapc (lambda (elm)
               (format stream "~%~S" elm))
             it)
       (format stream "~%OK. No errors"))
  (format stream "~%~%IDENTIFERS: ")
  (maphash-keys (lambda (id)
                  (format stream "~S " id))
                (first tokens))
  (format stream "~%~%~%"))

(uiop:define-package :signal/core/parser/utils
    (:use :cl
          :alexandria
          :signal/core/lexer/lexer-back)
  (:nicknames :sig-parser-utils)
  (:export #:get-type-of-value
           #:value-is-type-p
           #:delimeter-symbol-p
           #:delimeter-to-str
           #:keyword-symbol-p))

(in-package :signal/core/parser/utils)

(defun get-type-of-value (symbol)
  (or (and (delimeter-symbol-p symbol) 'DELIMETER)
      (and (keyword-symbol-p symbol) 'KEY-WORD)
      (and (eq symbol 'UNSIGNED-INTEGER) 'UNSIGNED-INTEGER)
      (and (eq symbol 'IDENTIFIER) 'IDENTIFIER)))

(defun value-is-type-p (symbol)
  (some (alexandria:curry #'eq symbol)
        '(KEY-WORD UNSIGNED-INTEGER DELIMETER IDENTIFIER)))

(defun delimeter-to-str (delimeter)
  (case delimeter
    (COMMA ",")
    (SEMICOLON ";")
    (COLON ":")
    (DOT ".")))

(defun delimeter-symbol-p (symbol)
  (some (alexandria:curry #'eq symbol)
        '(SEMICOLON COMMA COLON DOT)))

(defun keyword-symbol-p (symbol)
  (some (alexandria:curry #'eq symbol)
        '(PROGRAM BEGIN END LABEL GOTO LINK IN OUT)))

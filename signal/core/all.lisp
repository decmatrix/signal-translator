(uiop:define-package :signal/core/all
    (:use :cl
          :signal/core/lexer/all
          :signal/core/parser/all
          :signal/core/code-gen/all)
  (:nicknames :signal/core
              :sig-core)
  (:export #:format-out-tokens
           #:format-out-tree
           #:format-out-output-code
           #:upload-result-to-file
           #:lexer
           #:parser
           #:generate-output-code
           #:translate-code))

(in-package :signal/core/all)

(defgeneric uploa-result-to-file (result &key path-to-file)
  (:documentation
   "Upload result of lexer to file."))

(defun translate-code (path-to-input-file &key (path-to-output-file "out.asm"))
  "Translate input SIGNAL code to assembly language code:
[input-code.sig] -> Lexer -> Parser -> Code generator -> [output-code.asm]
--------------------------
Args:
 `path-to-input-file' - path to input .sig file,
 &key `out-file' - path to output file .asm."
  (signal/core/code-gen/code-gen-front::upload-result-to-file
   (generate-output-code (parser (lexer path-to-input-file)))
   :path-to-file path-to-output-file)
  t)

(uiop:define-package :signal/core/parser/parser-front
    (:use :cl)
  (:import-from :anaphora
                #:aif
                #:it)
  (:import-from :signal/core/parser/parser-back
                #:res-parser
                #:res-parser-get-tree
                #:res-parser-get-error)
  (:import-from :signal/core/lexer/lexer-back)
  (:nicknames :signal/parser-front
              :sig-parser-front)
  (:export #:format-out-tree))

(in-package :signal/core/parser/parser-front)

(defun format-out-tree (result-parser &key (stream t))
  "Out tree of parser.
------------------------------------------------------
Args:
 `result-parser' - instance of `res-parser' class, tree + error info,
 &key `stream' -  stream output information."
  (labels ((%out-tree (tree &key (indentation 1) is-main-root)
             (when tree
               (if (listp tree)
                   (let ((tail-sub-tree (cdr tree)))
                     (format-tree-node stream
                                       (if is-main-root
                                           ""
                                           (create-indentation-str indentation))
                                       (car tree))
                     (if tail-sub-tree
                         (mapc (lambda (sub-tree)
                                 (%out-tree sub-tree
                                            :indentation (if is-main-root
                                                             indentation
                                                             (1+ indentation))))
                               tail-sub-tree)
                         (format-tree-node stream
                                           (create-indentation-str (if is-main-root
                                                                       indentation
                                                                       (1+ indentation)))
                                           '<empty>)))
                   (format-tree-node stream (create-indentation-str indentation) tree)))))
    (%out-tree (res-parser-get-tree result-parser)
               :is-main-root t)
    (aif (res-parser-get-error result-parser)
         (format stream "~%~%~A~%" it)
         (format stream "~%~%OK. No errors"))
    (format stream "~%~%~%~%")))

(defmethod upload-result-to-file ((result res-parser) &key (path-to-file (concatenate
                                                                          'string
                                                                          (string (gensym "signal-parser-res-gen"))
                                                                          ".sigparser")))
  (with-open-file (stream path-to-file
                          :direction :output
                          :if-does-not-exist :create
                          :if-exists :overwrite)
    (format-out-tree result
                     :stream stream)))


(defun format-tree-node (stream indentation node)
  (format stream "~A~A~%" indentation node))

(defun create-indentation-str (len)
  (unless (zerop len)
    (concatenate 'string "..." (create-indentation-str (- len 1)))))


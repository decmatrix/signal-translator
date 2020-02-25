(uiop:define-package :signal/core/lexer
    (:use :cl
          :alexandria
          :anaphora)
  (:nicknames :signal/lexer
              :sig-lexer)
  (:export #:lexer
           ;; symbols
           #:PROGRAM
           #:BEGIN
           #:END
           #:LABEL
           #:GOTO
           #:LINK
           #:IN
           #:OUT))

(in-package :signal/core/lexer)

(defstruct token
  type
  val
  y-pos
  x-pos)

(defun lexer (input-file &key to-string)
  "generate tokens from file `.sig' and return
list of this tokens
----------------------------------------------
Args:
  `input-file' - input file with programs `.sig'
  &key `to-string' - output list of tokens"
  (let ((x 0)
        (y 1)
        res buff
        (id-table (make-hash-table :test #'eq)))
    (labels ((%analysis ()
               (with-open-file (stream input-file)
                 (do ((ch (read-char stream) (read-char stream nil 'eof)))
                     ((eq ch 'eof))
                   (cond
                     ((and (eq ch #\()
                           (eq (peek-char nil stream) #\*))
                      (aif (%read-comment stream)
                           (setq res (append res (list it)))))
                     ((delimeter-p ch)
                      (let ((lst (delimeter-p ch)))
                        (setq res (append res
                                          (list (make-token
                                                 :type (first lst)
                                                 :val (second lst)
                                                 :y-pos y
                                                 :x-pos x))))))
                     ((alpha-char-p ch)
                      (setq buff (list ch))
                      (setq res (append res
                                        (list (%read-identifier stream id-table)))))
                     ((digit-char-p ch)
                      (setq buff (list ch))
                      (setq res (append res
                                        (list (%read-digits-string stream)))))
                     ((not (white-char-p ch))
                      (setq res
                            (append res
                                    (list 'LEX-ERROR
                                          (format nil "Unknown character: ~@C" ch))))))
                   (if (eq ch #\Newline)
                       (progn
                         (setq x 0)
                         (incf y))
                       (incf x)))))
             (%read-identifier (stream id-table)
               (let ((x-fix x)
                     (y-fix y))
                 (do ((ch (read-char stream) (read-char stream nil 'eof)))
                     ((or (eq ch 'eof)
                          (not (alphanumericp ch)))
                      (unread-char ch stream))
                   (incf x)
                   (setq buff (nconc buff (list ch))))  ;; TODO: rewrite
                 (setq buff (coerce (mapcar #'char-upcase buff) 'string))
                 (aif (keyword-p buff)
                      (make-token
                       :type 'KEY-WORD
                       :val it
                       :y-pos y-fix
                       :x-pos x-fix)
                      (let ((id (intern buff)))
                        (setf (gethash id id-table) nil)
                        (make-token
                         :type 'IDENTIIFER
                         :val buff
                         :y-pos y-fix
                         :x-pos x-fix)))))
             (%read-digits-string (stream)
               (let ((x-fix x)
                     (y-fix y))
                 (do ((ch (read-char stream) (read-char stream nil 'eof)))
                     ((or (eq ch 'eof)
                          (not (digit-char-p ch)))
                      (unread-char ch stream))
                   (incf x)
                   (setq buff (nconc buff (list ch)))) ;; TODO:  rewrite
                 (make-token
                  :type 'UNSIGNED-INTEGER
                  :val (parse-integer (coerce buff 'string))
                  :y-pos y-fix
                  :x-pos x-fix)))
             (%read-comment (stream)
               (let (end-of-comment)
                 (do ((ch (read-char stream) (read-char stream nil 'eof)))
                     ((or (eq ch 'eof)
                          (and (eq ch #\*)
                               (eq (peek-char nil stream) #\))
                               (progn (read-char stream)
                                      (incf x)
                                      (setq end-of-comment t)))))
                   (if (eq ch #\Newline)
                       (progn (setq x 0)
                              (incf y))
                       (incf x)))
                 (unless end-of-comment
                   (list 'LEX-ERROR "Not closed comment")))))
      (%analysis)
      (when to-string
        (format t "Res tokens: ~S~%" res))
      (list id-table res))))

(defun keyword-p (word)
  (or (and (equal word "PROGRAM") 'PROGRAM)
      (and (equal word "BEGIN") 'BEGIN)
      (and (equal word "END") 'END)
      (and (equal word "LABEL") 'LABEL)
      (and (equal word "GOTO") 'GOTO)
      (and (equal word "LINK") 'LINK)
      (and (equal word "IN") 'IN)
      (and (equal word "OUT") 'OUT)))

(defun delimeter-p (ch)
  (or
   (and (eq ch #\;) (list 'DELIMETER 'SEMICOLON))
   (and (eq ch #\,) (list 'DELIMETER 'COMMA))
   (and (eq ch #\:) (list 'DELIMETER 'COLON))))

(defun white-char-p (ch)
  (or
   (eq ch #\space)
   (eq ch #\Tab)
   (eq ch #\Newline)
   (eq ch #\Backspace)
   (eq ch #\Return)))

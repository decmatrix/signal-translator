(uiop:define-package :signal/core/lexer/lexer-back
    (:use :cl
          :alexandria
          :anaphora)
  (:nicknames :signal/lexer-back
              :sig-lexer-back)
  (:export #:lexer
           ;; symbols
           #:PROGRAM
           #:BEGIN
           #:END
           #:LABEL
           #:GOTO
           #:LINK
           #:IN
           #:OUT
           #:SEMICOLON
           #:COMMA
           #:COLON
           ;; token struct
           #:token
           #:token-y-pos
           #:token-x-pos
           #:token-type
           #:token-val))

(in-package :signal/core/lexer/lexer-back)

(defstruct token
  "structure of token
----------------------------------------------
Fileds:
  `type' - type of token: keyword, id, num etc.
  `val' - value of token
  `y-pos' - number line in file
  `x-pos' - number column in file"  
  type
  val
  y-pos
  x-pos)

(defun lexer (input-file &key with-errors)
  "generate tokens from file `.sig' and return
list of this tokens
----------------------------------------------
Args:
  `input-file' - input file with programs `.sig'
  &key `with-errors' - save errors as part of result lexer for tests"
  (let ((x 1) ;; x pos in file
        (y 1) ;; y pos in file
        (id-table (make-hash-table :test #'eq))
        errors res buff x-fix y-fix)
    (labels ((%analysis ()
               (with-open-file (stream input-file)
                 (do ((ch (read-char stream) (read-char stream nil 'eof)))
                     ((eq ch 'eof))
                   (setq x-fix x)
                   (setq y-fix y)
                   (cond
                     ((and (eq ch #\()
                           (eq (peek-char nil stream) #\*))
                      (%read-comment stream))
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
                      (setq errors
                            (append errors
                                    (list (format nil
                                                  "Lexer: Error (line ~S, column ~S): Illegal symbol '~c'"
                                                  y x ch))))))
                   (if (eq ch #\Newline)
                       (progn
                         (setq x 1)
                         (incf y))
                       (incf x)))))
             (%read-identifier (stream id-table)
               (do ((ch (read-char stream) (read-char stream nil 'eof)))
                   ((or (eq ch 'eof)
                        (not (alphanumericp ch)))
                    (unread-char ch stream))
                 (incf x)
                 (setq buff (nconc buff (list ch))))
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
                       :val id
                       :y-pos y-fix
                       :x-pos x-fix))))
             (%read-digits-string (stream)
               (do ((ch (read-char stream) (read-char stream nil 'eof)))
                   ((or (eq ch 'eof)
                        (not (digit-char-p ch)))
                    (unread-char ch stream))
                 (incf x)
                 (setq buff (nconc buff (list ch))))

               (make-token
                :type 'UNSIGNED-INTEGER
                :val (parse-integer (coerce buff 'string))
                :y-pos y-fix
                :x-pos x-fix))
             (%read-comment (stream)
               (let (end-of-comment)
                 (do ((ch (read-char stream) (read-char stream nil 'eof)))
                     ((or (eq ch 'eof)
                          (and (eq ch #\*)
                               (eq (peek-char nil stream) #\))
                               (progn (read-char stream)
                                      (setq x (+ x 2))
                                      (setq end-of-comment t)))))
                   (if (eq ch #\Newline)
                       (progn (setq x 1)
                              (incf y))
                       (incf x)))
                 (unless end-of-comment
                   (setq  errors
                          (append errors
                                  (list
                                   (format nil "Lexer: Error (line ~S, column ~S): not closed comment"
                                           y-fix x-fix))))))))
      (%analysis)
      (if errors
          (if with-errors
              (list id-table res errors)
              (error "~S~%" (car errors)))
          (list id-table res)))))


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
  (or (and (eq ch #\;) (list 'DELIMETER 'SEMICOLON))
      (and (eq ch #\,) (list 'DELIMETER 'COMMA))
      (and (eq ch #\:) (list 'DELIMETER 'COLON))
      (and (eq ch #\.) (list 'DELIMETER 'DOT))))

(defun white-char-p (ch)
  (or (eq ch #\space)
      (eq ch #\Tab)
      (eq ch #\Newline)
      (eq ch #\Backspace)
      (eq ch #\Return)
      (eq ch #\Linefeed)
      (eq ch #\Page)))



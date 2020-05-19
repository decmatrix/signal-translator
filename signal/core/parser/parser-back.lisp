(uiop:define-package :signal/core/parser/parser-back
    (:use :cl
          :anaphora
          :signal/core/utils/error-utils
          :signal/core/parser/utils
          :signal/core/lexer/lexer-back)
  (:nicknames :signal/parser-back
              :sig-parser-back)
  (:export #:parser
           #:res-parser
           #:res-parser-get-tree
           #:res-parser-get-error
           ;; symbols
           #:<signal-program>
           #:<program>
           #:<block>
           #:<declarations>
           #:<label-declarations>
           #:<labels-list>
           #:<statements-list>
           #:<statement>
           #:<variable-identifier>
           #:<procedure-identifier>
           #:<keyword>
           #:<identifier>
           #:<unsigned-integer>
           #:<delimeter>))

(in-package :signal/core/parser/parser-back)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; TOKEN UTILS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defparameter *tokens* nil
  "Uses in `with-tokens' macros, stores copy of the tokens list and serves as
a stack of tokens.")

(defmacro with-tokens (tokens &body body)
  "Saves the context of used tokens, overrides the variable `*tokens*'. 
Any destructive manipulation of the tokens list will remain unchanged outside the macro.
------------------------------------------
Args:
 `tokens' - list of tokens,
 &body `body' - body."
  `(let ((*tokens* (copy-list ,tokens)))
     ,@body))

(defun next-token ()
  "Pop next token from tokens stack."
  (pop *tokens*))

(defun put-token (token)
  "Pusn current token to tokens stack.
------------------------
Args:
 `token' - token to push in stack."
  (push token *tokens*))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; TREE UNTILS ;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defparameter *tree* nil
  "Uses in `with-tree' macros, stores stores the current new sub-tree.")

(defparameter *root* nil
  "Uses in `with-tree' macros, stores the root of the current new sub-tree.")

(defmacro with-tree (init-node &body body)
  "Overrides the variables `*tree*' and `*root*' that contains the current created
sub-tree. The macro generates two functions that are available within the macro.
1) `@revert-tree' - cancels changes to the main tree,
2) `@revert-sub-tree' - cancels changes to the sub-tree.
If the function `@revert-tree' has not been applied, that means at the end of
the macro execution the new created sub-tree will be entered into the main tree.
------------------------------------------
Args:
 `init-node' - inital value of root of new sub-tree. If arg is `nil' means the function
in which this macro is used with such an initial value, the initial state function of the
automaton otherwise this is the normal state of the machine,
 `body' - body."
  (let ((new-sub-tree (gensym)))
    `(multiple-value-bind (res-body new-tree)
         (let* ((*root* ',init-node)
                (,new-sub-tree (when *root*
                                 (list *root*)))
                (*tree* ,new-sub-tree))
           (defun @revert-tree ()
             (setf *tree* nil))
           (defun @revert-sub-tree ()
             (setf *tree* ,new-sub-tree))
           (values (progn ,@body) *tree*))
       (when ',init-node
         (add-sub-tree new-tree))
       res-body)))

(defun add-sub-tree (sub-tree)
  "Add sub-tree to current tree.
------------------------------
Args:
 `sub-tree' - sub-tree to be added to the tree."
  (when sub-tree
    (labels ((%traverse (tree)
               (when tree
                 (let ((head (car tree)))
                   (if (eq head *root*)
                       (aif (cdr tree)
                            (cons head (append it (list sub-tree)))
                            (list head sub-tree))
                       (cons head (mapcar #'%traverse (cdr tree))))))))
      (setf *tree* (if *tree*
                       (%traverse *tree*)
                       sub-tree)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ERROR UTILS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun hand-error (&key reason token)
  "Will capture a caught error into a `*error*' variable by the parser and
the value of the `*error*' variable will be the generated error message.
-------------------------------------
Args:
 &key `reason' - reason of error occurred,
 &key `token' - token type of `token' struct with position in file."
  (throw-error
   (let ((pos-in-file (awhen (or token (car *tokens*))
                        (format nil " (line ~A, column ~A)"
                                (token-y-pos it)
                                (token-x-pos it))))
         (but-found (format nil "but ~A found"
                            (if token
                                (format nil "'~A'" (if (eq (token-type token) 'DELIMETER)
                                                       (delimeter-to-str (token-val token))
                                                       (token-val token)))
                                (aif (car *tokens*)
                                     (format nil "'~A'"
                                             (if (eq (token-type it) 'DELIMETER)
                                                 (delimeter-to-str (token-val it))
                                                 (token-val it)))
                                     "end of file")))))
     (format nil "Error~A: ~A ~A"
             (or pos-in-file "")
             (or reason "Expected end of file")
             but-found)))
  nil)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; MAIN ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass res-parser ()
  ((tree
     :initarg :init-tree
     :initform nil
     :accessor res-parser-get-tree
     :documentation "Result tree of parser (syntactiacal analyzer).")
   (parser-error
    :initarg :init-error
    :initform nil
    :accessor res-parser-get-error
    :documentation "Parrsing error."))
  (:documentation "Class represent result of lexical analyzer."))

(defmacro def-state (state-name (&rest args) &body body)
  "Generates a state-procedure (function) like `@<state-name>' of the automaton.
With the context of the new sub-tree (`<state-name>' ...). Each state is a grammar rule.
--------------------------------------------------------
Args:
 `state-name' - name of state, name of grammar rule like `<name>' -> ...',
 &rest `args' - args of state-procedure (function),
 &body `body' - body of state-procedure (function)."
  `(defun ,(intern (concatenate 'string (string '@) (string state-name))) ,args
     (with-tree ,state-name
       ,@body)))

(defun parser (result-lexer &key with-errors)
  "Syntactical analyzer.
----------------------------
Args:
 `result-lexer' - lexical analyzer result, structure of the `res-lexer' type,
 &key `with-errors' - return result with erors message, otherwise function will fail
with parser error."
  (when (typep result-lexer 'res-lexer)
    (catch-error ("Parser")
      (with-tree nil
        (@start (res-lexer-get-tokens result-lexer))
        (when (and (null with-errors) *error*)
          (error "~A~%" *error*))
        (make-instance 'res-parser
         :init-tree *tree*
         :init-error *error*)))))

;;;;;;;;;;;;;;;;;;;;;;;; STATETS ;;;;;;;;;;;;;;;;;;;;;;;
(defun @start (lexemes)
  (with-tokens lexemes
    (when (@<signal-program>)
      (when *tokens*
        (hand-error)))))

(def-state <signal-program> ()
  (@<program>))

(def-state <program> ()
  (and
   (check-token-value (next-token) 'PROGRAM)
   (@<procedure-identifier>)
   (check-token-value (next-token) 'SEMICOLON)
   (@<block>) 
   (check-token-value (next-token) 'DOT)))

(def-state <block> ()
  (and
   (@<declarations>)
   (check-token-value (next-token) 'BEGIN)
   (@<statements-list>)
   (check-token-value (next-token) 'END)))

(def-state <declarations> ()
  (@<label-declarations>))

(def-state <label-declarations> ()
  (let* ((token (next-token)))
    (if (check-token-value token 'LABEL t)
        (and
         (check-token-value (next-token) 'UNSIGNED-INTEGER)
         (@<labels-list>)
         (check-token-value (next-token) 'SEMICOLON))
        (progn (put-token token) t))))

(def-state <labels-list> ()
  (labels ((%local-<labels-list> ()
             (let ((token (next-token)))
               (if (and (check-token-value token 'COMMA t)
                        (check-token-value (next-token) 'UNSIGNED-INTEGER))
                   (%local-<labels-list>)
                   (progn (put-token token)
                          (unless *error*
                            t))))))
    (%local-<labels-list>)))

(def-state <statements-list> ()
  (labels ((%local-<statements-list> ()
             (if (@<statement>)
                 (%local-<statements-list>)
                 (unless *error*
                   t))))
    (%local-<statements-list>)))

(def-state <statement> ()
  (let ((token (next-token)))
    (cond
      ((check-token-value token 'UNSIGNED-INTEGER t)
       (and (check-token-value (next-token) 'COLON)
            (@<statement>)))
      ((check-token-value token 'LINK t)
       (and (@<variable-identifier>)
            (check-token-value (next-token) 'COMMA)
            (check-token-value (next-token) 'UNSIGNED-INTEGER)
            (check-token-value (next-token) 'SEMICOLON)))
      ((or (check-token-value token 'OUT t)
           (check-token-value token 'IN t)
           (check-token-value token 'GOTO t))
       (and (check-token-value (next-token) 'UNSIGNED-INTEGER)
            (check-token-value (next-token) 'SEMICOLON)))
      (t (progn (@revert-tree)
                (put-token token) nil)))))

(def-state <variable-identifier> ()
  (check-token-value (next-token) 'IDENTIFIER))

(def-state <procedure-identifier> ()
  (check-token-value (next-token) 'IDENTIFIER))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun check-token-value (token test-value &optional without-error)
  (let ((reason (if (value-is-type-p test-value)
                                      (make-expected-error-msg test-value)
                                      (make-expected-error-msg (get-type-of-value test-value)
                                                               test-value))))
    (if token
        (let ((val (token-val token)))
          (if (and
               (or (eq (token-type token) test-value)
                   (eq val test-value)))
              (add-sub-tree
               (cond
                 ((keyword-symbol-p test-value) (list '<keyword> val))
                 ((eq 'UNSIGNED-INTEGER test-value) (list '<unsigned-integer> val))
                 ((delimeter-symbol-p test-value) (list '<delimeter> val))
                 ((eq 'IDENTIFIER test-value) (list '<identifier> val))))
              (unless without-error
                (hand-error :reason reason
                            :token token))))
        (unless without-error
          (hand-error :reason reason)))))

(defun make-expected-error-msg (type-token &optional value-token)
  (case type-token
    (IDENTIFIER "Identifier expected")
    (UNSIGNED-INTEGER "Unsigned integer expected")
    (KEY-WORD (format nil "Keyword '~A' expected" value-token))
    (DELIMETER (format nil "Delimeter '~A' expected" (delimeter-to-str value-token)))))

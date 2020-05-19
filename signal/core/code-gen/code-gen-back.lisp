(uiop:define-package :signal/core/code-gen/code-gen-back
    (:use :cl
          :anaphora
          :signal/core/utils/error-utils
          :signal/core/parser/parser-back
          :signal/core/lexer/lexer-back)
  (:nicknames :signal/code-gen-back
              :sig-code-gen-back)
  (:export #:generate-output-code
           #:res-code-gen
           #:res-code-gen-get-output-code
           #:res-code-gen-get-error))

(in-package :signal/core/code-gen/code-gen-back)

(defparameter *label-table* nil
  "Use in `with-context' macros, store information about declared labels.")
(defparameter *identifier-table* nil
  "Use in `with-context' macros, store information about idetifiers in program.")
(defparameter *used-labels* nil
  "Use in `with-context' macros, store information about used labels in program.")
(defparameter *ports-in/out-table* nil
  "Use in `with-context' macros, store information about ports for in/out asm commands.")
(defparameter *data-segment* nil
  "Use in `with-context' macors, store asm code for data segment.")
(defparameter *code-segement* nil
  "Use in `with-context' macors, store asm code for code segment.")

(defmacro with-context (&body body)
  "Create context with informations about lable, ports, ids etc.
-------------------------
Args:
 &body `body' - body of macros"
  `(let ((*label-table* (make-hash-table :test #'eq))
         (*identifier-table* (make-hash-table :test #'eq))
         (*used-labels* '())
         (*ports-in/out-table* (make-hash-table :test #'eq))
         (*data-segment* '(".data"))
         (*code-segement* '(".code")))
     ,@body))

(defstruct port
  "Structure of port information
-------------------------
Fields:
  `val' - number of ports in HEX NS,
  `type' - type of port for input (IN) or for output (OUT),
  `is-need-dx' - flag indicates that the port number is greater than
8 bits and should be stored in the DX register."
  val
  type
  is-need-dx)

(defclass res-code-gen ()
  ((output-code
    :initarg :init-output-code
    :initform nil
    :accessor res-code-gen-get-output-code
    :documentation "Result of code generator (ouput code of assembly language.")
   (code-gen-error
    :initarg :init-error
    :initform nil
    :accessor res-code-gen-get-error
    :documentation "Code generation error."))
  (:documentation "Class represent result of code generator."))

(defun throw-code-gen-error (reason)
  "Throw code generator error to `*error*'
-------------------------
Args:
 `reason' - reason of error."
  (throw-error (format nil "Error: ~A" reason)))

(defgeneric generate-output-code (instance &key with-errors)
  (:documentation "Translates the parsing tree of the input l
anguage into the output assembler language
-------------------------
Args:
 `instance' - instance of res-parser class,
 &key `with-errors' - return result with erors message, otherwise function will fail
with parser error."))

(defmethod generate-output-code ((obj res-parser) &key with-errors)
  (with-context
    (catch-error ("Code generator")
      (collect-used-labels (res-parser-get-tree obj))
      (traverse-tree (res-parser-get-tree obj))
      (let ((output-code (build-asm-code)))
        (when (and (null with-errors) *error*)
          (error "~A~%" *error*))
        (make-instance 'res-code-gen
                       :init-output-code output-code
                       :init-error *error*)))))

(defun collect-used-labels (tree)
  (when (listp tree)
    (if (and (eq (car tree) '<statement>)
             (eq (car (second tree)) '<unsigned-integer>))
        (let ((label (second (second tree))))
          (if (label-was-used-p label)
              (throw-code-gen-error
               (format nil "<~A> uncertainty more than once using a label"
                       label))
              (add-used-label label))
          (collect-used-labels (fourth tree)))
        (mapc #'collect-used-labels (cdr tree)))))

(defun traverse-tree (tree)
  (when (and (listp tree) (null *error*))
    (let ((root (car tree)))
      (case root
        (<procedure-identifier>
         (add-str-code (build-label-stmt
                        (add-asm-label (second (second tree))))))
        (<unsigned-integer>
         (let ((label (second tree)))
           (if (get-asm-label label)
               (throw-code-gen-error
                (format nil "<~A> label has already been declared" label))
               (add-asm-label label))))
        (<statements-list>
         (if (cdr tree)
             (mapc #'traverse-tree (cdr tree))
             (add-str-code "NOP")))
        (<statement>
         (let ((first-child (second tree)))
           (if (eq (car first-child) '<unsigned-integer>)
               (let ((label (second first-child)))
                 (aif (get-asm-label label)     
                      (add-str-code (build-label-stmt it))
                      (throw-code-gen-error
                       (format nil "<~A> label not declared" label)))
                 (traverse-tree (fourth tree)))
               (case (second (second tree))
                 (GOTO
                  (let ((label (second (third tree))))
                    (aif (get-asm-label label)
                         (if (label-was-used-p label)
                             (add-str-code (build-jmp-stmt it))
                             (throw-code-gen-error
                              (format
                               nil
                               "<~A> jump to the label to which has been declared but not used"
                               label)))
                         (throw-code-gen-error
                          (format nil "<~A> label not declared"
                                  label)))))
                 (LINK
                  (let ((id (second (second (third tree))))
                        (num (second (fifth tree))))
                    (aif (gethash num *ports-in/out-table*)
                         (let ((str-id (or (get-id id)
                                           (let ((string-id (add-identifier id)))
                                             (add-str-code (build-data-stmt string-id)
                                                           :segment :data)
                                             string-id)))
                               (port-or-reg (if (port-is-need-dx it)
                                                "DX"
                                                (port-val it))))
                           (add-str-code (build-xor-stmt "AX"))
                           (add-str-code (build-xor-stmt str-id))
                           (when (port-is-need-dx it)
                             (add-str-code (build-xor-stmt "DX"))
                             (add-str-code (build-mov-stmt "DX" (port-val it))))
                           (case (port-type it)
                             (:in
                              (add-str-code (build-in-stmt port-or-reg))
                              (add-str-code (build-mov-stmt str-id "AX")))
                             (:out
                              (add-str-code (build-mov-stmt "AX" str-id))
                              (add-str-code (build-out-stmt port-or-reg)))))
                         (throw-code-gen-error
                          (format nil
                                  "<~A> port action not defined"
                                  num)))))
                 ((IN OUT)
                  (let ((num (second (third tree))))
                    (if (gethash num *ports-in/out-table*)
                        (throw-code-gen-error
                         (format nil
                                 "<~A> port action already defined"
                                 num))
                        (let ((keyword-val (second (second tree))))
                          (if (num-in-range-16-bit-p num)
                              (setf (gethash num *ports-in/out-table*)
                                    (make-port
                                     :val (format nil "~xh" num)
                                     :type (case keyword-val
                                             (IN :in)
                                             (OUT :out))
                                     :is-need-dx (not (num-in-range-8-bit-p num))))
                              (throw-code-gen-error
                               (format nil
                                       "<~A> for operator <~A> must be in range 16 bit"
                                       num
                                       keyword-val)))))))))))
        (t (mapc #'traverse-tree (cdr tree)))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ASM STR CODE BUILDERS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun build-label-stmt (label)
  (format nil "~A:" label))

(defun build-xor-stmt (operand)
  (format nil "XOR ~A, ~A" operand operand))

(defun build-mov-stmt (left-operand right-operand)
  (format nil "MOV ~A, ~A" left-operand right-operand))

(defun build-in-stmt (port)
  (format nil "IN AX, ~A" port))

(defun build-data-stmt (id)
  (format nil "~A DW 0h" id))

(defun build-jmp-stmt (label)
  (format nil "JMP ~A" label))

(defun build-out-stmt (port)
  (format nil "OUT ~A, AX" port))

(defun build-asm-code ()
  (concatenate
   'string
   (format nil ".i386~%~%~%")
   (when (cdr *data-segment*)
     (concatenate 'string
                  (reduce (lambda (acc val)
                            (format nil "~A~A~%" acc val))
                          *data-segment*
                          :initial-value "")
                  (format nil "~%~%")))
   (reduce (lambda (acc val)
             (format nil "~A~A~%" acc val))
           *code-segement*
           :initial-value "")
   (format nil "~%~%MOV AX, 4c00h~%INT 21h")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;; FUNCTIONS FOR WORK CONTEXT TABLES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ADDERS
(defun add-asm-label (name)
  (let ((asm-label (concatenate 'string "@" (format nil "~A" name))))
    (setf (gethash name *label-table*) asm-label)
    asm-label))

(defun add-identifier (id)
  (let ((str-id (format nil "VAR_~A" id)))
    (setf (gethash id *identifier-table*) str-id)
    str-id))

(defun add-used-label (label)
  (setf *used-labels* (cons label *used-labels*)))

(defun add-str-code (str &key (segment :code))
  (case segment
    (:code
     (setf *code-segement* (append *code-segement* (list str))))
    (:data
     (setf *data-segment* (append *data-segment* (list str))))
    (t (error "Unknown segement ~A~%" segment))))

;; GETTERS
(defun get-asm-label (name)
  (gethash name *label-table*))

(defun get-id (id)
  (gethash id *identifier-table*))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; PREDICATE FUNCTIONS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
(defun num-in-range-16-bit-p (number)
  (and (>= number 0) (<= number 65535)))

(defun num-in-range-8-bit-p (number)
  (and (>= number 0) (<= number 255)))

(defun label-was-used-p (label)
  (find label *used-labels*
        :test #'eq))

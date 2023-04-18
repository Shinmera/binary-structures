#|
 This file is a part of binary-structures
 (c) 2023 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.binary-structures)

(define-condition no-such-io-type (error)
  ((designator :initarg :designator :reader designator))
  (:report (lambda (c s) (format s "No io-type with name~%  ~s" (designator c)))))

(defvar *io-backends* ())
(defvar *io-types* (make-hash-table :test 'eql))

(defclass io-backend () ())
(defclass io-type () ())

(defgeneric read-defun (backend io-type name))
(defgeneric write-defun (backend io-type name))
(defgeneric read-form (backend io-type))
(defgeneric write-form (backend io-type value-variable))
(defgeneric lisp-type (io-type))
(defgeneric default-value (io-type))
(defgeneric octet-size (io-type))
(defgeneric initargs (io-type)
  (:method-combination append :most-specific-first))
(defgeneric parse-io-type (type &rest args))

(defmethod read-defun ((backend symbol) io-type name)
  (read-defun (make-instance backend) io-type name))

(defmethod read-defun (backend (io-type T) name)
  (read-defun backend (parse-io-type io-type) name))

(defmethod write-defun ((backend symbol) io-type name)
  (write-defun (make-instance backend) io-type name))

(defmethod write-defun (backend (io-type T) name)
  (write-defun backend (parse-io-type io-type) name))

(defmethod read-form ((backend symbol) io-type)
  (read-form (make-instance backend) io-type))

(defmethod read-form (backend (io-type T))
  (read-form backend (parse-io-type io-type)))

(defmethod write-form ((backend symbol) io-type value-variable)
  (write-form (make-instance backend) io-type value-variable))

(defmethod write-form (backend (io-type T) value-variable)
  (write-form backend (parse-io-type io-type) value-variable))

(defmethod lisp-type (type)
  (lisp-type (parse-io-type type)))

(defmethod default-value (type)
  (default-value (parse-io-type type)))

(defmethod octet-size (type)
  (octet-size (parse-io-type type)))

(defun io-type (name &optional (errorp T))
  (or (when (typep name 'io-type) name)
      (gethash name *io-types*)
      (when errorp
        (restart-case (error 'no-such-io-type :designator name)
          (continue ()
            :report "Continue using the top-type."
            (make-instance 'top-type))
          (use-value (value)
            :report "Use the specified designator instead."
            (io-type value))))))

(defun (setf io-type) (value name)
  (if value
      (setf (gethash name *io-types*) value)
      (remhash name *io-types*))
  value)

(defmacro define-io-type ((type name) &body initargs)
  `(setf (io-type ',name) (make-instance ',type ,@initargs)))

(defun list-io-types ()
  (alexandria:hash-table-keys *io-types*))

(defun list-io-backends ()
  (copy-list *io-backends*))

(defmacro define-io-backend (name &rest slots)
  `(progn
     (defclass ,name (io-backend) ,slots)
     (pushnew ',name *io-backends*)
     ',name))

(defmethod parse-io-type ((type cons) &rest args)
  (apply #'parse-io-type (car type) (append (cdr type) args)))

(defmethod parse-io-type ((type symbol) &rest args)
  (let ((type (io-type type)))
    (apply #'make-instance (type-of type) (append args (initargs type)))))

(defmacro define-io-type-parser (designator lambda-list &body body)
  (let ((args (gensym "ARGS")))
    `(defmethod parse-io-type ((type (eql ',designator)) &rest ,args)
       (destructuring-bind ,lambda-list ,args
         ,@body))))

(defmacro define-io-backend-functions (io-backend io-type)
  (let ((reader-name (intern* 'read- io-backend '- io-type))
        (writer-name (intern* 'write- io-backend '- io-type)))
    `(progn
       ,(read-defun io-backend io-type reader-name)
       ,(write-defun io-backend io-type writer-name))))

(defmacro define-io-functions (io-type)
  `(progn
     ,@(loop for backend in *io-backends*
             collect `(define-io-backend-functions ,backend ,io-type))))

(defmacro define-io-alias (name expansion)
  `(define-io-type-parser ,name ()
     (parse-io-type ',expansion)))

(defclass top-type (io-type) ())

(defmethod lisp-type ((type top-type)) T)
(defmethod default-value ((type top-type)) NIL)
(defmethod initargs append ((type top-type)) ())

(defclass numeric-type (io-type)
  ((octet-size :initarg :octet-size :accessor octet-size)
   (order :initarg :order :initform :little-endian :accessor order)))

(defmethod initargs append ((type numeric-type))
  (list :octet-size (octet-size type)))

(defclass io-integer (numeric-type)
  ((signed-p :initarg :signed-p :initform T :accessor signed-p)))

(define-print-object-method io-integer
  "~:[S~;U~]B ~d ~a" signed-p (* 8 (octet-size io-integer)) order)

(defmethod default-value ((type io-integer))
  0)

(defmethod lisp-type ((type io-integer))
  (list (if (signed-p type) 'signed-byte 'unsigned-byte)
        (* 8 (octet-size type))))

(defmethod initargs append ((type io-integer))
  (list :order (order type) :signed-p (signed-p type)))

(defclass io-float (numeric-type)
  ())

(define-print-object-method io-float
  "~d ~a" (* 8 (octet-size io-float)) order)

(defmethod default-value ((type io-float))
  (ecase (octet-size type)
    (2 0s0)
    (4 0f0)
    (8 0d0)
    (16 0l0)))

(defmethod lisp-type ((type io-float))
  (ecase (octet-size type)
    (2 'short-float)
    (4 'single-float)
    (8 'double-float)
    (16 'long-float)))

(defclass io-vector (io-type)
  ((element-type :initarg :element-type :initform 'uint8 :accessor element-type)
   (element-count :initarg :element-count :initform '* :accessor element-count)
   (element-offset :initarg :element-offset :initform NIL :accessor element-offset)
   (stride :initarg :stride :initform NIL :accessor stride)))

(define-print-object-method io-vector
  "~a ~a" element-type element-count)

(defmethod read-form ((backend io-backend) (type io-vector))
  `(let ((vector (make-array ,(read-form backend (element-count type))
                             :element-type ',(lisp-type (element-type type)))))
     (dotimes (i (length vector) vector)
       (setf (aref vector i) ,(read-form backend (element-type type))))))

(defmethod write-form :around ((backend io-backend) (type io-vector) value-variable)
  ;; If the element-count was an immediate type, emit it.
  (if (io-type (element-count type) NIL)
      `(progn (let ((value (length ,value-variable)))
                ,(write-form backend (element-count type) 'value))
              ,(call-next-method))
      (call-next-method)))

(defmethod write-form ((backend io-backend) (type io-vector) value-variable)
  (let ((element (gensym "ELEMENT")))
    `(dotimes (i (length ,value-variable))
       (let ((,element (aref ,value-variable i)))
         ,(write-form backend (element-type type) element)))))

(defmethod lisp-type ((type io-vector))
  `(simple-array ,(lisp-type (element-type type))
                 (,(if (numberp (element-count type))
                       (element-count type)
                       '*))))

(defmethod default-value ((type io-vector))
  `(make-array 0 :element-type ',(lisp-type (element-type type))))

(defmethod octet-size ((type io-vector))
  (let ((count (element-count type)))
    (if (numberp count)
        (* count 
           (octet-size (element-type type)))
        '*)))

(defmethod initargs append ((type io-vector))
  (list :element-type (element-type type)
        :element-count (element-count type)
        :element-offset (element-offset type)
        :stride (stride type)))

(define-io-type-parser vector (element-type &optional (element-count '*) element-offset)
  (make-instance 'io-vector :element-type element-type
                            :element-count element-count
                            :element-offset element-offset))

(defclass io-string (io-type) 
  ((element-count :initarg :element-count :initform '* :accessor element-count)
   (encoding :initarg :encoding :initform :utf-8 :accessor encoding)))

(define-print-object-method io-string
  "~a ~a" encoding element-count)

(defmethod read-form ((backend io-backend) (type io-string))
  `(let ((octet ,(read-form backend (make-instance 'io-vector :element-count (element-count type)))))
     (babel:octets-to-string octet :encoding ,(encoding type))))

(defmethod write-form ((backend io-backend) (type io-string) value-variable)
  `(let ((octets (babel:string-to-octets ,value-variable :encoding ,(encoding type))))
     ,(write-form backend (make-instance 'io-vector :element-count (element-count type))
                  'octets)))

(defmethod lisp-type ((type io-string)) 'string)
(defmethod default-value ((type io-string)) "")

(defmethod octet-size ((type io-string))
  (let ((count (element-count type)))
    (if (numberp count) count '*)))

(defmethod initargs :around ((type io-string))
  (list :encoding (encoding type)
        :element-count (element-count type)))

(define-io-type-parser string (element-count &optional (encoding :utf-8))
  (make-instance 'io-string :element-count element-count
                            :encoding encoding))

(defclass io-case (io-type)
  ((value-type :initarg :value-type :accessor value-type)
   (cases :initarg :cases :accessor cases)))

(defmethod read-form ((backend io-backend) (type io-case))
  (let ((value (gensym "VALUE")))
    `(let ((,value ,(read-form backend (value-type type))))
       (cond ,@(loop for (test form) in (cases type)
                     collect (list (etypecase test
                                     (number `(= ,value ,test))
                                     (character `(char= ,value ,test))
                                     (string `(string= ,value ,test))
                                     (vector `(equalp ,value ,test)))
                                   (etypecase form
                                     (cons
                                      (if (eql 'quote (car form))
                                          form
                                          (read-form backend form)))
                                     ((or number character array keyword (eql T) (eql NIL))
                                      form)
                                     (symbol
                                      (read-form backend form)))))))))

(defmethod write-form ((backend io-backend) (type io-case) value)
  `(cond ,@(loop for (form test) in (cases type)
                 collect (list (etypecase test
                                 (number `(= ,value ,test))
                                 (character `(char= ,value ,test))
                                 (string `(string= ,value ,test))
                                 (vector `(equalp ,value ,test))
                                 ((or keyword (eql T) (eql NIL)) `(eq ,value ,test))
                                 (cons
                                  (if (eql 'quote (car form))
                                      `(eq ,value ,test)
                                      `(typep ,value ',(lisp-type test))))
                                 (symbol `(typep ,value ',(lisp-type test))))
                               (write-form backend (value-type type) form)
                               (typecase test
                                 ((and symbol (not (or keyword (eql T) (eql NIL))))
                                  (write-form backend test value))
                                 (cons
                                  (unless (eql 'quote (car form))
                                    (write-form backend test value))))))))

(defmethod lisp-type ((type io-case)) T)
(defmethod default-value ((type io-case)) NIL)
(defmethod octet-size ((type io-case))
  (octet-size (value-type type)))

(define-io-type-parser case (value-type &rest cases)
  (make-instance 'io-case :value-type value-type
                          :cases cases))

(defclass io-structure (io-type)
  ((value-type :initarg :value-type :accessor value-type)
   (constructor :initarg :constructor :accessor constructor)
   (slots :initarg :slots :initform () :accessor slots)))

(define-print-object-method io-structure
  "~a" value-type)

(defmethod lisp-type ((type io-structure))
  (value-type type))

(defmethod default-value ((type io-structure))
  (list (constructor type)))

(defmethod octet-size ((type io-structure))
  (let ((last (car (last (slots type)))))
    (or (unspecific-p (offset last) (octet-size last))
        (+ (offset last)
           (octet-size last)))))

(defmethod initargs append ((type io-structure))
  (list :value-type (value-type type)
        :constructor (constructor type)
        :slots (slots type)))

(defvar *io-structure*)

(define-io-type-parser slot-value (slot)
  (find slot (slots *io-structure*) :key #'name))

(defclass io-structure-slot ()
  ((value-type :initarg :value-type :accessor value-type)
   (octet-size :initarg :octet-size :accessor octet-size)
   (offset :initarg :offset :accessor offset)
   (name :initarg :name :accessor name)))

(define-print-object-method io-structure-slot
  "~a ~a" name value-type)

(defmethod read-form ((backend io-backend) (type io-structure-slot))
  `(slot ,(name type)))

(defmethod write-form ((backend io-backend) (type io-structure-slot) value-variable)
  ())

(defclass io-structure-magic (io-structure-slot)
  ((value-type :initform NIL)
   (name :initform NIL)
   (default-value :initarg :default-value :initform NIL :accessor default-value)))

(defmethod shared-initialize :after ((type io-structure-magic) slots &key (default-value NIL default-value-p))
  (when default-value-p (setf (default-value type) default-value)))

(define-print-object-method io-structure-magic
  "~s" (or (default-value io-structure-magic) (value-type io-structure-magic)))

(defmethod (setf default-value) ((value string) (type io-structure-magic))
  (setf (default-value type) (map '(simple-array (unsigned-byte 8) (*)) #'char-code value)))

(defmethod read-form ((backend io-backend) (type io-structure-magic))
  (if (value-type type)
      `(let ((value ,(read-form backend (value-type type))))
         (declare (ignorable value))
         ,(when (default-value type)
            `(assert (equalp value ,(default-value type)))))
      `(assert (and ,@(loop for value across (default-value type)
                            collect `(= ,value ,(read-form backend 'uint8))))
               () "Check for magic value failed, expected~{~%  ~{~2,'0X ~c~}~}"
               ',(loop for value across (default-value type)
                       collect (list value (code-char value))))))

(defmethod write-form ((backend io-backend) (type io-structure-magic) value-variable)
  (cond ((null (value-type type))
         `(progn ,@(loop for value across (default-value type)
                         collect (write-form backend 'uint8 value))))
        ((null (default-value type))
         (write-form backend (value-type type) (default-value (value-type type))))
        (T
         (write-form backend (value-type type) (default-value type)))))

(defmethod read-form ((backend io-backend) (type io-structure))
  (let ((value (gensym "VALUE"))
        (*io-structure* type))
    `(let ((,value ,(default-value type)))
       (macrolet ((slot (name)
                    (list (intern* ',(lisp-type type) '- name) ',value)))
         ,@(loop for slot in (slots type)
                 collect (if (typep slot 'io-structure-magic)
                             (read-form backend slot)
                             `(setf (,(intern* (lisp-type type) '- (name slot)) ,value)
                                    ,(read-form backend (value-type slot))))))
       ,value)))

(defmethod write-form ((backend io-backend) (type io-structure) value-variable)
  (let ((value (gensym "VALUE"))
        (*io-structure* type))
    `(macrolet ((slot (name)
                  (list (intern* ',(lisp-type type) '- name) ',value)))
       ,@(loop for slot in (slots type)
               collect (if (typep slot 'io-structure-magic)
                           (write-form backend slot value-variable)
                           `(let ((,value (,(intern* (lisp-type type) '- (name slot)) ,value-variable)))
                              ,(write-form backend (value-type slot) value)))))))

(defun parse-io-structure-slots (slots)
  (loop with total-offset = 0
        for slot in slots
        collect (etypecase slot
                  (cons
                   (destructuring-bind (name type &key (offset total-offset) (size (octet-size type)) (align 1) (pad 0)) slot
                     (setf offset (* (ceiling offset align) align))
                     (prog1 (make-instance 'io-structure-slot
                                           :value-type type
                                           :octet-size size
                                           :offset offset
                                           :name name)
                       (unless (numberp size)
                         (setf size 0))
                       (setf total-offset (+ offset size pad)))))
                  (symbol
                   (prog1 (make-instance 'io-structure-magic
                                         :value-type slot
                                         :offset total-offset)
                     (incf total-offset (octet-size slot))))
                  (T
                   (prog1 (make-instance 'io-structure-magic
                                         :default-value slot
                                         :octet-size (length slot)
                                         :offset total-offset)
                     (incf total-offset (length slot)))))))

(defmacro define-io-structure (name &body slots)
  (let ((constructor (intern* 'make- name)))
    (handler-bind ((no-such-io-type #'continue))
      `(progn
         (defstruct (,name
                     (:constructor ,constructor))
           ,@(loop for slot in slots
                   when (consp slot)
                   collect `(,(first slot)
                             ,(default-value (second slot))
                             :type ,(lisp-type (second slot)))))

         (define-io-type (io-structure ,name)
           :value-type ',name
           :constructor ',constructor
           :slots (parse-io-structure-slots ',slots))))))

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

(defclass io-backend () 
  ((offset :initarg :offset :initform 0 :accessor offset)))

(defclass io-type () ())

(defgeneric read-defun (backend io-type name))
(defgeneric write-defun (backend io-type name))
(defgeneric read-form (backend io-type))
(defgeneric write-form (backend io-type value-variable))
(defgeneric index-form (backend))
(defgeneric seek-form (backend offset))
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

(defmethod index-form :around ((backend io-backend))
  (if (unspecific-p (offset backend))
      (call-next-method)
      (offset backend)))

(defmethod seek-form :around ((backend io-backend) offset)
  (let ((current (offset backend)))
    (prog1 (if (unspecific-p offset current)
               (call-next-method)
               (let ((diff (- offset current)))
                 (unless (= 0 diff)
                   (call-next-method))))
      (setf (offset backend) offset))))

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
(defmethod octet-size ((type top-type)) '*)

(defclass numeric-type (io-type)
  ((octet-size :initarg :octet-size :accessor octet-size)
   (order :initarg :order :initform :little-endian :accessor order)))

(defmethod initargs append ((type numeric-type))
  (list :octet-size (octet-size type)))

(defmethod read-form :after ((backend io-backend) (type numeric-type))
  (setf (offset backend) (or (unspecific-p (offset backend) (octet-size type))
                             (+ (offset backend) (octet-size type)))))

(defmethod write-form :after ((backend io-backend) (type numeric-type) value-variable)
  (setf (offset backend) (or (unspecific-p (offset backend) (octet-size type))
                             (+ (offset backend) (octet-size type)))))

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

(defmethod read-form :around ((backend io-backend) (type io-vector))
  (let ((offset (offset backend)))
    (prog1 (call-next-method)
      (setf (offset backend) (or (unspecific-p offset (octet-size type))
                                 (+ offset (octet-size type)))))))

(defmethod write-form :around ((backend io-backend) (type io-vector) value-variable)
  (let ((offset (offset backend)))
    (prog1 (call-next-method)
      (setf (offset backend) (or (unspecific-p offset (octet-size type))
                                 (+ offset (octet-size type)))))))

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
                                   (if (constantp form)
                                       form
                                       (read-form backend form))))
             (T (error "The value~%  ~a~%is not among the set of known values:~%~a"
                       ,value ',(mapcar #'first (cases type))))))))

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
                                    (write-form backend test value))))))
         (T (error "The value~%  ~a~%is not among the set of accepted values:~%~a"
                   ,value ',(mapcar #'first (cases type))))))

(defmethod lisp-type ((type io-case)) T)
(defmethod default-value ((type io-case)) NIL)
(defmethod octet-size ((type io-case))
  (let ((size (octet-size (value-type type)))
        (add NIL))
    (dolist (case (cases type))
      (unless (constantp (second case))
        (cond ((null add)
               (setf add (octet-size (second case))))
              ((not (eql add (octet-size (second case))))
               (setf add '*)))))
    (or (unspecific-p size add)
        (+ size add))))

(define-io-type-parser case (value-type &rest cases)
  (make-instance 'io-case :value-type value-type
                          :cases cases))

(defclass io-value (io-type)
  ((form :initarg :form :accessor form)))

(defmethod read-form ((backend io-backend) (type io-value))
  (form type))

(defmethod write-form ((backend io-backend) (type io-value) value)
  (form type))

(defmethod lisp-type ((type io-type)) T)
(defmethod default-value ((type io-type)) NIL)
(defmethod octet-size ((type io-typecase)) '*)

(defmacro define-io-value-form (name &optional (function name))
  `(define-io-type-parser ,name (&rest args)
     (make-instance 'io-value :form (list* ',function args))))

(define-io-value-form slot-value slot)
(define-io-value-form slot)
(define-io-value-form +)
(define-io-value-form -)
(define-io-value-form *)
(define-io-value-form /)
(define-io-value-form expt)
(define-io-value-form ash)

(defclass io-typecase (io-value)
  ((cases :initarg :cases :accessor cases)))

(defmethod read-form ((backend io-backend) (type io-typecase))
  (let ((value (gensym "VALUE")))
    `(let ((,value ,(read-form backend (form type))))
       (typecase ,value
         ,@(loop for (type form) in (cases type)
                 collect (list type
                               (if (constantp form)
                                   form
                                   (read-form backend form))))))))

(defmethod write-form ((backend io-backend) (type io-typecase) value)
  (let ((value (gensym "VALUE")))
    `(let ((,value ,(read-form backend (form type))))
       (typecase ,value
         ,@(loop for (type form) in (cases type)
                 collect (list type
                               (if (constantp form)
                                   form
                                   (write-form backend form value))))))))

(defmethod octet-size ((type io-typecase))
  (let ((add NIL))
    (dolist (case (cases type))
      (unless (constantp (second case))
        (cond ((null add)
               (setf add (octet-size (second case))))
              ((not (eql add (octet-size (second case))))
               (setf add '*)))))
    (or (unspecific-p add)
        add '*)))

(define-io-type-parser typecase (form &rest cases)
  (make-instance 'io-typecase :form form :cases cases))

(defclass io-structure (io-type)
  ((value-type :initarg :value-type :accessor value-type)
   (constructor :initarg :constructor :accessor constructor)
   (slots :initarg :slots :initform () :accessor slots)
   (name :initarg :name :initform NIL :accessor name)))

(define-print-object-method io-structure
  "~a" value-type)

(defmethod describe-object ((structure io-structure) stream)
  (format stream "~a~%  [~s]~%~%"
          (name structure) (type-of structure))
  (format stream "ADDRESS  SIZE TYPE     NAME")
  (loop for slot in (slots structure)
        do (format stream "~%~:[        ~;~:*~8,'0x~] ~4a ~8a ~a"
                   (unless (unspecific-p (offset slot))
                     (offset slot))
                   (octet-size slot)
                   (truncate-text (princ-to-string (value-type slot)) 8)
                   (or (name slot) "----"))))

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

(defmethod find-slot (name (structure io-structure))
  (or (find name (slots structure) :key #'name)
      (error "No slot with name ~s found on ~s" name structure)))

(defmethod find-slot (name (type T))
  (find-slot name (parse-io-type type)))

(defclass io-structure-slot ()
  ((value-type :initarg :value-type :accessor value-type)
   (octet-size :initarg :octet-size :accessor octet-size)
   (offset :initarg :offset :accessor offset)
   (name :initarg :name :accessor name)))

(define-print-object-method io-structure-slot
  "~a ~a" name value-type)

(defmethod default-value ((slot io-structure-slot))
  (default-value (value-type slot)))

(defmethod lisp-type ((slot io-structure-slot))
  (lisp-type (value-type slot)))

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
  (cond ((null (value-type type))
         `(assert (and ,@(loop for value across (default-value type)
                               collect `(= ,value ,(read-form backend 'uint8))))
                  () "Check for magic value failed, expected~{~%  ~{~2,'0X ~c~}~}"
                  ',(loop for value across (default-value type)
                          collect (list value (code-char value)))))
        ((default-value type)
         `(let ((value ,(read-form backend (value-type type))))
            (assert (equalp value ,(default-value type)))))
        (T
         (read-form backend (value-type type)))))

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
       (macrolet ((slot (name &rest descendants)
                    (let ((value (list (intern* ',(lisp-type type) '- name) ',value)))
                      (dolist (name descendants value)
                        (setf value `(slot-value ,value ',name))))))
         ,@(loop for slot in (slots type)
                 for align = (seek-form backend (offset slot))
                 when align collect align
                 collect (if (typep slot 'io-structure-magic)
                             (read-form backend slot)
                             `(setf (,(intern* (lisp-type type) '- (name slot)) ,value)
                                    ,(read-form backend (value-type slot))))))
       ,value)))

(defmethod write-form ((backend io-backend) (type io-structure) value-variable)
  (let ((value (gensym "VALUE"))
        (*io-structure* type))
    `(macrolet ((slot (name &rest descendants)
                  (let ((value (list (intern* ',(lisp-type type) '- name) ',value-variable)))
                    (dolist (name descendants value)
                      (setf value `(slot-value ,value ',name))))))
       ,@(loop for slot in (slots type)
               for align = (seek-form backend (offset slot))
               when align collect align
               collect (if (typep slot 'io-structure-magic)
                           (write-form backend slot value-variable)
                           `(let ((,value (,(intern* (lisp-type type) '- (name slot)) ,value-variable)))
                              ,(write-form backend (value-type slot) value)))))))

(defun parse-io-structure-slots (defs)
  (let ((slots ())
        (total-offset 0))
    (flet ((finish (slot &optional (pad 0))
             (let ((end (or (unspecific-p (octet-size slot) (offset slot))
                            (+ (octet-size slot) (offset slot) pad))))
               (setf total-offset (or (unspecific-p end total-offset)
                                      (+ end total-offset)))
               (push slot slots))))
      (dolist (slot defs (nreverse slots))
        (etypecase slot
          (cons
           (destructuring-bind (name type &key (offset total-offset) (size (octet-size type)) (align 1) (pad 0)) slot
             (case name
               (:include
                (dolist (slot (slots (io-type type)))
                  (finish slot)))
               (T
                (finish (make-instance 'io-structure-slot
                                       :value-type type
                                       :octet-size size
                                       :offset (or (unspecific-p offset)
                                                   (* (ceiling offset align) align))
                                       :name name)
                        pad)))))
          (symbol
           (finish (make-instance 'io-structure-magic
                                  :value-type slot
                                  :octet-size (octet-size slot)
                                  :offset total-offset)))
          (T
           (finish (make-instance 'io-structure-magic
                                  :default-value slot
                                  :octet-size (length slot)
                                  :offset total-offset))))))))

(defmacro define-io-structure (name &body slots)
  (let ((constructor (intern* 'make- name))
        (include (when (and (listp (first slots)) (eql :include (caar slots)))
                   (pop slots)))
        (slotdefs ()))
    (handler-bind ((no-such-io-type #'continue))
      (dolist (slot slots)
        (when (consp slot)
          (case (first slot)
            (:include
             (dolist (slot (slots (io-type (second slot))))
               (push `(,(name slot) ,(default-value slot) :type ,(lisp-type slot))
                     slotdefs)))
            (T
             (push `(,(first slot) ,(default-value (second slot)) :type ,(lisp-type (second slot))) 
                   slotdefs)))))
      `(progn
         (defstruct (,name
                     (:constructor ,constructor)
                     ,@(when include `((:include ,(second include)))))
           ,@(nreverse slotdefs))

         (define-io-type (io-structure ,name)
           :name ',name
           :value-type ',name
           :constructor ',constructor
           :slots (parse-io-structure-slots ',(if include (list* include slots) slots)))))))

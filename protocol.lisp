(in-package #:org.shirakumo.binary-structures)

;; TODO: thin representation

(defvar *io-backends* ())
(defvar *io-types* (make-hash-table :test 'eql))
(defvar *io-dispatchers* (make-hash-table :test 'equalp))

(define-condition binary-structures-condition (condition)
  ())

(define-condition no-such-io-type (error binary-structures-condition)
  ((designator :initarg :designator :reader designator))
  (:report (lambda (c s) (format s "No io-type with name~%  ~s" (designator c)))))

(define-condition end-of-storage (error binary-structures-condition)
  ((index :initarg :index :initform NIL :reader index)
   (end :initarg :end :initform NIL :reader end))
  (:report (lambda (c s) (format s "Failed to parse, as the storage backend ran out of space~@[~%at ~d (~:*~4x), trying to go beyond ~d (~:*~4x)~]"
                                 (index c) (end c)))))

(define-condition unknown-value (error binary-structures-condition)
  ((value :initarg :value :reader value)
   (accepted :initarg :accepted :reader accepted))
  (:report (lambda (c s) (format s "Encountered the value~%  ~a~%but it is not one of the accepted declared values~%  ~a"
                                 (value c) (accepted c)))))

(define-condition no-such-slot (error binary-structures-condition)
  ((name :initarg :name :reader name)
   (struct :initarg :struct :reader struct))
  (:report (lambda (c s) (format s "No slot with name~%  ~s~%found on~%  ~s"
                                 (name c) (struct c)))))

(defclass io-backend () 
  ((offset :initarg :offset :initform 0 :accessor offset)))

(defclass io-type () ())

(defgeneric read-defun (backend io-type &optional name))
(defgeneric write-defun (backend io-type &optional name))
(defgeneric call-read-form (backend io-type))
(defgeneric call-write-form (backend io-type value-variable))
(defgeneric read-form (backend io-type))
(defgeneric write-form (backend io-type value-variable))
(defgeneric index-form (backend &optional force-dynamic))
(defgeneric seek-form (backend offset))
(defgeneric lisp-type (io-type))
(defgeneric default-value (io-type))
(defgeneric octet-size (io-type))
(defgeneric octet-size-form (io-type value-variable))
(defgeneric initargs (io-type)
  (:method-combination append :most-specific-first))
(defgeneric parse-io-type (type &rest args))

(defmethod read-defun ((backend symbol) io-type &optional name)
  (read-defun (make-instance backend) io-type name))

(defmethod read-defun (backend (io-type T) &optional name)
  (read-defun backend (parse-io-type io-type) name))

(defmethod read-defun :before ((backend io-backend) io-type &optional name)
  (declare (ignore name))
  (setf (offset backend) 0))

(defmethod write-defun ((backend symbol) io-type &optional name)
  (write-defun (make-instance backend) io-type name))

(defmethod write-defun (backend (io-type T) &optional name)
  (write-defun backend (parse-io-type io-type) name))

(defmethod write-defun :before ((backend io-backend) io-type &optional name)
  (declare (ignore name))
  (setf (offset backend) 0))

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

(defmethod octet-size-form (type value-variable)
  (octet-size-form (parse-io-type type) value-variable))

(defmethod octet-size-form ((type io-type) value-variable)
  (octet-size type))

(defmethod index-form :around ((backend io-backend) &optional force-dynamic)
  (if (or force-dynamic (unspecific-p (offset backend)))
      (call-next-method)
      (offset backend)))

(defmethod seek-form :around ((backend io-backend) offset)
  (let ((current (offset backend)))
    (prog1 (cond ((eql '* offset)
                  NIL)
                 ((unspecific-p offset current)
                  (call-next-method))
                 (T
                  (let ((diff (- offset current)))
                    (unless (= 0 diff)
                      (call-next-method)))))
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
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (setf (io-type ',name) (make-instance ',type ,@initargs))))

(defun list-io-types ()
  (alexandria:hash-table-keys *io-types*))

(defun list-io-backends ()
  (copy-list *io-backends*))

(defmacro define-io-backend (name &optional (supers '(io-backend)) &rest slots)
  `(progn
     (defclass ,name ,supers ,slots)
     (pushnew ',name *io-backends*)
     ',name))

(defmacro define-io-dispatch (type lisp-type (value-var &rest runtime-args) &body body)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (setf (gethash (list ,type ',lisp-type) *io-dispatchers*)
           (lambda (,value-var)
             (values (progn ,@body) ',runtime-args)))))

(defun remove-io-dispatch (type lisp-type)
  (remhash (list type lisp-type) *io-dispatchers*))

(defmethod parse-io-type ((type io-type) &rest args)
  (apply #'make-instance (type-of type) (append args (initargs type))))

(defmethod parse-io-type ((type cons) &rest args)
  (apply #'parse-io-type (car type) (append (cdr type) args)))

(defmethod parse-io-type ((type symbol) &rest args)
  (apply #'parse-io-type (io-type type) args))

(defmacro define-io-type-parser (designator lambda-list &body body)
  (let ((args (gensym "ARGS")))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (defmethod parse-io-type ((type (eql ',designator)) &rest ,args)
         (destructuring-bind ,lambda-list ,args
           ,@body)))))

(defmacro define-io-backend-function (type io-backend io-type)
  (ecase type
    (:read (read-defun io-backend io-type))
    (:write (write-defun io-backend io-type))))

(defmacro define-io-dispatch-function (type io-type)
  (ecase type
    (:read
     `(defun ,(intern* type '- io-type) (storage &rest args)
        (etypecase storage
          ,@(loop for (generator-type storage-type) being the hash-keys of *io-dispatchers* using (hash-value generator)
                  for (body args) = (multiple-value-list (funcall generator io-type))
                  when (eql type generator-type)
                  collect `(,storage-type
                            (let ((,(first args) storage))
                              (destructuring-bind ,(rest args) args
                                ,body)))))))
    (:write
     `(defun ,(intern* type '- io-type) (value storage &rest args)
        (etypecase storage
          ,@(loop for (generator-type storage-type) being the hash-keys of *io-dispatchers* using (hash-value generator)
                  for (body args) = (multiple-value-list (funcall generator io-type))
                  when (eql type generator-type)
                  collect `(,storage-type
                            (let ((,(first args) value)
                                  (,(second args) storage))
                              (destructuring-bind ,(cddr args) args
                                ,body)))))))))

(defmacro define-io-functions (io-type)
  `(progn
     ,@(loop for backend in *io-backends*
             collect `(define-io-backend-function :read ,backend ,io-type)
             collect `(define-io-backend-function :write ,backend ,io-type))
     (define-io-dispatch-function :read ,io-type)
     (define-io-dispatch-function :write ,io-type)

     (defmethod octet-size ((value ,io-type))
       ,(octet-size-form io-type 'value))))

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
  "~:[U~;S~]B ~d ~a" signed-p (* 8 (octet-size io-integer)) order)

(defmethod default-value ((type io-integer))
  0)

(defmethod lisp-type ((type io-integer))
  (list (if (signed-p type) 'signed-byte 'unsigned-byte)
        (* 8 (octet-size type))))

(defmethod initargs append ((type io-integer))
  (list :order (order type) :signed-p (signed-p type)))

(define-io-type-parser integer (&optional (size 4) (signedness) (order :little-endian))
  (make-instance 'io-integer :octet-size size
                             :signed-p (ecase signedness
                                         ((:signed T) T)
                                         ((:unsigned NIL) NIL))
                             :order order))

(defclass io-boolean (io-integer)
  ((octet-size :initform 1)
   (signed-p :initform NIL)))

(defmethod default-value ((type io-boolean))
  NIL)

(defmethod lisp-type ((type io-boolean))
  'boolean)

(define-io-type-parser boolean (&optional (size 1) (signedness) (order :little-endian))
  (make-instance 'io-boolean :octet-size size
                             :signed-p (ecase signedness
                                         ((:signed T) T)
                                         ((:unsigned NIL) NIL))
                             :order order))

(defmethod read-form :around (backend (type io-boolean))
  `(< 0 (the ,(list (if (signed-p type) 'signed-byte 'unsigned-byte)
                    (* 8 (octet-size type)))
             ,(call-next-method))))

(defmethod write-form :around (backend (type io-boolean) value-variable)
  (call-next-method backend type `(if ,value-variable 1 0)))

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

(define-io-type-parser float (&optional (size 4))
  (make-instance 'io-float :octet-size size))

(defclass io-vector (io-type)
  ((element-type :initarg :element-type :initform 'uint8 :accessor element-type)
   (element-count :initarg :element-count :initform '* :accessor element-count)
   (element-offset :initarg :element-offset :initform NIL :accessor element-offset)
   (stride :initarg :stride :initform NIL :accessor stride)))

(define-print-object-method io-vector
  "~a ~a" element-type element-count)

(defmethod read-form ((backend io-backend) (type io-vector))
  (let ((vector (gensym "VECTOR")))
    `(let ((,vector (make-array ,(if (unspecific-p (element-count type))
                                     (read-form backend (element-count type))
                                     (element-count type))
                               :element-type ',(lisp-type (element-type type)))))
       (dotimes (i (length ,vector) ,vector)
         ,@(when (element-offset type)
             (list (seek-form backend (element-offset type))))
         (setf (aref ,vector i) ,(read-form backend (element-type type)))))))

(defmethod write-form ((backend io-backend) (type io-vector) value-variable)
  (let ((element (gensym "ELEMENT")))
    `(etypecase ,value-variable
       (simple-array
        (dotimes (i (length ,value-variable))
          (let ((,element (aref ,value-variable i)))
            ,(write-form backend (element-type type) element))))
       (array
        (dotimes (i (length ,value-variable))
          (let ((,element (aref ,value-variable i)))
            ,(write-form backend (element-type type) element)))))))

(defmethod read-form :around ((backend io-backend) (type io-vector))
  (let ((offset (offset backend)))
    (prog1 (call-next-method)
      (setf (offset backend) (or (unspecific-p offset (octet-size type))
                                 (+ offset (octet-size type)))))))

(defmethod write-form :around ((backend io-backend) (type io-vector) value-variable)
  (let ((offset (offset backend)))
    (prog1 (if (io-type (element-count type) NIL)
               ;; If the element-count was an immediate type, emit it.
               `(progn (let ((value (length ,value-variable)))
                         ,(write-form backend (element-count type) 'value))
                       ,(call-next-method))
               (call-next-method))
      (setf (offset backend) (or (unspecific-p offset (octet-size type))
                                 (+ offset (octet-size type)))))))

(defmethod lisp-type ((type io-vector))
  `(,(if (numberp (element-count type))
         'simple-array 'array)
    ,(lisp-type (element-type type))
    (,(if (numberp (element-count type))
          (element-count type)
          '*))))

(defmethod default-value ((type io-vector))
  `(make-array ,(if (numberp (element-count type))
                    (element-count type)
                    0)
               :element-type ',(lisp-type (element-type type))))

(defmethod octet-size ((type io-vector))
  (let ((count (element-count type)))
    (if (numberp count)
        (* count 
           (octet-size (element-type type)))
        '*)))

(defmethod octet-size-form ((type io-vector) value-variable)
  `(+ ,(if (symbolp (element-count type)) (octet-size-form (element-count type) value-variable) 0)
      (if (= 0 (length ,value-variable))
          0
          ,(if (unspecific-p (octet-size (element-type type)))
               (let ((var (gensym "EL")))
                 `(loop for ,var across ,value-variable
                        sum ,(octet-size-form (element-type type) var)))
               `(* (length ,value-variable)
                   ,(octet-size-form (element-type type) `(aref ,value-variable 0)))))))

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
   (null-terminated-p :initarg :null-terminated-p :initform T :accessor null-terminated-p)
   (encoding :initarg :encoding :initform :utf-8 :accessor encoding)))

(define-print-object-method io-string
  "~a ~a" encoding element-count)

(defmethod read-form ((backend io-backend) (type io-string))
  `(let* ((octet ,(read-form backend (make-instance 'io-vector :element-count (element-count type))))
          (end ,(if (null-terminated-p type)
                    `(string-length octet ,(encoding type))
                    0)))
     (babel:octets-to-string octet :encoding ,(encoding type) :end end)))

(defmethod write-form ((backend io-backend) (type io-string) value-variable)
  `(let ((octets ,(if (null-terminated-p type)
                      ;; KLUDGE: babel does not give us a way to write to a vector we allocated
                      ;;         nor a way to also have the null byte, so we use this disgusting
                      ;;         hack with a second, constant string that is nothing but the null.
                      `(babel:concatenate-strings-to-octets ,(encoding type)
                                                            ,value-variable
                                                            ,(load-time-value (string (code-char 0))))
                      `(babel:string-to-octets ,value-variable :encoding ,(encoding type)))))
     ,(write-form backend (make-instance 'io-vector :element-count (element-count type))
                  'octets)))

(defmethod lisp-type ((type io-string)) 'string)
(defmethod default-value ((type io-string)) "")

(defmethod octet-size ((type io-string))
  (let ((count (element-count type)))
    (if (numberp count) count '*)))

(defmethod octet-size-form ((type io-string) value-variable)
  `(+ (babel:string-size-in-octets ,value-variable :encoding ,(encoding type))
      ,(if (symbolp (element-count type)) (octet-size-form (element-count type) value-variable))
      ,(if (null-terminated-p type) 1 0)))

(defmethod initargs append ((type io-string))
  (list :encoding (encoding type)
        :null-terminated-p (null-terminated-p type)
        :element-count (element-count type)))

(define-io-type-parser string (&optional (element-count '*) (encoding :utf-8) (null-terminated-p T))
  (make-instance 'io-string :element-count element-count
                            :null-terminated-p null-terminated-p
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
                                     (symbol `(eq ,value ',test))
                                     (vector `(equalp ,value ,test)))
                                   (if (constantp form)
                                       form
                                       (read-form backend form))))
             (T (error 'unknown-value :value ,value :accepted ',(mapcar #'first (cases type))))))))

(defmethod write-form ((backend io-backend) (type io-case) value)
  (let ((start-offset (offset backend))
        (reduced-offsets NIL))
    (prog1 `(cond ,@(loop for (form test) in (cases type)
                          collect (list (etypecase test
                                          (number `(= ,value ,test))
                                          (character `(char= ,value ,test))
                                          (string `(string= ,value ,test))
                                          (vector `(equalp ,value ,test))
                                          ((or keyword (eql T) (eql NIL)) `(eq ,value ,test))
                                          (cons
                                           (if (eql 'quote (car test))
                                               `(eq ,value ,test)
                                               `(typep ,value ',(lisp-type test))))
                                          (symbol `(typep ,value ',(lisp-type test))))
                                        (progn
                                          ;; Restore the offset to the start to avoid offset
                                          ;; drifting for each case
                                          (setf (offset backend) start-offset)
                                          (write-form backend (value-type type) form))
                                        (prog1 (typecase test
                                                 ((and symbol (not (or keyword (eql T) (eql NIL))))
                                                  (write-form backend test value))
                                                 (cons
                                                  (unless (eql 'quote (car test))
                                                    (write-form backend test value))))
                                          ;; Reduce the potential offsets of the case values
                                          ;; to track the static offset
                                          (cond ((null reduced-offsets)
                                                 (setf reduced-offsets (offset backend)))
                                                ((unspecific-p reduced-offsets))
                                                ((not (equal reduced-offsets (offset backend)))
                                                 (setf reduced-offsets '*))))))
                  (T (error 'unknown-value :value ,value :accepted ',(mapcar #'first (cases type)))))
      (setf (offset backend) reduced-offsets))))

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
    (if add
        (or (unspecific-p size add) (+ size add))
        size)))

(defmethod octet-size-form ((type io-case) value-variable)
  `(+ ,(if (unspecific-p (octet-size (value-type type)))
           0 (octet-size (value-type type)))
      (if (typep ,value-variable 'io-structure-object)
          (octet-size ,value-variable)
          0)))

(define-io-type-parser case (value-type &rest cases)
  (make-instance 'io-case :value-type value-type
                          :cases cases))

(defclass io-value (io-type)
  ((form :initarg :form :accessor form)))

(defmethod read-form ((backend io-backend) (type io-value))
  (form type))

(defmethod write-form ((backend io-backend) (type io-value) value)
  (form type))

(defmethod lisp-type ((type io-value)) T)
(defmethod default-value ((type io-value)) NIL)
(defmethod octet-size ((type io-value)) 0)
(defmethod octet-size-form ((type io-value) value-variable) 0)

(defmethod parse-io-type ((type cons) &rest args)
  (handler-case (apply #'parse-io-type (car type) (append (cdr type) args))
    (no-such-io-type ()
      (make-instance 'io-value :form (append type args)))))

(defclass io-typecase (io-value)
  ((cases :initarg :cases :accessor cases)))

(defmethod read-form ((backend io-backend) (type io-typecase))
  `(typecase ,(read-form backend (form type))
     ,@(loop for (type form) in (cases type)
             collect (list type
                           (if (constantp form)
                               form
                               (read-form backend form))))))

(defmethod write-form ((backend io-backend) (type io-typecase) value)
  `(typecase ,(read-form backend (form type))
     ,@(loop for (type form) in (cases type)
             collect (list type
                           (if (constantp form)
                               form
                               (write-form backend form value))))))

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

(defmethod octet-size-form ((type io-typecase) value-variable)
  `(octet-size ,value-variable))

(define-io-type-parser typecase (form &rest cases)
  (make-instance 'io-typecase :form form :cases cases))

(defvar *io-structure-inline* T)

(defclass io-structure (io-type)
  ((value-type :initarg :value-type :accessor value-type)
   (constructor :initarg :constructor :accessor constructor)
   (slots :initarg :slots :initform () :accessor slots)
   (name :initarg :name :initform NIL :accessor name)
   (size-form :initarg :size-form :initarg :octet-size :initform NIL :accessor size-form)))

(define-print-object-method io-structure
  "~a" value-type)

(defmethod describe-object :after ((structure io-structure) stream)
  (format stream "~&~%ADDRESS  SIZE TYPE     NAME")
  (loop for slot in (slots structure)
        do (format stream "~%~:[        ~;~:*~8,'0x~] ~4a ~8a ~a"
                   (unless (unspecific-p (offset slot))
                     (offset slot))
                   (octet-size slot)
                   (truncate-text (princ-to-string (value-type slot)) 8)
                   (or (name slot) "----"))))

(defmethod read-form :around ((backend io-backend) (type io-structure))
  (ecase *io-structure-inline*
    ((NIL)
     (call-read-form backend type))
    ((T :always)
     (let ((*io-structure-inline* (if (eq *io-structure-inline* :always) :always NIL)))
      (call-next-method)))))

(defmethod write-form :around ((backend io-backend) (type io-structure) value-variable)
  (ecase *io-structure-inline*
    ((NIL)
     (call-write-form backend type value-variable))
    ((T :always)
     (let ((*io-structure-inline* (if (eq *io-structure-inline* :always) :always NIL)))
       (call-next-method)))))

(defun %slot-macrolet (type value &rest body)
  `(macrolet ((slot (name &rest descendants)
                (let ((value (if (symbolp name)
                                 (list (intern* ',(lisp-type type) '- name) ',value)
                                 name)))
                  (dolist (name descendants value)
                    (setf value `(slot-value ,value ',name))))))
     (symbol-macrolet ((instance ,value))
       ,@body)))

(defmethod read-form ((backend io-backend) (type io-structure))
  (let ((value (gensym "VALUE")))
    `(let ((relative-offset ,(index-form backend T))
           (,value ,(default-value type)))
       ,(apply #'%slot-macrolet type value
               (loop for slot in (slots type)
                     for align = (seek-form backend (offset slot))
                     when align collect align
                     collect (if (typep slot 'io-structure-magic)
                                 (read-form backend slot)
                                 `(setf (,(intern* (lisp-type type) '- (name slot)) ,value)
                                        ,(read-form backend slot)))))
       ,value)))

(defmethod write-form ((backend io-backend) (type io-structure) value-variable)
  (let ((value (gensym "VALUE")))
    `(let ((relative-offset ,(index-form backend T)))
       ,(apply #'%slot-macrolet type value-variable
               (loop for slot in (slots type)
                     for align = (seek-form backend (offset slot))
                     when align collect align
                     collect (if (typep slot 'io-structure-magic)
                                 (write-form backend slot value-variable)
                                 `(let ((,value (,(intern* (lisp-type type) '- (name slot)) ,value-variable)))
                                    ,(write-form backend slot value))))))))

(defmethod lisp-type ((type io-structure))
  (value-type type))

(defmethod default-value ((type io-structure))
  (list (constructor type)))

(defmethod octet-size ((type io-structure))
  (let ((last (car (last (slots type)))))
    (or (unspecific-p (offset last) (octet-size last))
        (+ (offset last)
           (octet-size last)))))

(defmethod octet-size-form ((type io-structure) value-variable)
  (if (size-form type)
      (%slot-macrolet type value-variable
                      (size-form type))
      (let ((slots (slots type)))
        ;; Combine the most specific offset with the dynamic sizes of unspecific slots
        `(+ ,@(loop with last = ()
                    do (when (or (null slots) (unspecific-p (offset (car slots)) (octet-size (car slots))))
                         (return last))
                       (let ((slot (pop slots)))
                         (setf last (list (offset slot)
                                          (octet-size slot)))))
            ,@(loop for slot in slots
                    collect (octet-size-form slot (list (intern* (value-type type) '- (name slot)) value-variable)))))))

(defmethod initargs append ((type io-structure))
  (list :value-type (value-type type)
        :constructor (constructor type)
        :slots (slots type)
        :size-form (size-form type)))

(defmethod find-slot (name (structure io-structure))
  (or (find name (slots structure) :key #'name)
      (error 'no-such-slot :name name :struct structure)))

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

(defmethod read-form ((backend io-backend) (type io-structure-slot))
  (read-form backend (value-type type)))

(defmethod write-form ((backend io-backend) (type io-structure-slot) value-variable)
  (write-form backend (value-type type) value-variable))

(defmethod octet-size-form ((slot io-structure-slot) value-variable)
  (octet-size-form (value-type slot) value-variable))

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
            (declare (dynamic-extent value))
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

(defun parse-io-structure-slots (defs)
  (let ((slots ())
        (total-offset 0))
    (flet ((finish (slot &optional (pad 0))
             (setf total-offset (or (unspecific-p (octet-size slot) (offset slot))
                                    (+ (octet-size slot) (offset slot) pad)))
             (push slot slots)))
      (dolist (slot defs (nreverse slots))
        (etypecase slot
          (cons
           (destructuring-bind (name type &key (offset total-offset) (size (octet-size type)) (align 1) (pad 0)) slot
             (case name
               (:include
                (dolist (slot (slots (io-type type)))
                  (finish slot)))
               ((NIL)
                (finish (make-instance 'io-structure-magic
                                       :value-type type
                                       :octet-size size
                                       :offset (or (unspecific-p offset)
                                                   (* (ceiling offset align) align)))))
               (T
                ;; FIXME: The alignment and padding are discarded as soon as we enter UNSPECIFIC territory.
                ;;        That is obviously pretty dang bad.
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
                                  :name (princ-to-string slot)
                                  :default-value slot
                                  :octet-size (length slot)
                                  :offset total-offset))))))))

(defstruct io-structure-object)

(defmacro define-io-structure (name &body slots)
  (destructuring-bind (name &rest struct-args) (if (listp name) name (list name))
    (form-fiddle:with-body-options (slots io-type-options) slots
      (let ((constructor (or (second (assoc :constructor struct-args)) (intern* 'make- name)))
            (include (when (and (listp (first slots)) (eql :include (caar slots)))
                       (pop slots)))
            (slotdefs ())
            (struct-args (remove (assoc :constructor struct-args) struct-args)))
        (handler-bind ((no-such-io-type #'continue))
          (dolist (slot slots)
            (when (consp slot)
              (case (first slot)
                (:include
                 (dolist (slot (slots (io-type (second slot))))
                   (push `(,(name slot) ,(default-value slot) :type ,(lisp-type slot))
                         slotdefs)))
                ((NIL))
                (T
                 (push `(,(first slot) ,(default-value (second slot)) :type ,(lisp-type (second slot))) 
                       slotdefs)))))
          `(progn
             (defstruct (,name
                         (:constructor ,constructor)
                         (:include ,(if include (second include) 'io-structure-object))
                         ,@struct-args)
               ,@(nreverse slotdefs))

             (define-io-type (io-structure ,name)
               :name ',name
               :value-type ',name
               :constructor ',constructor
               :slots (parse-io-structure-slots ',(if include (list* include slots) slots))
               ,@(loop for (k v) on io-type-options by #'cddr
                       collect k collect `',v))

             (define-io-functions ,name)))))))

(defclass bounds-checked-io-backend (io-backend)
  ())

(defmethod read-form ((backend bounds-checked-io-backend) (type io-structure))
  (let ((minimum (loop for slot in (slots type)
                       for size = (octet-size slot)
                       until (unspecific-p size)
                       sum size)))
    (if (< 0 minimum)
        `(progn (check-available-space ,minimum)
                ,(call-next-method))
        (call-next-method))))

(defmethod write-form ((backend bounds-checked-io-backend) (type io-structure) value-variable)
  (let ((minimum (loop for slot in (slots type)
                       for size = (octet-size slot)
                       until (unspecific-p size)
                       sum size)))
    (if (< 0 minimum)
        `(progn (check-available-space ,minimum)
                ,(call-next-method))
        (call-next-method))))

(defmethod read-form ((backend bounds-checked-io-backend) (type io-vector))
  ;; KLUDGE: Sad, we mostly copy the entire thing just to emit the check form after allocating the vector.
  (let ((vector (gensym "VECTOR")))
    `(let ((,vector (make-array ,(cond ((not (unspecific-p (element-count type)))
                                        (element-count type))
                                       ((eql '* (element-count type))
                                        `(truncate (available-space) ,(octet-size (element-type type))))
                                       (T
                                        (read-form backend (element-count type))))
                                :element-type ',(lisp-type (element-type type)))))
       (declare (optimize #+sbcl (sb-c::insert-array-bounds-checks 0)))
       ,@(unless (or (element-offset type)
                     (eql '* (element-count type))
                     (typep (io-type (element-type type) NIL) '(or null io-structure)))
           `((check-available-space (* (length ,vector) ,(or (stride type) (octet-size (element-type type)))))))
       (dotimes (i (length ,vector) ,vector)
         ,@(when (element-offset type)
             (list (seek-form backend (element-offset type))))
         (setf (aref ,vector i) ,(read-form backend (element-type type)))))))

(defmethod write-form ((backend bounds-checked-io-backend) (type io-vector) value-variable)
  ;; KLUDGE: Sad, we mostly copy the entire thing just to emit the check form after allocating the vector.
  (let ((element (gensym "ELEMENT")))
    `(locally (declare (optimize #+sbcl (sb-c::insert-array-bounds-checks 0)))
       ,@(unless (or (element-offset type)
                     (typep (io-type (element-type type) NIL) '(or null io-structure)))
           `((check-available-space (* (length ,value-variable) ,(or (stride type) (octet-size (element-type type)))))))
       (dotimes (i (length ,value-variable))
         (let ((,element (aref ,value-variable i)))
           ,(write-form backend (element-type type) element))))))

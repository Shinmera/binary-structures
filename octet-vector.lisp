(in-package #:org.shirakumo.binary-structures)

(define-io-backend io-octet-vector (bounds-checked-io-backend))

(define-io-dispatch :read (simple-array (unsigned-byte 8) (*)) (type storage &key (start 0) (end (length storage)))
  `(progn
     (check-type start index)
     (check-type end index)
     (,(intern* 'read-io-octet-vector- (lisp-type type)) storage start end)))

(define-io-dispatch :write (simple-array (unsigned-byte 8) (*)) (type value storage &key (start 0) (end (length storage)))
  `(progn
     (check-type start index)
     (check-type end index)
     (,(intern* 'write-io-octet-vector- (lisp-type type)) value storage start end)))

(define-io-dispatch :read (array (unsigned-byte 8) (*)) (type storage &key (start 0) (end (length storage)))
  `(let ((array (make-array (- end start) :element-type '(unsigned-byte 8))))
     (declare (dynamic-extent array))
     (replace array storage :start2 start :end2 end)
     (,(intern* 'read-io-octet-vector- (lisp-type type)) array 0 (length array))))

(define-io-dispatch :write (array (unsigned-byte 8) (*)) (type value storage &key (start 0) (end (length storage)))
  `(let ((array (make-array (- end start) :element-type '(unsigned-byte 8))))
     (declare (dynamic-extent array))
     (replace array storage :start2 start :end2 end)
     (,(intern* 'write-io-octet-vector- (lisp-type type)) value array 0 (length array))))

(defmethod read-defun ((backend io-octet-vector) (type io-type) &optional name)
  `(define-typed-function ,(or name (intern* 'read- (type-of backend) '- (lisp-type type)))
       ((vector (simple-array (unsigned-byte 8) (*))) (start index) (end index))
       (values ,(lisp-type type) index &optional)
     (declare (ignorable end))
     (let ((index start))
       (declare (type index index))
       (flet ((check-available-space (space)
                (when (< end (the index (+ space index)))
                  (error 'end-of-storage :index index :end end)))
              (available-space ()
                (the index (- end index))))
         (declare (ignorable #'check-available-space #'available-space) (inline check-available-space available-space))
         (values ,(read-form backend type)
                 index)))))

(defmethod write-defun ((backend io-octet-vector) (type io-type) &optional name)
  `(define-typed-function ,(or name (intern* 'write- (type-of backend) '- (lisp-type type)))
       ((value ,(lisp-type type)) (vector (simple-array (unsigned-byte 8) (*))) (start index) (end index))
       (values index &optional)
     (declare (ignorable end))
     (let ((index start))
       (declare (type index index))
       (flet ((check-available-space (space)
                (when (< end (the index (+ space index)))
                  (error 'end-of-storage :index index :end end)))
              (available-space ()
                (the index (- end index))))
         (declare (ignorable #'check-available-space #'available-space) (inline check-available-space available-space))
         ,(write-form backend type 'value))
       index)))

(defmethod call-read-form ((backend io-octet-vector) (type io-type))
  `(multiple-value-bind (value new-index) (,(intern* 'read- (type-of backend) '- (lisp-type type))
                                           vector index end)
     (declare (type index new-index))
     (setf index new-index)
     value))

(defmethod call-write-form ((backend io-octet-vector) (type io-type) value-variable)
  `(setf index (,(intern* 'write- (type-of backend) '- (lisp-type type))
                ,value-variable vector index end)))

(declaim (inline sb-aref (setf sb-aref)))
(defun sb-aref (array index)
  (let ((byte (aref array index)))
    (logior byte (- (mask-field (byte 1 7) byte)))))

(defun (setf sb-aref) (value array index)
  (setf (aref array index) (logand #xFF value)))

(defmethod read-form ((backend io-octet-vector) (type io-integer))
  `(prog1 (,(if (= 1 (octet-size type))
                (if (signed-p type)
                    'sb-aref
                    'aref)
                (find-symbol* 'nibbles 
                              (if (signed-p type) 'sb 'ub)
                              (* 8 (octet-size type))
                              'ref
                              (ecase (order type)
                                (:little-endian '/le)
                                (:big-endian '/be))))
           vector index)
     (setf index (the index (+ index ,(octet-size type))))))

(defmethod write-form ((backend io-octet-vector) (type io-integer) value-variable)
  `(progn
     (setf (,(if (= 1 (octet-size type))
                 (if (signed-p type)
                     'sb-aref
                     'aref)
                 (find-symbol* 'nibbles
                               (if (signed-p type) 'sb 'ub)
                               (* 8 (octet-size type))
                               'ref
                               (ecase (order type)
                                 (:little-endian '/le)
                                 (:big-endian '/be))))
            vector index) ,value-variable)
     (setf index (the index (+ index ,(octet-size type))))))

(defmethod read-form ((backend io-octet-vector) (type io-float))
  `(prog1 (,(find-symbol* 'nibbles 'ieee-
                          (ecase (octet-size type)
                            (4 'single)
                            (8 'double))
                          '-ref
                          (ecase (order type)
                            (:little-endian '/le)
                            (:big-endian '/be)))
           vector index)
     (setf index (the index (+ index ,(octet-size type))))))

(defmethod write-form ((backend io-octet-vector) (type io-float) value-variable)
  `(progn
     (setf (,(find-symbol* 'nibbles 'ieee-
                           (ecase (octet-size type)
                             (4 'single)
                             (8 'double))
                           '-ref
                           (ecase (order type)
                             (:little-endian '/le)
                             (:big-endian '/be)))
            vector index) ,value-variable)
     (setf index (the index (+ index ,(octet-size type))))))

(defmethod read-form ((backend io-octet-vector) (type io-vector))
  (if (equalp '(unsigned-byte 8) (lisp-type (element-type type)))
      `(let ((array (make-array ,(cond ((not (unspecific-p (element-count type)))
                                        (element-count type))
                                       ((eql '* (element-count type))
                                        `(truncate (available-space) ,(octet-size (element-type type))))
                                       (T
                                        (read-form backend (element-count type))))
                                :element-type '(unsigned-byte 8))))
         (declare (optimize #+sbcl (sb-c::insert-array-bounds-checks 0)))
         ,@(unless (eql '* (element-count type)) `((check-available-space (length array))))
         (replace array vector :start2 index)
         (setf index (the index (+ index (length array))))
         array)
      (call-next-method)))

(defmethod write-form ((backend io-octet-vector) (type io-vector) value-variable)
  (if (equalp '(unsigned-byte 8) (lisp-type (element-type type)))
      `(locally (declare (optimize #+sbcl (sb-c::insert-array-bounds-checks 0)))
         (check-available-space (length ,value-variable))
         (replace vector ,value-variable :start1 index)
         (setf index (the index (+ index (length ,value-variable)))))
      (call-next-method)))

(defmethod index-form ((backend io-octet-vector) &optional force-dynamic)
  (declare (ignore force-dynamic))
  'index)

(defmethod seek-form ((backend io-octet-vector) offset)
  `(let ((new (+ relative-offset ,offset)))
     (declare (type index new))
     (if (< new end)
         (setf index new)
         (error 'end-of-storage :index index :end end))))

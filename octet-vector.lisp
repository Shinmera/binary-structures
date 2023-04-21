#|
 This file is a part of binary-structures
 (c) 2023 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.binary-structures)

(define-io-backend io-octet-vector (bounds-checked-io-backend))

(defmethod read-defun ((backend io-octet-vector) (type io-type))
  `(define-typed-function ,(intern* 'read- (type-of backend) '- (lisp-type type)) 
       ((vector (simple-array (unsigned-byte 8) (*))) (start (unsigned-byte 64)) (end (unsigned-byte 64)))
       (values ,(lisp-type type) (unsigned-byte 64))
     (declare (ignorable end))
     (let ((index start))
       (flet ((check-available-space (space)
                (when (< end (+ space index))
                  (error 'end-of-storage :index index :end end))))
         (declare (ignorable #'check-available-space) (inline check-available-space))
         (values ,(read-form backend type)
                 index)))))

(defmethod write-defun ((backend io-octet-vector) (type io-type))
  `(define-typed-function ,(intern* 'write- (type-of backend) '- (lisp-type type))
       ((value ,(lisp-type type)) (vector (simple-array (unsigned-byte 8) (*))) (start (unsigned-byte 64)) (end (unsigned-byte 64)))
       (unsigned-byte 64)
     (declare (ignorable end))
     (let ((index start))
       (flet ((check-available-space (space)
                (when (< end (+ space index))
                  (error 'end-of-storage :index index :end end))))
         (declare (ignorable #'check-available-space) (inline check-available-space))
         ,(write-form backend type 'value))
       index)))

(defmethod call-read-form ((backend io-octet-vector) (type io-type))
  `(multiple-value-bind (value new-index) (,(intern* 'read- (type-of backend) '- (lisp-type type))
                                           vector index end)
     (setf index new-index)
     value))

(defmethod call-write-form ((backend io-octet-vector) (type io-type) value-variable)
  `(setf index (,(intern* 'write- (type-of backend) '- (lisp-type type))
                ,value-variable vector index end)))

(defmethod read-form ((backend io-octet-vector) (type io-integer))
  `(prog1 (,(if (= 1 (octet-size type))
                'aref
                (find-symbol* 'nibbles 
                              (if (signed-p type) 'sb 'ub)
                              (* 8 (octet-size type))
                              'ref
                              (ecase (order type)
                                (:little-endian '/le)
                                (:big-endian '/be))))
           vector index)
     (incf index ,(octet-size type))))

(defmethod write-form ((backend io-octet-vector) (type io-integer) value-variable)
  `(progn
     (setf (,(if (= 1 (octet-size type))
                 'aref
                 (find-symbol* 'nibbles
                               (if (signed-p type) 'sb 'ub)
                               (* 8 (octet-size type))
                               'ref
                               (ecase (order type)
                                 (:little-endian '/le)
                                 (:big-endian '/be))))
            vector index) ,value-variable)
     (incf index ,(octet-size type))))

(defmethod read-form ((backend io-octet-vector) (type io-float))
  `(prog1 (,(find-symbol* 'nibbles 'ieee-
                          (ecase (octet-size type)
                            (4 'single)
                            (8 'double))
                          'ref
                          (ecase (order type)
                            (:little-endian '/le)
                            (:big-endian '/be)))
           vector index)
     (incf index ,(octet-size type))))

(defmethod write-form ((backend io-octet-vector) (type io-float) value-variable)
  `(progn
     (setf (,(find-symbol* 'nibbles 'ieee-
                           (ecase (octet-size type)
                             (4 'single)
                             (8 'double))
                           'ref
                           (ecase (order type)
                             (:little-endian '/le)
                             (:big-endian '/be)))
            vector index) ,value-variable)
     (incf index ,(octet-size type))))

(defmethod read-form ((backend io-octet-vector) (type io-vector))
  (if (equalp '(unsigned-byte 8) (lisp-type (element-type type)))
      `(let ((array (make-array ,(read-form backend (element-count type)) :element-type '(unsigned-byte 8))))
         (declare (optimize #+sbcl (sb-c::insert-array-bounds-checks 0)))
         (check-available-space (length array))
         (replace array vector :start2 index)
         (incf index (length array))
         array)
      (call-next-method)))

(defmethod write-form ((backend io-octet-vector) (type io-vector) value-variable)
  (if (equalp '(unsigned-byte 8) (lisp-type (element-type type)))
      `(locally (declare (optimize #+sbcl (sb-c::insert-array-bounds-checks 0)))
         (check-available-space (length ,value-variable))
         (replace vector ,value-variable :start1 index)
         (incf index (length ,value-variable)))
      (call-next-method)))

(defmethod index-form ((backend io-octet-vector))
  'index)

(defmethod seek-form ((backend io-octet-vector) offset)
  `(let ((new (+ start ,offset)))
     (if (< new end)
         (setf index new)
         (error 'end-of-storage :index index :end end))))

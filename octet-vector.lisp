#|
 This file is a part of binary-structures
 (c) 2023 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.binary-structures)

(define-io-backend io-octet-vector)

(defmethod read-defun ((backend io-octet-vector) type name)
  `(defun ,name (vector &key (start 0) (end (length vector)))
     (let ((index start))
       ,(read-form backend type))))

(defmethod write-defun ((backend io-octet-vector) type name)
  `(defun ,name (value vector &key (start 0) (end (length vector)))
     (let ((index start))
       ,(write-form backend type 'value))))

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
  (if (eql 1 (octet-size (element-type type)))
      `(let ((array (subseq vector index ,(read-form backend (element-count type))))) 
         (incf index (length array))
         array)
      (call-next-method)))

(defmethod write-form ((backend io-octet-vector) (type io-vector) value-variable)
  (if (eql 1 (octet-size (element-type type)))
      `(progn (replace vector ,value-variable :start1 index)
              (incf index (length ,value-variable)))
      (call-next-method)))

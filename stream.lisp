#|
 This file is a part of binary-structures
 (c) 2023 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.binary-structures)

(define-io-backend io-stream)

(defmethod read-defun ((backend io-stream) type name)
  `(defun ,name (stream)
     ,(read-form backend type)))

(defmethod write-defun ((backend io-stream) type name)
  `(defun ,name (value stream)
     ,(write-form backend type 'value)))

(defmethod read-form ((backend io-stream) (type io-integer))
  `(,(if (= 1 (octet-size type))
         'read-byte
         (find-symbol* 'nibbles 'read- 
                       (if (signed-p type) 'sb 'ub)
                       (* 8 (octet-size type))
                       (ecase (order type)
                         (:little-endian '/le)
                         (:big-endian '/be))))
    stream))

(defmethod write-form ((backend io-stream) (type io-integer) value-variable)
  `(,(if (= 1 (octet-size type))
         'write-byte
         (find-symbol* 'nibbles 'write- 
                       (if (signed-p type) 'sb 'ub)
                       (* 8 (octet-size type))
                       (ecase (order type)
                         (:little-endian '/le)
                         (:big-endian '/be))))
    ,value-variable
    stream))

(defmethod read-form ((backend io-stream) (type io-float))
  `(,(find-symbol* 'nibbles 'read-ieee-
                   (ecase (octet-size type)
                     (4 'single)
                     (8 'double))
                   (ecase (order type)
                     (:little-endian '/le)
                     (:big-endian '/be)))
    stream))

(defmethod write-form ((backend io-stream) (type io-float) value-variable)
  `(,(find-symbol* 'nibbles 'write-ieee-
                   (ecase (octet-size type)
                     (4 'single)
                     (8 'double))
                   (ecase (order type)
                     (:little-endian '/le)
                     (:big-endian '/be)))
    ,value-variable
    stream))

(defmethod read-form ((backend io-stream) (type io-vector))
  (if (eql 1 (octet-size (element-type type)))
      `(let ((array (make-array ,(read-form backend (element-count type)) :element-type '(unsigned-byte 8))))
         (read-sequence array stream)
         array)
      (call-next-method)))

(defmethod write-form ((backend io-stream) (type io-vector) value-variable)
  (if (eql 1 (octet-size (element-type type)))
      `(write-sequence ,value-variable stream)
      (call-next-method)))

(defmethod index-form ((backend io-stream))
  `(file-position stream))

(defmethod seek-form ((backend io-stream) offset)
  `(let ((diff (- ,offset ,(index-form backend))))
     (cond ((= 0 diff))
           ((< 8 diff)
            (let ((scratch (make-array diff :element-type '(unsigned-byte 8))))
              (declare (dynamic-extent scratch))
              (read-sequence scratch stream)))
           ((< 0 diff)
            (loop repeat diff do (read-byte stream)))
           (T
            (file-position stream ,offset)))))

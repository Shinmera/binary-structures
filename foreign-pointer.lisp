#|
 This file is a part of binary-structures
 (c) 2023 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.binary-structures)

(define-io-backend io-foreign-pointer)

(defmacro read-mem (pointer type &optional (octets `(cffi:foreign-type-size ,type)))
  `(prog1 (cffi:mem-ref ,pointer ,type)
    (cffi:incf-pointer ,pointer ,octets)))

(defmacro write-mem (value pointer type &optional (octets `(cffi:foreign-type-size ,type)))
  `(prog1 (setf (cffi:mem-ref ,pointer ,type) ,value)
     (cffi:incf-pointer ,pointer ,octets)))

(defmethod read-defun ((backend io-foreign-pointer) type name)
  `(defun ,name (pointer &key size)
     (check-type size (integer 0))
     (let ((start pointer))
       ,(read-form backend type))))

(defmethod write-defun ((backend io-foreign-pointer) type name)
  `(defun ,name (value pointer &key size)
     (check-type size (integer 0))
     (let ((start pointer))
       ,(write-form backend type 'value))))

(defmethod read-form ((backend io-foreign-pointer) (type io-integer))
  `(read-mem pointer
       ,(find-symbol* 'keyword (if (signed-p type) 'u '||) 'int
                      (* 8 (octet-size type)))
       ,(octet-size type)))

(defmethod write-form ((backend io-foreign-pointer) (type io-integer) value-variable)
  `(write-mem ,value-variable
              pointer
              ,(find-symbol* 'keyword (if (signed-p type) 'u '||) 'int
                             (* 8 (octet-size type)))
              ,(octet-size type)))

(defmethod read-form ((backend io-foreign-pointer) (type io-float))
  `(read-mem pointer 
       ,(ecase (octet-size type)
          (2 :half-float)
          (4 :float)
          (8 :double)
          (16 :long-double))
       ,(octet-size type)))

(defmethod write-form ((backend io-foreign-pointer) (type io-float) value-variable)
  `(write-pointer ,value-variable
                  pointer
                  ,(ecase (octet-size type)
                     (2 :half-float)
                     (4 :float)
                     (8 :double)
                     (16 :long-double))
                  ,(octet-size type)))

(defmethod index-form ((backend io-foreign-pointer))
  `(- (cffi:pointer-address pointer)
      (cffi:pointer-address start)))

(defmethod seek-form ((backend io-foreign-pointer) offset)
  `(setf pointer (cffi:inc-pointer start ,offset)))

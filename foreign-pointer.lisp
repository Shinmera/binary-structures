#|
 This file is a part of binary-structures
 (c) 2023 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.binary-structures)

(define-io-backend io-foreign-pointer)

(defmethod read-defun ((backend io-foreign-pointer) type name)
  `(defun ,name (pointer &key size)
     (check-type size (integer 0))
     ,(read-form backend type)))

(defmethod write-defun ((backend io-foreign-pointer) type name)
  `(defun ,name (value pointer &key size)
     (check-type size (integer 0))
     ,(write-form backend type 'value)))

(defmethod read-form ((backend io-foreign-pointer) (type io-integer))
  `(prog1 (cffi:mem-ref pointer ,(find-symbol* 'keyword (if (signed-p type) 'u '||) 'int
                                               (* 8 (octet-size type))))
     (cffi:incf-pointer pointer ,(octet-size type))))

(defmethod write-form ((backend io-foreign-pointer) (type io-integer) value-variable)
  `(progn
     (setf (cffi:mem-ref pointer ,(find-symbol* 'keyword (if (signed-p type) 'u '||) 'int
                                                (* 8 (octet-size type))))
           ,value-variable)
     (cffi:incf-pointer pointer ,(octet-size type))))

(defmethod read-form ((backend io-foreign-pointer) (type io-float))
  `(prog1 (cffi:mem-ref pointer ,(ecase (octet-size type)
                                   (2 :half-float)
                                   (4 :float)
                                   (8 :double)
                                   (16 :long-double)))
     (cffi:incf-pointer pointer ,(octet-size type))))

(defmethod write-form ((backend io-foreign-pointer) (type io-float) value-variable)
  `(progn
     (setf (cffi:mem-ref pointer ,(ecase (octet-size type)
                                    (2 :half-float)
                                    (4 :float)
                                    (8 :double)
                                    (16 :long-double)))
           ,value-variable)
     (cffi:incf-pointer pointer ,(octet-size type))))

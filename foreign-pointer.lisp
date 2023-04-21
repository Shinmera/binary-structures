#|
 This file is a part of binary-structures
 (c) 2023 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.binary-structures)

(define-io-backend io-foreign-pointer (bounds-checked-io-backend))

(defmacro read-mem (pointer type &optional (octets `(cffi:foreign-type-size ,type)))
  `(prog1 (cffi:mem-ref ,pointer ,type)
    (cffi:incf-pointer ,pointer ,octets)))

(defmacro write-mem (value pointer type &optional (octets `(cffi:foreign-type-size ,type)))
  `(prog1 (setf (cffi:mem-ref ,pointer ,type) ,value)
     (cffi:incf-pointer ,pointer ,octets)))

(defmethod read-defun ((backend io-foreign-pointer) (type io-type))
  `(define-typed-function ,(intern* 'read- (type-of backend) '- (lisp-type type))
       ((pointer cffi:foreign-pointer) (size (unsigned-byte 64)))
       (values ,(lisp-type type) cffi:foreign-pointer)
     (declare (ignorable size))
     (let ((start pointer)
           (pointer pointer))
       (declare (type cffi:foreign-pointer start pointer))
       (declare (ignorable start))
       (flet ((check-available-space (space)
                (when (< (- size (- (cffi:pointer-address pointer) (cffi:pointer-address start))) space)
                  (error "EOF"))))
         (declare (ignorable #'check-available-space) (inline check-available-space))
         (values ,(read-form backend type)
                 pointer)))))

(defmethod write-defun ((backend io-foreign-pointer) (type io-type))
  `(define-typed-function ,(intern* 'write- (type-of backend) '- (lisp-type type)) 
       ((value ,(lisp-type type)) (pointer cffi:foreign-pointer) (size (unsigned-byte 64)))
       cffi:foreign-pointer
     (declare (ignorable size))
     (let ((start pointer)
           (pointer pointer))
       (declare (type cffi:foreign-pointer start pointer))
       (declare (ignorable start))
       (flet ((check-available-space (space)
                (when (< (- size (- (cffi:pointer-address pointer) (cffi:pointer-address start))) space)
                  (error "EOF"))))
         (declare (ignorable #'check-available-space))
         ,(write-form backend type 'value))
       pointer)))

(defmethod call-read-form ((backend io-foreign-pointer) (type io-type))
  `(multiple-value-bind (value new-pointer) (,(intern* 'read- (type-of backend) '- (lisp-type type))
                                             pointer (- size (- (cffi:pointer-address pointer) (cffi:pointer-address start))))
     (setf pointer new-pointer)
     value))

(defmethod call-write-form ((backend io-foreign-pointer) (type io-type) value-variable)
  `(setf pointer (,(intern* 'write- (type-of backend) '- (lisp-type type))
                  ,value-variable pointer (- size (- (cffi:pointer-address pointer) (cffi:pointer-address start))))))

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
  `(if (<= ,offset size)
       (setf pointer (cffi:inc-pointer start ,offset))
       (error "EOF")))

(in-package #:org.shirakumo.binary-structures)

(defun intern* (&rest args)
  (intern (with-standard-io-syntax (format NIL "~{~a~^~}" args))))

(defun find-symbol* (package &rest args)
  (or (find-symbol (with-standard-io-syntax (format NIL "~{~a~^~}" args)) package)
      (error "No symbol named ~{~a~^~} on ~a" args package)))

(defmacro define-print-object-method (type format &rest args)
  `(defmethod print-object ((,type ,type) stream)
     (print-unreadable-object (,type stream :type T)
       (format stream ,format ,@(loop for arg in args
                                      collect (if (listp arg) arg `(,arg ,type)))))))

(defun truncate-text (text max-length)
  (if (< max-length (length text))
      (subseq text 0 max-length)
      text))

(defun unspecific-p (&rest things)
  (loop for thing in things
        do (cond ((null thing) (return '*))
                 ((not (numberp thing)) (return thing)))))

(defmacro define-typed-function (name args retval &body body)
  `(progn
     (declaim (ftype (function ,(loop for arg in args
                                      collect (cond ((listp arg) (second arg))
                                                    ((find arg LAMBDA-LIST-KEYWORDS) arg)
                                                    (T T)))
                               ,retval)
                     ,name))
     (defun ,name ,(loop for arg in args collect (if (listp arg) (first arg) arg))
       (declare (optimize speed (safety 1) (space 0)))
       ,@body)))

(defun reexport (symb pkg)
  (import symb pkg)
  (export symb pkg))

(deftype index ()
  `(integer 0 ,(1- ARRAY-DIMENSION-LIMIT)))

(defun string-length (octets &optional (encoding :utf-8))
  #-cffi
  (ecase encoding
    ((:utf-8 :ascii :latin-1)
     (position 0 octets)))
  #+cffi
  (cffi:with-pointer-to-vector-data (ptr octets)
    (cffi::foreign-string-length ptr :encoding encoding)))

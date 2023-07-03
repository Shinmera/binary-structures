(in-package #:org.shirakumo.binary-structures)

(define-io-backend io-stream)

(define-io-dispatch :read stream (type storage)
  `(,(intern* 'read-io-stream- (lisp-type type)) storage))

(define-io-dispatch :write stream (type value storage)
  `(,(intern* 'write-io-stream- (lisp-type type)) value storage))

(define-io-dispatch :read pathname (type storage &key (if-does-not-exist :error))
  `(with-open-file (stream storage :if-does-not-exist if-does-not-exist
                                   :element-type '(unsigned-byte 8)
                                   :direction :input)
     (,(intern* 'read-io-stream- (lisp-type type)) stream)))

(define-io-dispatch :write pathname (type value storage &key (if-exists :error))
  `(with-open-file (stream storage :if-exists if-exists
                                   :element-type '(unsigned-byte 8)
                                   :direction :output)
     (,(intern* 'write-io-stream- (lisp-type type)) value stream)))

(defmethod read-defun ((backend io-stream) (type io-type))
  `(define-typed-function ,(intern* 'read- (type-of backend) '- (lisp-type type))
       ((stream stream))
       ,(lisp-type type)
     (handler-bind ((end-of-file (lambda (e) (declare (ignore e)) (error 'end-of-storage))))
       ,(read-form backend type))))

(defmethod write-defun ((backend io-stream) (type io-type))
  `(define-typed-function ,(intern* 'write- (type-of backend) '- (lisp-type type))
       ((value ,(lisp-type type)) (stream stream))
       T
     (handler-bind ((end-of-file (lambda (e) (declare (ignore e)) (error 'end-of-storage))))
       ,(write-form backend type 'value))))

(defmethod call-read-form ((backend io-stream) (type io-type))
  `(,(intern* 'read- (type-of backend) '- (lisp-type type)) stream))

(defmethod call-write-form ((backend io-stream) (type io-type) value-variable)
  `(,(intern* 'write- (type-of backend) '- (lisp-type type)) ,value-variable stream))

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
  (if (equalp '(unsigned-byte 8) (lisp-type (element-type type)))
      `(let ((array (make-array ,(if (unspecific-p (element-count type))
                                     (read-form backend (element-count type))
                                     (element-count type))
                                :element-type '(unsigned-byte 8))))
         (read-sequence array stream)
         array)
      (call-next-method)))

(defmethod write-form ((backend io-stream) (type io-vector) value-variable)
  (if (equalp '(unsigned-byte 8) (lisp-type (element-type type)))
      `(write-sequence ,value-variable stream)
      (call-next-method)))

(defmethod index-form ((backend io-stream))
  `(file-position stream))

(defmethod seek-form ((backend io-stream) offset)
  `(let ((diff (- ,offset ,(index-form backend))))
     (cond ((= 0 diff))
           ((< 8 diff)
            (let ((scratch (make-array diff :element-type '(unsigned-byte 8))))
              (read-sequence scratch stream)))
           ((< 0 diff)
            (loop repeat diff do (read-byte stream)))
           (T
            (file-position stream ,offset)))))

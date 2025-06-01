(in-package #:org.shirakumo.binary-structures)

(define-io-backend io-foreign-pointer (bounds-checked-io-backend))

(define-io-dispatch :read cffi:foreign-pointer (type storage size)
  `(progn
     (check-type size index)
     (,(intern* 'read-io-foreign-pointer- (lisp-type type)) storage size)))

(define-io-dispatch :write cffi:foreign-pointer (type value storage size)
  `(progn
     (check-type size index)
     (,(intern* 'write-io-foreign-pointer- (lisp-type type)) value storage size)))

(defmacro read-mem (pointer type &optional (octets `(cffi:foreign-type-size ,type)))
  `(prog1 ,(case type
             (:uint24
              `(logior (cffi:mem-ref ,pointer :uint16)
                       (ash (cffi:mem-ref ,pointer :uint8 16) 16)))
             (:int24
              `(logior (cffi:mem-ref ,pointer :uint16)
                       (ash (cffi:mem-ref ,pointer :int8 16) 16)))
             (T
              `(cffi:mem-ref ,pointer ,type)))
     (cffi:incf-pointer ,pointer ,octets)))

(defmacro write-mem (value pointer type &optional (octets `(cffi:foreign-type-size ,type)))
  `(prog1 ,(case type
             (:uint24
              `(setf (cffi:mem-ref ,pointer :uint16) (ldb (byte 16 0) ,value)
                     (cffi:mem-ref ,pointer :uint8 16) (ash ,value -16)))
             (:int24
              `(setf (cffi:mem-ref ,pointer :uint16) (ldb (byte 16 0) ,value)
                     (cffi:mem-ref ,pointer :int8 16) (ash ,value -16)))
             (T
              `(setf (cffi:mem-ref ,pointer ,type) ,value)))
     (cffi:incf-pointer ,pointer ,octets)))

(defmethod read-defun ((backend io-foreign-pointer) (type io-type) &optional name)
  `(define-typed-function ,(or name (intern* 'read- (type-of backend) '- (lisp-type type)))
       ((pointer cffi:foreign-pointer) (size index))
       (values ,(lisp-type type) cffi:foreign-pointer &optional)
     (declare (ignorable size))
     (let ((start pointer)
           (pointer pointer))
       (declare (type cffi:foreign-pointer start pointer))
       (declare (type index))
       (declare (ignorable start))
       (flet ((check-available-space (space)
                (when (< (the index (- size (the index (- (cffi:pointer-address pointer) (cffi:pointer-address start)))))
                         (the index space))
                  (error 'end-of-storage :index (cffi:pointer-address pointer) :end (the index (+ (cffi:pointer-address start) size)))))
              (available-space ()
                (the index (- size (the index (- (cffi:pointer-address pointer) (cffi:pointer-address start)))))))
         (declare (ignorable #'check-available-space #'available-space) (inline check-available-space available-space))
         (values ,(read-form backend type)
                 pointer)))))

(defmethod write-defun ((backend io-foreign-pointer) (type io-type) &optional name)
  `(define-typed-function ,(or name (intern* 'write- (type-of backend) '- (lisp-type type)))
       ((value ,(lisp-type type)) (pointer cffi:foreign-pointer) (size index))
       (values cffi:foreign-pointer &optional)
     (declare (ignorable size))
     (let ((start pointer)
           (pointer pointer))
       (declare (type cffi:foreign-pointer start pointer))
       (declare (type index))
       (declare (ignorable start))
       (flet ((check-available-space (space)
                (when (< (the index (- size (the index (- (cffi:pointer-address pointer) (cffi:pointer-address start)))))
                         (the index space))
                  (locally (declare (optimize (speed 0)))
                    (error 'end-of-storage :index (cffi:pointer-address pointer) :end (the index (+ (cffi:pointer-address start) size))))))
              (available-space ()
                (the index (- size (the index (- (cffi:pointer-address pointer) (cffi:pointer-address start)))))))
         (declare (ignorable #'check-available-space #'available-space) (inline check-available-space available-space))
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
       ,(find-symbol* 'keyword (if (signed-p type) '|| 'u) 'int
                      (* 8 (octet-size type)))
       ,(octet-size type)))

(defmethod write-form ((backend io-foreign-pointer) (type io-integer) value-variable)
  `(write-mem ,value-variable
       pointer
       ,(find-symbol* 'keyword (if (signed-p type) '|| 'u) 'int
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
  `(write-mem ,value-variable
       pointer
       ,(ecase (octet-size type)
          (2 :half-float)
          (4 :float)
          (8 :double)
          (16 :long-double))
       ,(octet-size type)))

(defmethod index-form ((backend io-foreign-pointer) &optional force-dynamic)
  (declare (ignore force-dynamic))
  `(- (cffi:pointer-address pointer)
      (cffi:pointer-address start)))

(defmethod seek-form ((backend io-foreign-pointer) offset)
  `(let ((relative-offset (+ relative-offset ,offset)))
     (if (<= relative-offset size)
         (setf pointer (cffi:inc-pointer start relative-offset))
         (error 'end-of-storage :index (cffi:pointer-address pointer) :end (+ (cffi:pointer-address start) size)))))

(in-package #:org.shirakumo.binary-structures)

(defmacro define-io-accessor (backend io-type)
  (let ((name (let ((*package* (find-package '#:org.shirakumo.binary-structures.types)))
                (intern* io-type '/ backend)))
        (type (io-type io-type)))
    `(progn
       (export ',name '#:org.shirakumo.binary-structures.types)
       ,(read-defun backend type name)
       ,(write-defun backend type `(setf ,name)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun try-compile (form)
    (let* ((*standard-output* (make-broadcast-stream))
           (*error-output* *standard-output*)
           (*debug-io* *standard-output*))
      (handler-case (not (nth-value 2 (compile NIL `(lambda () ,form))))
        ((or error warning) () NIL)))))

(defmacro define-all-io-accessors ()
  `(progn
     ,@(loop for backend in (list-io-backends)
             nconc (loop for name in (list-io-types)
                         for type = (io-type name)
                         when (and (not (typep type 'io-structure))
                                   (not (eql (find-package "KEYWORD") (symbol-package name)))
                                   (not (member name '(T)))
                                   ;; KLUDGE: this kinda sucks since we're essentially compiling
                                   ;;         everything twice, but we have no other way of checking
                                   ;;         whether the types are actually supported or not.
                                   (try-compile `(define-io-accessor ,backend ,name)))
                         collect `(define-io-accessor ,backend ,name)))))

(define-all-io-accessors)

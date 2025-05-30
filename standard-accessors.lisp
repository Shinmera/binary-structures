(in-package #:org.shirakumo.binary-structures)

(defmacro define-io-accessor (backend io-type)
  (let ((name (let ((*package* (find-package '#:org.shirakumo.binary-structures.types)))
                (intern* io-type '/ backend))))
    `(progn
       (export ',name '#:org.shirakumo.binary-structures.types)
       ,(read-defun backend io-type name)
       ,(write-defun backend io-type `(setf ,name)))))

(defmacro define-all-io-accessors (&environment env)
  `(progn
     ,@(loop for backend in (list-io-backends)
             nconc (loop for name in (list-io-types)
                         for type = (io-type name)
                         when (and (not (typep type 'io-structure))
                                   (not (eql (find-package "KEYWORD") (symbol-package name)))
                                   (not (member name '(T)))
                                   (ignore-errors (macroexpand `(define-io-accessor ,backend ,name) env)))
                         collect `(define-io-accessor ,backend ,name)))))

(define-all-io-accessors)

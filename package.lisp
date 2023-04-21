#|
 This file is a part of binary-structures
 (c) 2023 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage #:org.shirakumo.binary-structures.types
  (:use))

(defpackage #:org.shirakumo.binary-structures
  (:use #:cl)
  (:local-nicknames
   (#:sequences #:org.shirakumo.trivial-extensible-sequences))
  ;; toolkit.lisp
  (:export
   #:unspecific-p)
  ;; protocol.lisp
  (:export
   #:binary-structures-condition
   #:no-such-io-type
   #:designator
   #:end-of-storage
   #:index
   #:end
   #:unknown-value
   #:value
   #:accepted
   #:no-such-slot
   #:name
   #:struct
   #:io-backend
   #:offset
   #:io-type
   #:read-defun
   #:write-defun
   #:call-read-form
   #:call-write-form
   #:read-form
   #:write-form
   #:index-form
   #:seek-form
   #:lisp-type
   #:default-value
   #:octet-size
   #:initargs
   #:parse-io-type
   #:io-type
   #:define-io-type
   #:list-io-types
   #:list-io-backends
   #:define-io-backend
   #:define-io-dispatch
   #:remove-io-dispatch
   #:define-io-type-parser
   #:define-io-backend-function
   #:define-io-dispatch-function
   #:define-io-functions
   #:define-io-alias
   #:top-type
   #:numeric-type
   #:order
   #:io-integer
   #:signed-p
   #:io-float
   #:io-vector
   #:element-type
   #:element-count
   #:element-offset
   #:stride
   #:i
   #:io-string
   #:encoding
   #:io-case
   #:value-type
   #:cases
   #:io-value
   #:form
   #:io-typecase
   #:io-structure
   #:constructor
   #:slots
   #:name
   #:find-slots
   #:slot
   #:io-structure-slot
   #:io-structure-magic
   #:define-io-structure
   #:bounds-checked-io-backend)
  ;; standard-types.lisp
  (:export
   #:define-io-types)
  ;; types
  (:export
   #:io-stream
   #:io-octet-vector
   #:io-foreign-pointer)))

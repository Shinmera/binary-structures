#|
 This file is a part of binary-structures
 (c) 2023 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.binary-structures)

(defmacro define-io-types (type &body combinators)
  (let ((combs ()))
    (labels ((r (name args &rest alts)
               (if alts
                   (destructuring-bind (choices . alts) alts
                     (dolist (choice (if (listp choices) choices (list choices)))
                       (destructuring-bind (name-part . args-part) (if (listp choice) choice (list choice))
                         (apply #'r (concatenate 'string name (princ-to-string name-part)) (append args args-part) alts))))
                   (push (list* name args) combs))))
      (apply #'r "" () combinators))
    `(progn
       ,@(loop for (name . args) in (nreverse combs)
               collect `(export ',(intern name))
               collect `(setf (io-type ',(intern name)) (make-instance ',type ,@args))
               collect `(setf (io-type ,(intern name "KEYWORD")) (io-type ',(intern name)))))))

(define-io-type (top-type T))

(define-io-types io-integer
  ((s :signed-p T) (u :signed-p NIL))
  int
  ((8 :octet-size 1) (16 :octet-size 2) (32 :octet-size 4) (64 :octet-size 8) (128 :octet-size 16))
  ((|| :order :little-endian) (-be :order :big-endian)))

(define-io-types io-float
  float
  ((16 :octet-size 2) (32 :octet-size 4) (64 :octet-size 8) (128 :octet-size 16))
  ((|| :order :little-endian) (-be :order :big-endian)))

(define-io-types io-string
  ((utf8 :encoding :utf-8)
   (utf16 :encoding :utf-16)
   (utf32 :encoding :utf-32)
   (latin1 :encoding :latin-1))
  -string)

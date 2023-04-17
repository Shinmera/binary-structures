#|
 This file is a part of binary-structures
 (c) 2023 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(asdf:defsystem binary-structures
  :version "1.0.0"
  :license "zlib"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "A library for reading, writing, and representing structures from binary representations"
  :homepage "https://shinmera.github.io/binary-structures/"
  :bug-tracker "https://github.com/shinmera/binary-structures/issues"
  :source-control (:git "https://github.com/shinmera/binary-structures.git")
  :serial T
  :components ((:file "package")
               (:file "toolkit")
               (:file "protocol")
               (:file "standard-types")
               (:file "stream")
               (:file "octet-vector")
               (:file "foreign-pointer")
               (:file "documentation"))
  :depends-on (:cffi
               :babel
               :nibbles
               :trivial-extensible-sequences
               :documentation-utils))

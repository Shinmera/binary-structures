(asdf:defsystem binary-structures
  :version "1.0.0"
  :license "zlib"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :description "A library for reading, writing, and representing structures from binary representations"
  :homepage "https://shinmera.com/docs/binary-structures/"
  :bug-tracker "https://shinmera.com/project/binary-structures/issues"
  :source-control (:git "https://shinmera.com/project/binary-structures.git")
  :serial T
  :components ((:file "package")
               (:file "toolkit")
               (:file "protocol")
               (:file "standard-types")
               (:file "stream")
               (:file "octet-vector")
               (:file "foreign-pointer" :if-feature (:not :mezzano))
               (:file "standard-accessors")
               (:file "documentation"))
  :depends-on ((:feature (:not :mezzano) :cffi)
               :babel
               :nibbles
               :form-fiddle
               :trivial-extensible-sequences
               :documentation-utils))

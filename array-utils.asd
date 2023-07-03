(asdf:defsystem array-utils
  :name "Array-Utils"
  :version "1.2.0"
  :license "zlib"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :description "A few utilities for working with arrays."
  :homepage "https://Shinmera.github.io/array-utils/"
  :bug-tracker "https://github.com/Shinmera/array-utils/issues"
  :source-control (:git "https://github.com/Shinmera/array-utils.git")
  :serial T
  :components ((:file "utils"))
  :depends-on ()
  :in-order-to ((asdf:test-op (asdf:test-op :array-utils-test))))

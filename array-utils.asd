(asdf:defsystem array-utils
  :name "Array-Utils"
  :version "1.3.0"
  :license "zlib"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :description "A few utilities for working with arrays."
  :homepage "https://shinmera.com/docs/array-utils/"
  :bug-tracker "https://shinmera.com/project/array-utils/issues"
  :source-control (:git "https://shinmera.com/project/array-utils.git")
  :serial T
  :components ((:file "utils"))
  :depends-on ()
  :in-order-to ((asdf:test-op (asdf:test-op :array-utils-test))))

(asdf:defsystem array-utils-test
  :version "1.0.0"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :license "zlib"
  :description "Tests for the array-utils package"
  :components ((:file "tests"))
  :depends-on (:array-utils :parachute)
  :perform (asdf:test-op (op c) (uiop:symbol-call :parachute :test :array-utils-test)))

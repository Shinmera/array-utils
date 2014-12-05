#|
 This file is a part of Array-Utils
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(asdf:defsystem array-utils
  :name "Array-Utils"
  :version "0.1.14"
  :license "Artistic"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "A few utilities for working with arrays."
  :homepage "https://github.com/Shinmera/array-utils"
  :serial T
  :components ((:file "utils"))
  :depends-on ())

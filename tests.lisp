#|
This file is a part of array-utils
(c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(defpackage #:array-utils-test
  (:use #:cl)
  (:shadow #:run)
  (:nicknames #:org.shirakumo.array-utils.test)
  (:export #:array-utils #:run))

(in-package #:org.shirakumo.array-utils.test)

(defmethod asdf:perform ((op asdf:test-op) (sys (eql (asdf:find-system :array-utils-test))))
  (run 'array-utils))

;;;;; FRAMEWORK
(defvar *tests* (make-hash-table :test 'eql))
(defvar *results*)
(defvar *test-count*)

(defun test (name)
  (gethash name *tests*))

(defun (setf test) (func name)
  (setf (gethash name *tests*) func))

(defun call-with-test-unit (function)
  (let ((*results* (list :passed () :failed () :errored ()))
        (*test-count* 0))
    (funcall function)
    (list :passed (reverse (getf *results* :passed))
          :failed (reverse (getf *results* :failed))
          :errored (reverse (getf *results* :errored)))))

(defmacro with-test-unit (() &body body)
  `(call-with-test-unit (lambda () ,@body)))

(defmacro define-test (name () &body tests)
  `(setf (test ',name)
         (lambda ()
           (with-test-unit ()
             (format T "~&Running test ~a~%  " ',name)
             ,@tests))))

(defun run-test (name)
  (let ((test (test name)))
    (unless test
      (error "No test found with name ~s" name))
    (funcall test)))

(defun run (name)
  (destructuring-bind (&key passed failed errored) (run-test name)
    (format T "~&~
               ~%~4d passed~
               ~%~4d failed~
               ~%~4d errored"
            (length passed) (length failed) (length errored))
    (when failed
      (format T "~&~%The following forms failed:")
      (dolist (f failed)
        (format T "~& ~s" f)))
    (when errored
      (format T "~&~%The following forms signalled an unexpected error:")
      (dolist (e errored)
        (format T "~& ~s" e)))
    (format T "~&")))

(defun include-test (name)
  (destructuring-bind (&key passed failed errored) (run-test name)
    (dolist (p passed) (push p (getf *results* :passed)))
    (dolist (p failed) (push p (getf *results* :failed)))
    (dolist (p errored) (push p (getf *results* :errored)))
    *results*))

(defun count-test ()
  (incf *test-count*)
  (when (= 0 (mod *test-count* 25))
    (format T "~&  ")))

(defun pass (form)
  (count-test)
  (format T ".")
  (push form (getf *results* :passed)))

(defun fail (form)
  (count-test)
  (format T "x")
  (push form (getf *results* :failed)))

(defun err (form)
  (count-test)
  (format T "E")
  (push form (getf *results* :errored)))

(defmacro is (a b &optional (test 'equalp))
  (let ((form `(,test ,a ,b)))
    `(handler-case
         (if ,form
             (pass ',form)
             (fail ',form))
       (error (err)
         (declare (ignore err))
         (err ',form)))))

(defmacro signals (signal form)
  `(handler-case
       (progn
         ,form
         (fail ',form))
     (,signal (o)
       (declare (ignore o))
       (pass ',form))
     (error (err)
       (declare (ignore err))
       (err ',form))))


;;;;; TEST SUITE

(defun cp (thing)
  (etypecase thing
    (array (make-array (array-dimensions thing)
                       :initial-contents thing
                       :adjustable (adjustable-array-p thing)
                       :fill-pointer (fill-pointer thing)))))

(defvar *array* (make-array 4 :initial-contents #(0 1 2 3) :adjustable T :fill-pointer T))

(defun as (&rest params &key &allow-other-keys)
  (apply #'array-utils:array-shift (cp *array*) params))

(defun eas (&rest params)
  (apply #'array-utils:ensure-array-size (cp *array*) params))

(define-test ensure-array-size ()
  (is (eas 4) #(0 1 2 3))
  (is (eas 3) #(0 1 2))
  (is (eas 5) #(0 1 2 3 0)))

(define-test array-shift ()
  ;;; Check errors
  (signals error (array-utils:array-shift NIL))
  (signals error (as :n 0.0))
  (signals error (as :from -1))
  (signals error (as :to -1))
  (signals error (as :from 2 :to 1))
  (signals error (as :n 1 :contents #()))
  ;;; Positive shifting
  (is (as :n 0)
      *array*)
  (is (as :n 1 :adjust NIL)
      #(0 0 1 2))
  (is (as :n 1 :adjust T)
      #(0 0 1 2 3))
  (is (as :n 1 :adjust NIL :from 1)
      #(0 1 1 2))
  (is (as :n 1 :adjust T :from 1)
      #(0 1 1 2 3))
  (is (as :n 1 :adjust NIL :to 2)
      #(0 0 1 3))
  (is (as :n 1 :adjust T :to 2)
      #(0 0 1 3))
  (is (as :n 1 :adjust NIL :from 1 :to 3)
      #(0 1 1 2))
  (is (as :n 1 :adjust T :from 1 :to 3)
      #(0 1 1 2))
  (is (as :n 3 :adjust NIL :to 1)
      #(0 1 2 0))
  (is (as :n 5 :adjust NIL)
      #(0 1 2 3))
  (is (as :n 4 :adjust T :to 1)
      #(0 1 2 3 0))
  (is (as :n 4 :adjust T :from 1 :to 2)
      #(0 1 2 3 0 1))
  ;; With filling
  (is (as :n 1 :adjust NIL :fill NIL)
      #(NIL 0 1 2))
  (is (as :n 5 :adjust NIL :fill NIL)
      #(NIL NIL NIL NIL))
  (is (as :n 1 :adjust NIL :from 1 :fill NIL)
      #(0 NIL 1 2))
  ;; With contents
  (is (as :n 2 :adjust NIL :contents #(5 6))
      #(5 6 0 1))
  (is (as :n 2 :adjust NIL :contents #(5 6 7))
      #(5 6 0 1))
  ;; With contents and filling
  (is (as :n 2 :adjust NIL :fill NIL :contents #(5))
      #(5 NIL 0 1))
  (is (as :n 2 :adjust NIL :fill NIL :contents #(5 6))
      #(5 6 0 1))
  ;;; Negative shifting
  (is (as :n -1 :adjust NIL)
      #(1 2 3 3))
  (is (as :n -1 :adjust T)
      #(1 2 3))
  (is (as :n -1 :adjust NIL :from 2)
      #(0 2 3 3))
  (is (as :n -1 :adjust T :from 2)
      #(0 2 3))
  (is (as :n -1 :adjust NIL :to 3)
      #(1 2 2 3))
  (is (as :n -1 :adjust T :to 3)
      #(1 2 2 3))
  (is (as :n -1 :adjust NIL :from 2 :to 3)
      #(0 2 2 3))
  (is (as :n -1 :adjust T :from 2 :to 3)
      #(0 2 2 3))
  (is (as :n -3 :adjust NIL :from 3 :to 4)
      #(3 1 2 3))
  (is (as :n -5 :adjust NIL)
      #(0 1 2 3))
  (is (as :n -3 :adjust T :from 3 :to 4)
      #(3 1 2))
  ;; With filling
  (is (as :n -1 :adjust NIL :fill NIL)
      #(1 2 3 NIL))
  (is (as :n -5 :adjust NIL :fill NIL)
      #(NIL NIL NIL NIL))
  (is (as :n -1 :adjust NIL :to 3 :fill NIL)
      #(1 2 NIL 3))
  ;; With contents
  (is (as :n -2 :adjust NIL :contents #(5 6))
      #(2 3 5 6))
  (is (as :n -2 :adjust NIL :contents #(5 6 7))
      #(2 3 5 6))
  ;; With contents and filling
  (is (as :n -2 :adjust NIL :fill NIL :contents #(5))
      #(2 3 5 NIL))
  (is (as :n -2 :adjust NIL :fill NIL :contents #(5 6))
      #(2 3 5 6)))

(define-test vector-push-extend-front ()
  (let ((arr (cp *array*)))
    (is (array-utils:vector-push-extend-front 5 arr)
        5)
    (is arr
        #(5 0 1 2 3))))

(define-test vector-push-extend-position ()
  (let ((arr (cp *array*)))
    (is (array-utils:vector-push-extend-position 5 arr 1)
        5)
    (is arr
        #(0 5 1 2 3))))

(define-test vector-pop-front ()
  (let ((arr (cp *array*)))
    (is (array-utils:vector-pop-front arr)
        0)
    (is arr
        #(1 2 3)))
  (signals error (array-utils:vector-pop-front #())))

(define-test vector-pop-front* ()
  (let ((arr (cp *array*)))
    (is (array-utils:vector-pop-front* arr)
        0)
    (is arr
        #(3 1 2)))
  (signals error (array-utils:vector-pop-front #())))

(define-test vector-pop-position ()
  (let ((arr (cp *array*)))
    (is (array-utils:vector-pop-position arr 1)
        1)
    (is arr
        #(0 2 3)))
  (let ((arr (cp *array*)))
    (is (array-utils:vector-pop-position arr 3) 3)
    (is arr
        #(0 1 2)))
  (signals error (array-utils:vector-pop-position (cp *array*) -1))
  (signals error (array-utils:vector-pop-position (cp *array*) 5)))

(define-test vector-pop-position* ()
  (let ((arr (cp *array*)))
    (is (array-utils:vector-pop-position* arr 1)
        1)
    (is arr
        #(0 3 2)))
  (let ((arr (cp *array*)))
    (is (array-utils:vector-pop-position arr 3) 3)
    (is arr
        #(0 1 2)))
  (signals error (array-utils:vector-pop-position* (cp *array*) -1))
  (signals error (array-utils:vector-pop-position* (cp *array*) 5)))

(define-test vector-append ()
  (is (array-utils:vector-append (cp *array*) #(4 5 6))
      #(0 1 2 3 4 5 6))
  (is (array-utils:vector-append (cp *array*) '(4 5 6))
      #(0 1 2 3 4 5 6))
  (is (array-utils:vector-append #() #())
      #()))

(define-test array-utils ()
  (mapcar #'include-test
          '(ensure-array-size
            array-shift
            vector-push-extend-front
            vector-push-extend-position
            vector-pop-front
            vector-pop-position
            vector-append)))

(in-package #:cl-user)
(defpackage #:array-utils
  (:nicknames #:org.shirakumo.array-utils)
  (:use #:cl)
  (:export
   #:ensure-array-size
   #:array-shift
   #:vector-push-extend-front
   #:vector-push-extend-position
   #:vector-push-extend-new
   #:vector-pop-front
   #:vector-pop-front*
   #:vector-pop-position
   #:vector-pop-position*
   #:vector-pop-element
   #:vector-pop-element*
   #:vector-append
   #:slice
   #:slice*
   #:nslice))
(in-package #:org.shirakumo.array-utils)

(defun ensure-array-size (array new-space)
  (when (array-has-fill-pointer-p array)
    (unless (array-in-bounds-p array new-space)
      (adjust-array array new-space))
    (incf (fill-pointer array)
          (- new-space (fill-pointer array))))
  array)

(deftype positive-fixnum ()
  `(and fixnum (integer 0)))

(defun array-shift (array &key (n 1) (from 0) to (adjust T) (fill NIL f-p) (contents NIL c-p))
  "Shifts a subset of array elements in either direction for a specified amount.
Optionally also extends the array and fills empty space with a given element.

N        --- The amount to be moved. If positive, things are shifted to the right. If
             negative, things are shifted to the left.
FROM     --- The left point of the region to move, inclusive.
TO       --- The right point of the region to move, exclusive.
ADJUST   --- Whether to adjust the fill pointer and the array bounds. The array is only
             adjusted if N is positive and the range of TO+N would exceed the ARRAY length,
             or if N is negative and TO equals the length of the ARRAY
FILL     --- If provided, empty spaces created by the move will be filled with this element.
CONTENTS --- If provided, uses the contents to fill the new space. If |N| is greater than the
             length of this sequence, FILL is used to fill the rest of the space if it is
             provided. If not, an error is signalled. No matter whether N is negative or
             positive, the content is filled in from left to right in the order it is given."
  (declare (optimize speed))
  (check-type n fixnum)
  (check-type from positive-fixnum)
  (check-type to (or null positive-fixnum))
  (check-type array vector)
  (let* ((length (length array))
         (to (or to length))
         (amount (abs n))
         (direction (cond ((< n 0) :left)
                          ((< 0 n) :right)
                          (T :nowhere)))
         (content-length (length contents)))
    (declare (type positive-fixnum from to amount content-length)
             (type vector array))
    (assert (<= from to) () "FROM must be smaller or equal to TO.")
    (assert (or f-p (not c-p) (<= amount content-length)) (fill)
            "FILL is not provided and CONTENTS is smaller than the amount to shift.")
    (case direction
      (:right
       (when (and adjust (<= length (+ to amount)))
         (ensure-array-size array (+ to amount))
         (setf length (+ to amount)))
       (loop for cursor of-type positive-fixnum
             from (1- (min length (+ to amount)))
             downto (+ from amount)
             do (setf (aref array cursor)
                      (aref array (the positive-fixnum (- cursor amount)))))
       (when c-p
         (macrolet ((iterate (iteration)
                      `(loop for cursor of-type positive-fixnum
                             from from below (min (+ from amount) to)
                             for item ,iteration contents
                             do (setf (aref array cursor) item))))
           (etypecase contents
             (list (iterate in))
             (vector (iterate across)))))
       (when f-p
         (loop repeat (- amount content-length)
               for cursor of-type positive-fixnum
               from (+ from content-length) below (min length (+ to amount))
               do (setf (aref array cursor) fill))))
      (:left
       (when (< 0 (- to amount))
         (loop for cursor of-type positive-fixnum
               from (max 0 (- from amount))
               upto (1- (- to amount))
               do (setf (aref array cursor)
                        (aref array (the positive-fixnum (+ cursor amount))))))
       (when (and adjust (= to length))
         (ensure-array-size array (max from (- to amount)))
         (setf length (max from (- to amount))))
       (when c-p
         (macrolet ((iterate (iteration)
                      `(loop for cursor of-type positive-fixnum
                             from (max (- to amount) from) below to
                             for item ,iteration contents
                             do (setf (aref array cursor) item))))
           (etypecase contents
             (list (iterate in))
             (vector (iterate across)))))
       (when f-p
         (loop for cursor of-type positive-fixnum
               from (+ content-length (max (- to amount) from)) below to
               do (setf (aref array cursor) fill))))))
  array)

(defun vector-push-extend-position (element vector position)
  "Pushes the element into the specified position and shifts everything
to the right to make space. This is potentially very costly as all
elements after the given position need to be shifted as per ARRAY-SHIFT."
  (array-shift vector :n 1 :from position)
  (setf (aref vector position) element)
  (fill-pointer vector))

(defun vector-push-extend-front (element vector)
  "Pushes the element onto the front of the vector and extends if necessary.
This operation is very costly and takes O(n) time as each element needs to
be shifted as per ARRAY-SHIFT.

See VECTOR-PUSH-EXTEND-POSITION"
  (vector-push-extend-position element vector 0))

(defun vector-push-extend-new (element vector &key key test test-not)
  "Pushes the element onto the back of the vector and extends if necessary, if it is not already part of the vector.

If TEST is passed, it is used to compare the elements. Defaults to EQL
If TEST-NOT is passed, its complement is used to compare the element.
If KEY is passed, it is used to extract the element comparison key for
both ELEMENT and each element in VECTOR.

Returns the existing element in VECTOR or the new ELEMENT if it was
inserted at the end.

See CL:VECTOR-PUSH-EXTEND"
  (check-type vector vector)
  (cond ((or key test test-not)
         (let* ((key (or key #'identity))
                (test (or test (if test-not (complement test-not) #'eql)))
                (comp (funcall key element)))
           (loop for el across vector
                 do (when (funcall test (funcall key el) comp)
                      (return el))
                 finally (progn (vector-push-extend element vector)
                                (return element)))))
        (T
         (loop for el across vector
               do (when (eql el element)
                    (return el))
               finally (progn (vector-push-extend element vector)
                              (return element))))))

(defun vector-pop-position (vector position)
  "Pops the element at the given position of the vector and returns it.
This is potentially very costly as all elements after the given position
need to be shifted back as per ARRAY-SHIFT.

If the array has an element-type of T, the element moved beyond the fill
pointer is set to NIL to avoid a memory leak.

See VECTOR-POP-POSITION*"
  (if (eql T (array-element-type vector))
      (if (= (1- (length vector)) position)
          (prog1 (vector-pop vector)
            (setf (aref vector position) NIL))
          (prog1 (aref vector position)
            (array-shift vector :n -1 :from (1+ position) :fill NIL)))
      (if (= (1- (length vector)) position)
          (vector-pop vector)
          (prog1 (aref vector position)
            (array-shift vector :n -1 :from (1+ position))))))

(defun vector-pop-position* (vector position)
  "Pops the element at the given position of the vector and returns it.
This is faster than VECTOR-POP-POSITION, but does not preserve the order of elements
in the vector.

If the array has an element-type of T, the element moved beyond the fill
pointer is set to NIL to avoid a memory leak.

See VECTOR-POP-POSITION"
  (decf (fill-pointer vector))
  (if (eql T (array-element-type vector))
      (shiftf (aref vector position) (aref vector (length vector)) NIL)
      (shiftf (aref vector position) (aref vector (length vector)))))

(defun vector-pop-front (vector)
  "Pops the first element off the vector and returns it.
This operation is very costly and takes O(n) time as each element needs to
be shifted as per ARRAY-SHIFT.

See VECTOR-POP-FRONT*
See VECTOR-POP-POSITION"
  (vector-pop-position vector 0))

(defun vector-pop-front* (vector)
  "Pops the first element off the vector and returns it.
This is faster than VECTOR-POP-FRONT, but does not preserve the order of elements
in the vector.

See VECTOR-POP-FRONT
See VECTOR-POP-POSITION"
  (vector-pop-position* vector 0))

(defun vector-pop-element (vector element)
  "Pops the element from the vector and returns it if it was contained.
This is potentially very costly as all elements after the given position
need to be shifted back as per ARRAY-SHIFT.

If the array has an element-type of T, the element moved beyond the fill
pointer is set to NIL to avoid a memory leak.

See VECTOR-POP-POSITION
See VECTOR-POP-ELEMENT*"
  (let ((pos (position element vector)))
    (when pos
      (vector-pop-position vector pos))))

(defun vector-pop-element* (vector element)
  "Pops the element from the vector and returns it if it was contained.
This is faster than VECTOR-POP-ELEMENT, but does not preserve the order of elements
in the vector.

If the array has an element-type of T, the element moved beyond the fill
pointer is set to NIL to avoid a memory leak.

See VECTOR-POP-POSITION*
See VECTOR-POP-ELEMENT"
  (let ((pos (position element vector)))
    (when pos
      (vector-pop-position* vector pos))))

(defun vector-append (vector sequence &optional position)
  "Appends all elements of the sequence at position of the vector and returns it.
This is potentially very costly as all elements after the given position
need to be shifted back as per ARRAY-SHIFT."
  (let ((position (or position (length vector))))
    (array-shift vector :n (length sequence) :from position)
    (macrolet ((iterate (iteration)
                 `(loop for i from position
                        for item ,iteration sequence
                        do (setf (aref vector i) item))))
      (etypecase sequence
        (list (iterate in))
        (vector (iterate across))))
    vector))

(defmacro do-vector-types (value &body body)
  `(etypecase ,value
     (simple-vector
      ,@body)
     (simple-string
      ,@body)
     ((simple-array (unsigned-byte 8) (*))
      ,@body)
     (string
      ,@body)
     (vector
      ,@body)))

(defun slice (vector &optional start end step)
  "Creates a sub-vector of the given vector.

START --- If positive:
            the inclusive starting index of the slice
          If negative:
            the inclusive starting index of the slice from
            the end of the vector
          If NULL: same as zero
END   --- If positive:
            the exclusive ending index of the slice
          If negative:
            the exclusive ending index of the slice from
            the end of the vector.
          If NULL: same as the length of the vector
STEP  --- If above zero:
            the step size between elements of the slice
          If NULL: same as one

Note that this always creates a fresh copy of the vector,
preserving the element type and other properties.

See NSLICE
See SLICE*"
  (declare (optimize speed))
  (check-type vector vector)
  (check-type start (or fixnum null))
  (check-type end (or fixnum null))
  (check-type step (or (and fixnum (integer 1)) null))
  (let* ((start (etypecase start
                  (null 0)
                  (unsigned-byte start)
                  (signed-byte (+ (length vector) start))))
         (end (etypecase end
                (null (length vector))
                (unsigned-byte end)
                (signed-byte (+ (length vector) end))))
         (step (etypecase step
                 (null 1)
                 (unsigned-byte step)))
         (nsize (ceiling (- end start) step)))
    (declare (type positive-fixnum start end step))
    (do-vector-types vector
      (if (= 1 step)
          (subseq vector start end)
          (let ((array (make-array nsize
                                   :element-type (array-element-type vector)
                                   :adjustable (adjustable-array-p vector)
                                   :fill-pointer (array-has-fill-pointer-p vector))))
            (loop for i of-type positive-fixnum from start below end by step
                  for j of-type positive-fixnum from 0
                  do (setf (aref array j) (aref vector i)))
            array)))))

(defun slice* (vector &optional start end)
  "Creates a sub-vector of the given vector.

START --- If positive:
            the inclusive starting index of the slice
          If negative:
            the inclusive starting index of the slice from
            the end of the vector
          If NULL: same as zero
END   --- If positive:
            the exclusive ending index of the slice
          If negative:
            the exclusive ending index of the slice from
            the end of the vector.
          If NULL: same as the length of the vector

Note that this creates a displaced vector pointing to the
original vector. It thus shares the data with the original
vector, and does not support a STEP size other than 1.

See SLICE
See NSLICE"
  (declare (optimize speed))
  (check-type vector vector)
  (check-type start (or fixnum null))
  (check-type end (or fixnum null))
  (let* ((start (etypecase start
                  (null 0)
                  (unsigned-byte start)
                  (signed-byte (+ (length vector) start))))
         (end (etypecase end
                (null (length vector))
                (unsigned-byte end)
                (signed-byte (+ (length vector) end)))))
    (declare (type positive-fixnum start end))
    (make-array (- end start) :element-type (array-element-type vector)
                              :displaced-to vector
                              :displaced-index-offset start)))

(defun nslice (vector &optional start end step)
  "Modifies the vector to a sub-vector of itself.

START --- If positive:
            the inclusive starting index of the slice
          If negative:
            the inclusive starting index of the slice from
            the end of the vector
          If NULL: same as zero
END   --- If positive:
            the exclusive ending index of the slice
          If negative:
            the exclusive ending index of the slice from
            the end of the vector.
          If NULL: same as the length of the vector
STEP  --- If above zero:
            the step size between elements of the slice
          If NULL: same as one

Note that this always modifies the passed vector, and
adjusts the fill pointer, if one is present.

See NSLICE
See SLICE*"
  (declare (optimize speed))
  (check-type vector vector)
  (check-type start (or fixnum null))
  (check-type end (or fixnum null))
  (check-type step (or (and fixnum (integer 1)) null))
  (let* ((start (etypecase start
                  (null 0)
                  (unsigned-byte start)
                  (signed-byte (+ (length vector) start))))
         (end (etypecase end
                (null (length vector))
                (unsigned-byte end)
                (signed-byte (+ (length vector) end))))
         (step (etypecase step
                 (null 1)
                 (unsigned-byte step)
                 (signed-byte step)))
         (nsize (ceiling (- end start) step)))
    (declare (type positive-fixnum start end step))
    (do-vector-types vector
      (if (= 1 step)
          (replace vector vector :start1 0 :start2 start :end2 end)
          (loop for i of-type positive-fixnum from start by step
                for j of-type positive-fixnum from 0 below nsize
                do (setf (aref vector j) (aref vector i))))
      (when (array-has-fill-pointer-p vector)
        (setf (fill-pointer vector) nsize)))
    vector))

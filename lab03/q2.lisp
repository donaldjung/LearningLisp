(defstruct person
  name age)

(defun filter-ages (array age)
  (let ((new-array (make-array 0 :adjustable t :fill-pointer 0)))
    (dotimes (index (length array) new-array)
      (if (> (person-age (aref array index)) age)
          (progn
            (vector-push-extend (person-name (aref array index)) new-array)))
      )))

(defun array-group (array1 array2)
  (let ((new-array2 (make-array 0 :adjustable t :fill-pointer 0))
        (x 0)
        (y 0))
    (dotimes (index (max (length array1) (length array2)) new-array2)
      (if (< index (length array1))
          (setf x (aref array1 index))
          (setf x nil))
      (if (< index (length array2))
          (setf y (aref array2 index))
          (setf y nil))
      (vector-push-extend (vector x y) new-array2)
      )
    )
  )


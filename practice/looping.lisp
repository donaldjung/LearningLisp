(defun sum-digit (n)
  (let ((sum 0)
        (digit 0)
        (temp n))
    (dotimes (i (ceiling (log temp 10)) sum)
      (setf digit (mod temp 10))
      (setf sum (+ sum digit))
      (setf temp (floor temp 10))
      )
    )
  )


(defun get-sports (array1 array2 interval)
  (let ((array3 (make-array 0 :adjustable t :fill-pointer 0)))
    (dotimes (index (length array1))
      (if (equal (aref array2 index) interval)
          (vector-push-extend (aref array1 index) array3))
      )
    array3
    )
  )

(defun list-maker (n)
  (let ((newlist (list 0)))
    (dotimes (index n (cdr (reverse newlist)))
      (setf newlist (cons index newlist)))
    )
  )

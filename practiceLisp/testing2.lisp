(defun getList (mylist)
  (let ((result '()))
    (dolist (i mylist)
      (push i result))
    result
    ))

(defun discount-calculator (orgPrice)
  (cond
    ((< orgPrice 20) (* orgPrice 0.95))
    ((<= orgPrice 100) (* orgPrice 0.90))
    ((> orgPrice 100) (* orgPrice 0.85))
    )
  )

(defun array-stats (arr)
  (let ((min 1000)
        (max 0)
        (sum 0)
        (average 0))
    (dotimes (i (length arr))
      (if (< (aref arr i) min) (setf min (aref arr i)))
      (if (> (aref arr i) max) (setf max (aref arr i)))
      (setf sum (+ sum (aref arr i)))
      )
    (setf average (/ sum (length arr)))
    (list min max sum average)
    )
  )

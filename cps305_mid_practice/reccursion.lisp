(defun factorial (n)
  (if (= n 0) 1
      (* n (factorial (- n 1))))
  )
()
(defun factorial-tail (n &optional (total 1))
  (if (= n 0) total
      (factorial-tail (- n 1) (* total n))))

(defun sumlist (list1)
  (if (null list1) 0
      (+ (first list1) (sumlist (rest list1)))))

(defun sumlist-tail (list1 &optional (sum 0))
  (if (null list1) sum
      (sumlist-tail (rest list1) (+ sum (first list1)))))

(defun dosum (n)
  (do ((i 0 (1+ i))
       (sum 0))
      ((= i n) sum)
    (setf sum (+ i sum))
    ))

(defun merge (list1 list2)
  (let ((i1 (first list1))
        (i2 (first list2))
        (res ()))
    (if (< i1 i2)
        (progn
          (cons res i1)
          (setf list1 (rest list1))))
    )
  

  )

(defun fact (x)
  (if (= x 1) 1
      (* x (fact (- x 1))))
  )

(defun fact2 (x &optional (sum 1))
  (if (= x 1) sum
      (fact2 (- x 1) (* x sum)))

  )

(defun is-palindrome? (list1)
  (let ((mid (floor (length list1) 2)))
    (equal (subseq list1 0 mid) (reverse (subseq list1 (- (length list1) mid))))
    ))

(defun print-squares-of-3s (x)
  (do ((num 1 (* num 3)))
      ((> (* num num) x) num)
    (print (* num num))
      ))

(defun discount-calculator (x)
  (cond
    ((<= x 0) "Invalid Price")
    ((< x 20) (* x 0.95))
    ((> x 100) (* x 0.85))
    ((>= x 20) (* x 0.9))
    ))

(defun array-stats (arr)
  (let ((min 0)
        (max 0)
        (sum 0))
    (dotimes (i (length arr))
      (cond
        ((> min (aref arr i)) (setf min (aref arr i)))
        ((< max (aref arr i)) (setf max (aref arr i)))
        )
      (setf sum (+ sum (aref arr i)))
      )
    (list min max (/ sum (length arr)) sum)
    )
  )

(defun generate-list (start end)
  (cond
    ((> start end) ())
    (t (cons start (generate-list (+ start 1) end))))

  )

(defun list-slice (list1 start end)
  (cond
    ((= start end) ())
    (t (cons (nth start list1) (list-slice list1 (+ start 1) end)))
    )
  )

(defun remove-int-duplicates-tail (list1 &optional (acc ()))
  (cond
    ((null list1) (reverse acc))
    ((member (first list1) acc) (remove-int-duplicates-tail (rest list1) acc))
    (t (remove-int-duplicates-tail (rest list1) (cons (first list1) acc)))
    ))

(defun remove-int-duplicates (list1)
  (cond
    ((null list1) ())
    ((member (first list1) (rest list1)) (remove-int-duplicates (rest list1)))
    (t (cons (first list1) (remove-int-duplicates (rest list1))))
    ))

(defun general-comparator (x y comp)
  (cond
    ((= x y) "They are equal")
    ((funcall comp x y) "The first is greater than the second")
    ((funcall comp y x) "The second is greater than the first")
    )
  )

(defun only-evens (list1)
  (cond
    ((null list1) ())
    ((oddp (first list1)) (only-evens (rest list1)))
    (t (cons (first list1) (only-evens (rest list1))))

    )
    )
